(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-trade-not-pending (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-insufficient-balance (err u106))
(define-constant err-trade-expired (err u107))
(define-constant err-self-trade (err u108))

(define-data-var next-item-id uint u1)
(define-data-var next-trade-id uint u1)
(define-data-var platform-fee uint u100)
(define-data-var trade-expiry-blocks uint u1440)

(define-map users principal {
    username: (string-ascii 50),
    reputation: uint,
    total-trades: uint,
    active-since: uint,
    is-verified: bool
})

(define-map items uint {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 50),
    condition: (string-ascii 20),
    value-estimate: uint,
    is-available: bool,
    created-at: uint,
    location: (string-ascii 100)
})

(define-map trades uint {
    id: uint,
    initiator: principal,
    responder: principal,
    initiator-item: uint,
    responder-item: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint),
    initiator-rating: (optional uint),
    responder-rating: (optional uint)
})

(define-map user-items principal (list 100 uint))
(define-map item-watchers uint (list 50 principal))
(define-map user-trades principal (list 50 uint))

(define-public (register-user (username (string-ascii 50)))
    (let ((caller tx-sender))
        (asserts! (is-none (map-get? users caller)) err-already-exists)
        (asserts! (> (len username) u0) err-invalid-input)
        (map-set users caller {
            username: username,
            reputation: u100,
            total-trades: u0,
            active-since: stacks-block-height,
            is-verified: false
        })
        (ok true)))

(define-public (create-item (title (string-ascii 100)) (description (string-ascii 500)) 
                           (category (string-ascii 50)) (condition (string-ascii 20))
                           (value-estimate uint) (location (string-ascii 100)))
    (let ((caller tx-sender)
          (item-id (var-get next-item-id)))
        (asserts! (is-some (map-get? users caller)) err-unauthorized)
        (asserts! (and (> (len title) u0) (> (len description) u0)) err-invalid-input)
        (asserts! (> value-estimate u0) err-invalid-input)
        
        (map-set items item-id {
            owner: caller,
            title: title,
            description: description,
            category: category,
            condition: condition,
            value-estimate: value-estimate,
            is-available: true,
            created-at: stacks-block-height,
            location: location
        })
        
        (let ((current-items (default-to (list) (map-get? user-items caller))))
            (map-set user-items caller (unwrap! (as-max-len? (append current-items item-id) u100) err-invalid-input)))
        
        (var-set next-item-id (+ item-id u1))
        (ok item-id)))

(define-public (update-item-availability (item-id uint) (available bool))
    (let ((item (unwrap! (map-get? items item-id) err-not-found)))
        (asserts! (is-eq (get owner item) tx-sender) err-unauthorized)
        (map-set items item-id (merge item { is-available: available }))
        (ok true)))

(define-public (propose-trade (initiator-item-id uint) (responder-item-id uint))
    (let ((caller tx-sender)
          (trade-id (var-get next-trade-id))
          (initiator-item (unwrap! (map-get? items initiator-item-id) err-not-found))
          (responder-item (unwrap! (map-get? items responder-item-id) err-not-found)))
        
        (asserts! (is-some (map-get? users caller)) err-unauthorized)
        (asserts! (is-eq (get owner initiator-item) caller) err-unauthorized)
        (asserts! (not (is-eq (get owner responder-item) caller)) err-self-trade)
        (asserts! (get is-available initiator-item) err-not-found)
        (asserts! (get is-available responder-item) err-not-found)
        
        (map-set trades trade-id {
            id: trade-id,
            initiator: caller,
            responder: (get owner responder-item),
            initiator-item: initiator-item-id,
            responder-item: responder-item-id,
            status: "pending",
            created-at: stacks-block-height,
            completed-at: none,
            initiator-rating: none,
            responder-rating: none
        })
        
        (let ((initiator-trades (default-to (list) (map-get? user-trades caller)))
              (responder-trades (default-to (list) (map-get? user-trades (get owner responder-item)))))
            (map-set user-trades caller (unwrap! (as-max-len? (append initiator-trades trade-id) u50) err-invalid-input))
            (map-set user-trades (get owner responder-item) 
                     (unwrap! (as-max-len? (append responder-trades trade-id) u50) err-invalid-input)))
        
        (var-set next-trade-id (+ trade-id u1))
        (ok trade-id)))

(define-public (accept-trade (trade-id uint))
    (let ((trade (unwrap! (map-get? trades trade-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-eq (get responder trade) caller) err-unauthorized)
        (asserts! (is-eq (get status trade) "pending") err-trade-not-pending)
        (asserts! (< (- stacks-block-height (get created-at trade)) (var-get trade-expiry-blocks)) err-trade-expired)
        
        (map-set trades trade-id (merge trade { status: "accepted" }))
        (ok true)))

(define-public (complete-trade (trade-id uint))
    (let ((trade (unwrap! (map-get? trades trade-id) err-not-found))
          (caller tx-sender))
        (asserts! (or (is-eq (get initiator trade) caller) (is-eq (get responder trade) caller)) err-unauthorized)
        (asserts! (is-eq (get status trade) "accepted") err-trade-not-pending)
        
        (let ((initiator-item (unwrap! (map-get? items (get initiator-item trade)) err-not-found))
              (responder-item (unwrap! (map-get? items (get responder-item trade)) err-not-found)))
            
            (map-set items (get initiator-item trade) 
                     (merge initiator-item { owner: (get responder trade), is-available: false }))
            (map-set items (get responder-item trade) 
                     (merge responder-item { owner: (get initiator trade), is-available: false }))
            
            (map-set trades trade-id (merge trade { 
                status: "completed",
                completed-at: (some stacks-block-height)
            }))
            
            (try! (update-user-trade-count (get initiator trade)))
            (try! (update-user-trade-count (get responder trade)))
            
            (ok true))))
(define-public (reject-trade (trade-id uint))
    (let ((trade (unwrap! (map-get? trades trade-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-eq (get responder trade) caller) err-unauthorized)
        (asserts! (is-eq (get status trade) "pending") err-trade-not-pending)
        
        (map-set trades trade-id (merge trade { status: "rejected" }))
        (ok true)))

(define-public (rate-trade (trade-id uint) (rating uint))
    (let ((trade (unwrap! (map-get? trades trade-id) err-not-found))
          (caller tx-sender))
        (asserts! (or (is-eq (get initiator trade) caller) (is-eq (get responder trade) caller)) err-unauthorized)
        (asserts! (is-eq (get status trade) "completed") err-trade-not-pending)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-input)
        
        (if (is-eq (get initiator trade) caller)
            (begin
                (asserts! (is-none (get initiator-rating trade)) err-already-exists)
                (map-set trades trade-id (merge trade { initiator-rating: (some rating) }))
                (try! (update-user-reputation (get responder trade) rating)))
            (begin
                (asserts! (is-none (get responder-rating trade)) err-already-exists)
                (map-set trades trade-id (merge trade { responder-rating: (some rating) }))
                (try! (update-user-reputation (get initiator trade) rating))))
        (ok true)))
(define-public (watch-item (item-id uint))
    (let ((item (unwrap! (map-get? items item-id) err-not-found))
          (caller tx-sender)
          (current-watchers (default-to (list) (map-get? item-watchers item-id))))
        (asserts! (is-some (map-get? users caller)) err-unauthorized)
        (asserts! (is-none (index-of current-watchers caller)) err-already-exists)
        (map-set item-watchers item-id 
                 (unwrap! (as-max-len? (append current-watchers caller) u50) err-invalid-input))
        (ok true)))

(define-private (update-user-trade-count (user principal))
    (let ((user-data (unwrap! (map-get? users user) err-not-found)))
        (map-set users user (merge user-data { 
            total-trades: (+ (get total-trades user-data) u1) 
        }))
        (ok true)))

(define-private (update-user-reputation (user principal) (rating uint))
    (let ((user-data (unwrap! (map-get? users user) err-not-found))
          (current-rep (get reputation user-data))
          (trade-count (get total-trades user-data)))
        (if (> trade-count u0)
            (let ((new-rep (/ (+ (* current-rep trade-count) (* rating u20)) (+ trade-count u1))))
                (map-set users user (merge user-data { reputation: new-rep }))
                (ok true))
            (ok true))))

(define-read-only (get-user (user principal))
    (map-get? users user))

(define-read-only (get-item (item-id uint))
    (map-get? items item-id))

(define-read-only (get-trade (trade-id uint))
    (map-get? trades trade-id))

(define-read-only (get-user-items (user principal))
    (map-get? user-items user))

(define-read-only (get-user-trades (user principal))
    (map-get? user-trades user))

(define-read-only (get-item-watchers (item-id uint))
    (map-get? item-watchers item-id))

(define-read-only (search-items-by-category (category (string-ascii 50)))
    (ok category))

(define-read-only (get-platform-stats)
    (ok {
        total-items: (var-get next-item-id),
        total-trades: (var-get next-trade-id),
        platform-fee: (var-get platform-fee),
        current-block: stacks-block-height
    }))
