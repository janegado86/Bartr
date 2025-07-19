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
(define-constant err-auction-ended (err u109))
(define-constant err-auction-not-ended (err u110))
(define-constant err-bid-too-low (err u111))
(define-constant err-no-bids (err u112))
(define-constant err-not-auction-owner (err u113))
(define-constant err-auction-not-active (err u114))

(define-data-var next-item-id uint u1)
(define-data-var next-auction-id uint u1)
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

(define-map auctions uint {
    id: uint,
    item-id: uint,
    seller: principal,
    starting-bid: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20),
    reserve-met: bool,
    reserve-price: uint
})

(define-map auction-bids uint (list 100 {bidder: principal, amount: uint, block-height: uint}))
(define-map user-auctions principal (list 50 uint))
(define-map auction-history uint (list 100 {event: (string-ascii 50), user: principal, amount: uint, block: uint}))

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

(define-public (create-auction (item-id uint) (starting-bid uint) (duration-blocks uint) (reserve-price uint))
    (let ((caller tx-sender)
          (auction-id (var-get next-auction-id))
          (item (unwrap! (map-get? items item-id) err-not-found)))
        (asserts! (is-some (map-get? users caller)) err-unauthorized)
        (asserts! (is-eq (get owner item) caller) err-unauthorized)
        (asserts! (get is-available item) err-not-found)
        (asserts! (> starting-bid u0) err-invalid-input)
        (asserts! (> duration-blocks u0) err-invalid-input)
        (asserts! (>= reserve-price starting-bid) err-invalid-input)
        
        (map-set auctions auction-id {
            id: auction-id,
            item-id: item-id,
            seller: caller,
            starting-bid: starting-bid,
            current-bid: starting-bid,
            highest-bidder: none,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration-blocks),
            status: "active",
            reserve-met: (<= reserve-price starting-bid),
            reserve-price: reserve-price
        })
        
        (map-set items item-id (merge item { is-available: false }))
        
        (let ((user-auctions-list (default-to (list) (map-get? user-auctions caller))))
            (map-set user-auctions caller 
                     (unwrap! (as-max-len? (append user-auctions-list auction-id) u50) err-invalid-input)))
        
        (let ((history-list (list {event: "auction-created", user: caller, amount: starting-bid, block: stacks-block-height})))
            (map-set auction-history auction-id history-list))
        
        (var-set next-auction-id (+ auction-id u1))
        (ok auction-id)))

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-some (map-get? users caller)) err-unauthorized)
        (asserts! (is-eq (get status auction) "active") err-auction-not-active)
        (asserts! (< stacks-block-height (get end-block auction)) err-auction-ended)
        (asserts! (not (is-eq (get seller auction) caller)) err-self-trade)
        (asserts! (> bid-amount (get current-bid auction)) err-bid-too-low)
        
        (let ((current-bids (default-to (list) (map-get? auction-bids auction-id)))
              (new-bid {bidder: caller, amount: bid-amount, block-height: stacks-block-height}))
            (map-set auction-bids auction-id 
                     (unwrap! (as-max-len? (append current-bids new-bid) u100) err-invalid-input)))
        
        (let ((reserve-met (>= bid-amount (get reserve-price auction))))
            (map-set auctions auction-id (merge auction {
                current-bid: bid-amount,
                highest-bidder: (some caller),
                reserve-met: reserve-met
            })))
        
        (let ((current-history (default-to (list) (map-get? auction-history auction-id)))
              (new-event {event: "bid-placed", user: caller, amount: bid-amount, block: stacks-block-height}))
            (map-set auction-history auction-id 
                     (unwrap! (as-max-len? (append current-history new-event) u100) err-invalid-input)))
        
        (ok true)))

(define-public (end-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) err-not-found))
          (caller tx-sender))
        (asserts! (or (is-eq (get seller auction) caller) (>= stacks-block-height (get end-block auction))) err-auction-not-ended)
        (asserts! (is-eq (get status auction) "active") err-auction-not-active)
        
        (if (and (is-some (get highest-bidder auction)) (get reserve-met auction))
            (begin
                (let ((winner (unwrap-panic (get highest-bidder auction)))
                      (item (unwrap! (map-get? items (get item-id auction)) err-not-found)))
                    (map-set items (get item-id auction) (merge item { owner: winner }))
                    (map-set auctions auction-id (merge auction { status: "completed" }))
                    
                    (try! (update-user-trade-count (get seller auction)))
                    (try! (update-user-trade-count winner))
                    
                    (let ((current-history (default-to (list) (map-get? auction-history auction-id)))
                          (end-event {event: "auction-won", user: winner, amount: (get current-bid auction), block: stacks-block-height}))
                        (map-set auction-history auction-id 
                                 (unwrap! (as-max-len? (append current-history end-event) u100) err-invalid-input)))
                    (ok true)))
            (begin
                (let ((item (unwrap! (map-get? items (get item-id auction)) err-not-found)))
                    (map-set items (get item-id auction) (merge item { is-available: true }))
                    (map-set auctions auction-id (merge auction { status: "failed" }))
                    
                    (let ((current-history (default-to (list) (map-get? auction-history auction-id)))
                          (fail-event {event: "auction-failed", user: (get seller auction), amount: u0, block: stacks-block-height}))
                        (map-set auction-history auction-id 
                                 (unwrap! (as-max-len? (append current-history fail-event) u100) err-invalid-input)))
                    (ok false))))))

(define-public (cancel-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-eq (get seller auction) caller) err-not-auction-owner)
        (asserts! (is-eq (get status auction) "active") err-auction-not-active)
        (asserts! (is-none (get highest-bidder auction)) err-invalid-input)
        
        (let ((item (unwrap! (map-get? items (get item-id auction)) err-not-found)))
            (map-set items (get item-id auction) (merge item { is-available: true }))
            (map-set auctions auction-id (merge auction { status: "cancelled" }))
            
            (let ((current-history (default-to (list) (map-get? auction-history auction-id)))
                  (cancel-event {event: "auction-cancelled", user: caller, amount: u0, block: stacks-block-height}))
                (map-set auction-history auction-id 
                         (unwrap! (as-max-len? (append current-history cancel-event) u100) err-invalid-input)))
            (ok true))))

(define-public (extend-auction (auction-id uint) (additional-blocks uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-eq (get seller auction) caller) err-not-auction-owner)
        (asserts! (is-eq (get status auction) "active") err-auction-not-active)
        (asserts! (> additional-blocks u0) err-invalid-input)
        (asserts! (< stacks-block-height (get end-block auction)) err-auction-ended)
        
        (map-set auctions auction-id (merge auction { 
            end-block: (+ (get end-block auction) additional-blocks) 
        }))
        
        (let ((current-history (default-to (list) (map-get? auction-history auction-id)))
              (extend-event {event: "auction-extended", user: caller, amount: additional-blocks, block: stacks-block-height}))
            (map-set auction-history auction-id 
                     (unwrap! (as-max-len? (append current-history extend-event) u100) err-invalid-input)))
        (ok true)))

(define-read-only (get-auction (auction-id uint))
    (map-get? auctions auction-id))

(define-read-only (get-auction-bids (auction-id uint))
    (map-get? auction-bids auction-id))

(define-read-only (get-auction-history (auction-id uint))
    (map-get? auction-history auction-id))

(define-read-only (get-user-auctions (user principal))
    (map-get? user-auctions user))

(define-read-only (get-active-auctions)
    (ok (var-get next-auction-id)))

(define-read-only (get-auction-stats)
    (ok {
        total-auctions: (var-get next-auction-id),
        current-block: stacks-block-height
    }))

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
