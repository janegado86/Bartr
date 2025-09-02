;; Reputation-Based Escrow Service Contract
;; Secure trading with automated dispute resolution for Bartr

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u500))
(define-constant err-not-found (err u501))
(define-constant err-unauthorized (err u502))
(define-constant err-invalid-input (err u503))
(define-constant err-escrow-exists (err u504))
(define-constant err-invalid-status (err u505))
(define-constant err-insufficient-funds (err u506))
(define-constant err-escrow-expired (err u507))
(define-constant err-dispute-window-closed (err u508))
(define-constant err-already-voted (err u509))
(define-constant err-not-mediator (err u510))

;; Escrow status constants
(define-constant ESCROW-PENDING u1)
(define-constant ESCROW-FUNDED u2)
(define-constant ESCROW-COMPLETED u3)
(define-constant ESCROW-DISPUTED u4)
(define-constant ESCROW-RESOLVED u5)
(define-constant ESCROW-CANCELLED u6)

;; Dispute resolution constants
(define-constant DISPUTE-WINDOW-BLOCKS u1440) ;; 24 hours
(define-constant MEDIATION-VOTING-PERIOD u720) ;; 12 hours
(define-constant MIN-MEDIATOR-REPUTATION u150)
(define-constant MEDIATOR-CONSENSUS-THRESHOLD u60) ;; 60% agreement

(define-data-var next-escrow-id uint u1)
(define-data-var escrow-fee-rate uint u25) ;; 0.25%
(define-data-var total-escrows-created uint u0)
(define-data-var dispute-resolution-enabled bool true)

;; Map to store escrow details
(define-map EscrowContracts
    uint
    {
        escrow-id: uint,
        initiator: principal,
        responder: principal,
        initiator-item: uint,
        responder-item: uint,
        escrow-amount: uint,
        fee-amount: uint,
        status: uint,
        created-at: uint,
        funded-at: (optional uint),
        expiry-block: uint,
        completion-deadline: uint,
        initiator-confirmed: bool,
        responder-confirmed: bool,
        auto-release-enabled: bool
    }
)

;; Map to store dispute information
(define-map Disputes
    uint
    {
        escrow-id: uint,
        plaintiff: principal,
        defendant: principal,
        dispute-reason: (string-ascii 200),
        evidence-initiator: (string-ascii 500),
        evidence-responder: (string-ascii 500),
        created-at: uint,
        resolution-deadline: uint,
        mediator-votes: uint,
        votes-for-initiator: uint,
        votes-for-responder: uint,
        final-decision: (optional principal),
        resolved-at: (optional uint),
        resolution-method: (string-ascii 50)
    }
)

;; Map to store mediator information
(define-map Mediators
    principal
    {
        reputation-score: uint,
        total-mediations: uint,
        successful-resolutions: uint,
        active-disputes: uint,
        specialization: (string-ascii 100),
        is-active: bool,
        joined-at: uint,
        last-activity: uint
    }
)

;; Map to store mediation votes
(define-map MediationVotes
    {dispute-id: uint, mediator: principal}
    {
        vote: principal,
        reasoning: (string-ascii 300),
        confidence-level: uint,
        voted-at: uint
    }
)

;; Map to store escrow participation history
(define-map UserEscrowHistory
    principal
    {
        total-escrows: uint,
        successful-escrows: uint,
        disputed-escrows: uint,
        total-fees-paid: uint,
        escrow-reputation: uint,
        reliability-score: uint
    }
)

;; Map to store reputation-based settings
(define-map ReputationSettings
    uint
    {
        min-reputation-threshold: uint,
        auto-release-threshold: uint,
        dispute-risk-multiplier: uint,
        fee-reduction-threshold: uint
    }
)

;; Create a new escrow contract
(define-public (create-escrow 
    (responder principal) 
    (initiator-item uint) 
    (responder-item uint) 
    (escrow-amount uint)
    (completion-deadline uint)
    (auto-release bool))
    (let ((escrow-id (var-get next-escrow-id))
          (caller tx-sender))
        (asserts! (> escrow-amount u0) err-invalid-input)
        (asserts! (> completion-deadline stacks-block-height) err-invalid-input)
        (asserts! (not (is-eq caller responder)) err-invalid-input)
        (asserts! (is-none (get-active-escrow-between caller responder)) err-escrow-exists)
        
        (let ((fee-amount (calculate-escrow-fee caller escrow-amount))
              (reputation-settings (get-reputation-settings (get-user-reputation caller))))
            
            (map-set EscrowContracts escrow-id {
                escrow-id: escrow-id,
                initiator: caller,
                responder: responder,
                initiator-item: initiator-item,
                responder-item: responder-item,
                escrow-amount: escrow-amount,
                fee-amount: fee-amount,
                status: ESCROW-PENDING,
                created-at: stacks-block-height,
                funded-at: none,
                expiry-block: (+ stacks-block-height u14400), ;; 10 days
                completion-deadline: completion-deadline,
                initiator-confirmed: false,
                responder-confirmed: false,
                auto-release-enabled: auto-release
            })
            
            (unwrap-panic (update-user-escrow-history caller u1 u0 u0 fee-amount))
            (var-set next-escrow-id (+ escrow-id u1))
            (var-set total-escrows-created (+ (var-get total-escrows-created) u1))
            (ok escrow-id))))

;; Fund an escrow contract
(define-public (fund-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found))
          (caller tx-sender))
        (asserts! (is-eq caller (get initiator escrow)) err-unauthorized)
        (asserts! (is-eq (get status escrow) ESCROW-PENDING) err-invalid-status)
        (asserts! (< stacks-block-height (get expiry-block escrow)) err-escrow-expired)
        
        (map-set EscrowContracts escrow-id (merge escrow {
            status: ESCROW-FUNDED,
            funded-at: (some stacks-block-height)
        }))
        (ok true)))

;; Confirm completion by either party
(define-public (confirm-completion (escrow-id uint))
    (let ((escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found))
          (caller tx-sender))
        (asserts! (or (is-eq caller (get initiator escrow)) (is-eq caller (get responder escrow))) err-unauthorized)
        (asserts! (is-eq (get status escrow) ESCROW-FUNDED) err-invalid-status)
        
        (let ((updated-escrow 
                (if (is-eq caller (get initiator escrow))
                    (merge escrow { initiator-confirmed: true })
                    (merge escrow { responder-confirmed: true }))))
            (map-set EscrowContracts escrow-id updated-escrow)
            
            (if (and (get initiator-confirmed updated-escrow) (get responder-confirmed updated-escrow))
                (complete-escrow escrow-id)
                (ok true)))))

;; Complete escrow when both parties confirm or auto-release triggers
(define-private (complete-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found)))
        (map-set EscrowContracts escrow-id (merge escrow {
            status: ESCROW-COMPLETED
        }))
        
        (unwrap-panic (update-user-escrow-history (get initiator escrow) u0 u1 u0 u0))
        (unwrap-panic (update-user-escrow-history (get responder escrow) u0 u1 u0 u0))
        (ok true)))

;; Initiate a dispute
(define-public (initiate-dispute (escrow-id uint) (reason (string-ascii 200)) (evidence (string-ascii 500)))
    (let ((escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found))
          (caller tx-sender))
        (asserts! (var-get dispute-resolution-enabled) err-invalid-input)
        (asserts! (or (is-eq caller (get initiator escrow)) (is-eq caller (get responder escrow))) err-unauthorized)
        (asserts! (is-eq (get status escrow) ESCROW-FUNDED) err-invalid-status)
        (asserts! (< (- stacks-block-height (unwrap-panic (get funded-at escrow))) DISPUTE-WINDOW-BLOCKS) err-dispute-window-closed)
        (asserts! (> (len reason) u0) err-invalid-input)
        
        (let ((defendant (if (is-eq caller (get initiator escrow)) (get responder escrow) (get initiator escrow))))
            (map-set Disputes escrow-id {
                escrow-id: escrow-id,
                plaintiff: caller,
                defendant: defendant,
                dispute-reason: reason,
                evidence-initiator: (if (is-eq caller (get initiator escrow)) evidence ""),
                evidence-responder: (if (is-eq caller (get responder escrow)) evidence ""),
                created-at: stacks-block-height,
                resolution-deadline: (+ stacks-block-height MEDIATION-VOTING-PERIOD),
                mediator-votes: u0,
                votes-for-initiator: u0,
                votes-for-responder: u0,
                final-decision: none,
                resolved-at: none,
                resolution-method: "mediation"
            })
            
            (map-set EscrowContracts escrow-id (merge escrow { status: ESCROW-DISPUTED }))
            (unwrap-panic (update-user-escrow-history caller u0 u0 u1 u0))
            (ok true))))

;; Submit evidence for dispute
(define-public (submit-dispute-evidence (escrow-id uint) (evidence (string-ascii 500)))
    (let ((dispute (unwrap! (map-get? Disputes escrow-id) err-not-found))
          (caller tx-sender))
        (asserts! (or (is-eq caller (get plaintiff dispute)) (is-eq caller (get defendant dispute))) err-unauthorized)
        (asserts! (< stacks-block-height (get resolution-deadline dispute)) err-escrow-expired)
        (asserts! (> (len evidence) u0) err-invalid-input)
        
        (if (is-eq caller (get plaintiff dispute))
            (map-set Disputes escrow-id (merge dispute { evidence-initiator: evidence }))
            (map-set Disputes escrow-id (merge dispute { evidence-responder: evidence })))
        (ok true)))

;; Register as a mediator
(define-public (register-mediator (specialization (string-ascii 100)))
    (let ((caller tx-sender))
        (asserts! (>= (get-user-reputation caller) MIN-MEDIATOR-REPUTATION) err-unauthorized)
        (asserts! (is-none (map-get? Mediators caller)) err-escrow-exists)
        (asserts! (> (len specialization) u0) err-invalid-input)
        
        (map-set Mediators caller {
            reputation-score: (get-user-reputation caller),
            total-mediations: u0,
            successful-resolutions: u0,
            active-disputes: u0,
            specialization: specialization,
            is-active: true,
            joined-at: stacks-block-height,
            last-activity: stacks-block-height
        })
        (ok true)))

;; Vote on dispute resolution as mediator
(define-public (vote-on-dispute (escrow-id uint) (vote-for principal) (reasoning (string-ascii 300)) (confidence uint))
    (let ((dispute (unwrap! (map-get? Disputes escrow-id) err-not-found))
          (mediator-data (unwrap! (map-get? Mediators tx-sender) err-not-mediator))
          (caller tx-sender))
        (asserts! (get is-active mediator-data) err-not-mediator)
        (asserts! (< stacks-block-height (get resolution-deadline dispute)) err-escrow-expired)
        (asserts! (or (is-eq vote-for (get plaintiff dispute)) (is-eq vote-for (get defendant dispute))) err-invalid-input)
        (asserts! (is-none (map-get? MediationVotes {dispute-id: escrow-id, mediator: caller})) err-already-voted)
        (asserts! (and (>= confidence u1) (<= confidence u5)) err-invalid-input)
        
        (map-set MediationVotes {dispute-id: escrow-id, mediator: caller} {
            vote: vote-for,
            reasoning: reasoning,
            confidence-level: confidence,
            voted-at: stacks-block-height
        })
        
        (let ((updated-dispute 
                (merge dispute {
                    mediator-votes: (+ (get mediator-votes dispute) u1),
                    votes-for-initiator: (if (is-eq vote-for (get plaintiff dispute))
                                           (+ (get votes-for-initiator dispute) confidence)
                                           (get votes-for-initiator dispute)),
                    votes-for-responder: (if (is-eq vote-for (get defendant dispute))
                                           (+ (get votes-for-responder dispute) confidence)
                                           (get votes-for-responder dispute))
                })))
            (map-set Disputes escrow-id updated-dispute)
            
            (map-set Mediators caller (merge mediator-data {
                active-disputes: (+ (get active-disputes mediator-data) u1),
                last-activity: stacks-block-height
            }))
            
            (unwrap-panic (check-dispute-consensus escrow-id))
            (ok true))))

;; Check if dispute has reached consensus
(define-private (check-dispute-consensus (escrow-id uint))
    (let ((dispute (unwrap! (map-get? Disputes escrow-id) err-not-found)))
        (if (> (get mediator-votes dispute) u2) ;; Minimum 3 mediators
            (let ((total-votes (+ (get votes-for-initiator dispute) (get votes-for-responder dispute)))
                  (winner-votes (max (get votes-for-initiator dispute) (get votes-for-responder dispute))))
                (if (>= (/ (* winner-votes u100) total-votes) MEDIATOR-CONSENSUS-THRESHOLD)
                    (let ((winner (if (> (get votes-for-initiator dispute) (get votes-for-responder dispute))
                                    (get plaintiff dispute)
                                    (get defendant dispute))))
                        (unwrap-panic (resolve-dispute escrow-id winner))
                        (ok true))
                    (ok true)))
            (ok true))))

;; Resolve dispute with final decision
(define-private (resolve-dispute (escrow-id uint) (winner principal))
    (let ((dispute (unwrap! (map-get? Disputes escrow-id) err-not-found))
          (escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found)))
        (map-set Disputes escrow-id (merge dispute {
            final-decision: (some winner),
            resolved-at: (some stacks-block-height),
            resolution-method: "mediation-consensus"
        }))
        
        (map-set EscrowContracts escrow-id (merge escrow {
            status: ESCROW-RESOLVED
        }))
        
        (ok true)))

;; Helper functions
(define-private (get-active-escrow-between (user1 principal) (user2 principal))
    none) ;; Simplified for now

(define-private (get-user-reputation (user principal))
    u100) ;; Simplified - would integrate with main Bartr reputation

(define-private (calculate-escrow-fee (user principal) (amount uint))
    (let ((user-reputation (get-user-reputation user))
          (base-fee (/ (* amount (var-get escrow-fee-rate)) u10000)))
        (if (> user-reputation u150)
            (/ (* base-fee u75) u100) ;; 25% discount for high reputation
            base-fee)))

(define-private (get-reputation-settings (reputation uint))
    {
        min-reputation-threshold: u50,
        auto-release-threshold: u150,
        dispute-risk-multiplier: (if (< reputation u100) u150 u100),
        fee-reduction-threshold: u150
    })

(define-private (update-user-escrow-history (user principal) (new-escrows uint) (successful uint) (disputed uint) (fees uint))
    (let ((current-history (default-to {
            total-escrows: u0,
            successful-escrows: u0,
            disputed-escrows: u0,
            total-fees-paid: u0,
            escrow-reputation: u100,
            reliability-score: u100
        } (map-get? UserEscrowHistory user))))
        (let ((updated-total (+ (get total-escrows current-history) new-escrows))
              (updated-successful (+ (get successful-escrows current-history) successful))
              (updated-disputed (+ (get disputed-escrows current-history) disputed)))
            
            (map-set UserEscrowHistory user (merge current-history {
                total-escrows: updated-total,
                successful-escrows: updated-successful,
                disputed-escrows: updated-disputed,
                total-fees-paid: (+ (get total-fees-paid current-history) fees),
                reliability-score: (if (> updated-total u0)
                                     (/ (* updated-successful u100) updated-total)
                                     u100)
            }))
            (ok true))))

(define-private (max (a uint) (b uint))
    (if (>= a b) a b))

;; Auto-release escrow for high-reputation users
(define-public (auto-release-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? EscrowContracts escrow-id) err-not-found)))
        (asserts! (get auto-release-enabled escrow) err-invalid-input)
        (asserts! (is-eq (get status escrow) ESCROW-FUNDED) err-invalid-status)
        (asserts! (> stacks-block-height (get completion-deadline escrow)) err-invalid-input)
        (asserts! (>= (get-user-reputation (get initiator escrow)) u150) err-unauthorized)
        (asserts! (>= (get-user-reputation (get responder escrow)) u150) err-unauthorized)
        
        (unwrap-panic (complete-escrow escrow-id))
        (ok true)))

;; Toggle dispute resolution system
(define-public (toggle-dispute-resolution)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set dispute-resolution-enabled (not (var-get dispute-resolution-enabled)))
        (ok (var-get dispute-resolution-enabled))))

;; Read-only functions
(define-read-only (get-escrow (escrow-id uint))
    (map-get? EscrowContracts escrow-id))

(define-read-only (get-dispute (escrow-id uint))
    (map-get? Disputes escrow-id))

(define-read-only (get-mediator (mediator principal))
    (map-get? Mediators mediator))

(define-read-only (get-mediation-vote (dispute-id uint) (mediator principal))
    (map-get? MediationVotes {dispute-id: dispute-id, mediator: mediator}))

(define-read-only (get-user-escrow-history (user principal))
    (map-get? UserEscrowHistory user))

(define-read-only (get-escrow-stats)
    {
        total-escrows: (var-get total-escrows-created),
        escrow-fee-rate: (var-get escrow-fee-rate),
        dispute-resolution-enabled: (var-get dispute-resolution-enabled),
        min-mediator-reputation: MIN-MEDIATOR-REPUTATION,
        consensus-threshold: MEDIATOR-CONSENSUS-THRESHOLD
    })

(define-read-only (calculate-escrow-fee-estimate (user principal) (amount uint))
    (calculate-escrow-fee user amount))

(define-read-only (is-eligible-mediator (user principal))
    (>= (get-user-reputation user) MIN-MEDIATOR-REPUTATION))
