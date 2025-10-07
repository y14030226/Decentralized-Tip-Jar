(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-amount (err u101))
(define-constant err-transfer-failed (err u102))
(define-constant err-no-tips (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-same-sender (err u105))

(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u401))
(define-constant err-already-released (err u409))
(define-constant err-not-expired (err u403))
(define-constant err-milestone-not-reached (err u406))
(define-constant err-self-referral (err u407))
(define-constant err-no-referral-rewards (err u408))
(define-constant err-no-active-campaign (err u409))
(define-constant err-campaign-depleted (err u410))
(define-constant err-campaign-active (err u411))

(define-data-var escrow-counter uint u0)
(define-data-var referral-reward-rate uint u10)

(define-data-var matching-campaign-active bool false)
(define-data-var matching-pool uint u0)
(define-data-var matching-ratio uint u100)
(define-data-var total-matched uint u0)

(define-constant milestone-bronze u1000000)
(define-constant milestone-silver u5000000)
(define-constant milestone-gold u10000000)
(define-constant milestone-platinum u25000000)

(define-map user-achievements principal {
  bronze: bool,
  silver: bool,
  gold: bool,
  platinum: bool,
  last-updated: uint
})

(define-data-var total-tips-received uint u0)
(define-data-var total-tips-count uint u0)
(define-data-var contract-active bool true)

(define-map tips-by-sender principal uint)
(define-map tip-history uint {sender: principal, amount: uint, stacks-block-height: uint, timestamp: uint})
(define-map sender-tip-count principal uint)

(define-map user-referrals principal principal)
(define-map referral-rewards principal uint)
(define-map referral-counts principal uint)

(define-public (send-tip (amount uint))
  (let (
    (sender tx-sender)
    (current-tips (default-to u0 (map-get? tips-by-sender sender)))
    (current-count (default-to u0 (map-get? sender-tip-count sender)))
    (tip-id (+ (var-get total-tips-count) u1))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq sender contract-owner)) err-same-sender)
    (match (stx-transfer? amount sender contract-owner)
      success (begin
        (var-set total-tips-received (+ (var-get total-tips-received) amount))
        (var-set total-tips-count tip-id)
        (map-set tips-by-sender sender (+ current-tips amount))
        (map-set sender-tip-count sender (+ current-count u1))
        (map-set tip-history tip-id {
          sender: sender,
          amount: amount,
          stacks-block-height: stacks-block-height,
          timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height))
        })
        (unwrap-panic (update-user-achievements sender))
        (ok tip-id)
      )
      error err-transfer-failed
    )
  )
)

(define-public (withdraw-tips (amount uint))
  (let (
    (contract-balance (stx-get-balance (as-contract tx-sender)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount contract-balance) err-insufficient-amount)
    (match (as-contract (stx-transfer? amount tx-sender contract-owner))
      success (ok amount)
      error err-transfer-failed
    )
  )
)

(define-public (withdraw-all-tips)
  (let (
    (contract-balance (stx-get-balance (as-contract tx-sender)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> contract-balance u0) err-no-tips)
    (match (as-contract (stx-transfer? contract-balance tx-sender contract-owner))
      success (ok contract-balance)
      error err-transfer-failed
    )
  ))
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

(define-public (send-bulk-tip (recipients (list 10 principal)) (amount uint))
  (let (
    (sender tx-sender)
    (total-amount (* amount (len recipients)))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> (len recipients) u0) err-invalid-amount)
    (match (stx-transfer? total-amount sender (as-contract tx-sender))
      success (begin
        (map distribute-tip-to-recipient recipients)
        (var-set total-tips-received (+ (var-get total-tips-received) total-amount))
        (ok total-amount)
      )
      error err-transfer-failed
    )
  )
)

(define-private (distribute-tip-to-recipient (recipient principal))
  (let (
    (amount u1000000)
  )
    (as-contract (stx-transfer? amount tx-sender recipient))
  )
)

(define-public (tip-with-message (amount uint) (message (string-ascii 280)))
  (let (
    (sender tx-sender)
    (tip-id (+ (var-get total-tips-count) u1))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq sender contract-owner)) err-same-sender)
    (match (stx-transfer? amount sender contract-owner)
      success (begin
        (var-set total-tips-received (+ (var-get total-tips-received) amount))
        (var-set total-tips-count tip-id)
        (map-set tip-messages tip-id {
          sender: sender,
          amount: amount,
          message: message,
          stacks-block-height: stacks-block-height
        })
        (ok tip-id)
      )
      error err-transfer-failed
    )
  )
)

(define-map tip-messages uint {sender: principal, amount: uint, message: (string-ascii 280), stacks-block-height: uint})

(define-read-only (get-contract-owner)
  contract-owner
)


(define-read-only (get-total-tips-received)
  (var-get total-tips-received))


(define-read-only (get-total-tips-count)
  (var-get total-tips-count))


(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender)))


(define-read-only (get-tips-by-sender (sender principal))
  (default-to u0 (map-get? tips-by-sender sender)))


(define-read-only (get-sender-tip-count (sender principal))
  (default-to u0 (map-get? sender-tip-count sender)))


(define-read-only (get-tip-history (tip-id uint))
  (map-get? tip-history tip-id))


(define-read-only (get-contract-status)
  (var-get contract-active))


(define-read-only (get-tip-message (tip-id uint))
  (map-get? tip-messages tip-id))


(define-read-only (get-contract-stats)
  {
    total-tips: (var-get total-tips-received),
    tip-count: (var-get total-tips-count),
    contract-balance: (stx-get-balance (as-contract tx-sender)),
    owner: contract-owner,
    active: (var-get contract-active)
  })


(define-read-only (calculate-average-tip)
  (if (> (var-get total-tips-count) u0)
    (/ (var-get total-tips-received) (var-get total-tips-count))
    u0
  )
)

(define-read-only (get-top-tipper-info (sender principal))
  (let (
    (total-tipped (default-to u0 (map-get? tips-by-sender sender)))
    (tip-count (default-to u0 (map-get? sender-tip-count sender)))
  )
    {
      total-tipped: total-tipped,
      tip-count: tip-count,
      average-tip: (if (> tip-count u0)
                      (/ total-tipped tip-count)
                      u0)
    }
  )
)

(define-read-only (is-generous-tipper (sender principal))
  (let (
    (sender-average (let (
                          (tip-count (default-to u0 (map-get? sender-tip-count sender)))
                          (total-tipped (default-to u0 (map-get? tips-by-sender sender)))
                        )
                        (if (> tip-count u0)
                          (/ total-tipped tip-count)
                          u0)))
    (global-average (calculate-average-tip))
  )
    (and (> sender-average u0) (>= sender-average global-average))
  )
)



(define-map escrows uint {
  creator: principal,
  beneficiary: principal,
  amount: uint,
  deadline: uint,
  released: bool,
  description: (string-ascii 500)
})

(define-public (create-escrow (beneficiary principal) (deadline uint) (description (string-ascii 500)))
  (let (
    (escrow-id (+ (var-get escrow-counter) u1))
    (amount (stx-get-balance tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> deadline stacks-block-height) err-invalid-amount)
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (begin
        (var-set escrow-counter escrow-id)
        (map-set escrows escrow-id {
          creator: tx-sender,
          beneficiary: beneficiary,
          amount: amount,
          deadline: deadline,
          released: false,
          description: description
        })
        (ok escrow-id)
      )
      error err-transfer-failed
    )
  )
)

(define-public (release-escrow (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator escrow-data)) err-unauthorized)
    (asserts! (not (get released escrow-data)) err-already-released)
    (match (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get beneficiary escrow-data)))
      success (begin
        (map-set escrows escrow-id (merge escrow-data {released: true}))
        (ok (get amount escrow-data))
      )
      error err-transfer-failed
    )
  )
)

(define-public (claim-expired-escrow (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) err-not-found))
  )
    (asserts! (>= stacks-block-height (get deadline escrow-data)) err-not-expired)
    (asserts! (not (get released escrow-data)) err-already-released)
    (match (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get creator escrow-data)))
      success (begin
        (map-set escrows escrow-id (merge escrow-data {released: true}))
        (ok (get amount escrow-data))
      )
      error err-transfer-failed
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id))

(define-read-only (get-total-escrows)
  (var-get escrow-counter))

(define-public (update-user-achievements (user principal))
  (let (
    (total-tipped (default-to u0 (map-get? tips-by-sender user)))
    (current-achievements (default-to {bronze: false, silver: false, gold: false, platinum: false, last-updated: u0} 
                                      (map-get? user-achievements user)))
    (new-bronze (>= total-tipped milestone-bronze))
    (new-silver (>= total-tipped milestone-silver))
    (new-gold (>= total-tipped milestone-gold))
    (new-platinum (>= total-tipped milestone-platinum))
  )
    (map-set user-achievements user {
      bronze: new-bronze,
      silver: new-silver,
      gold: new-gold,
      platinum: new-platinum,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-read-only (get-user-achievements (user principal))
  (default-to {bronze: false, silver: false, gold: false, platinum: false, last-updated: u0}
              (map-get? user-achievements user)))

(define-read-only (get-milestone-progress (user principal))
  (let (
    (total-tipped (default-to u0 (map-get? tips-by-sender user)))
    (next-milestone (if (< total-tipped milestone-bronze) milestone-bronze
                       (if (< total-tipped milestone-silver) milestone-silver
                          (if (< total-tipped milestone-gold) milestone-gold
                             (if (< total-tipped milestone-platinum) milestone-platinum u0)))))
  )
    {
      total-tipped: total-tipped,
      next-milestone: next-milestone,
      progress-percentage: (if (> next-milestone u0)
                              (/ (* total-tipped u100) next-milestone)
                              u100)
    }
  )
)

(define-read-only (get-achievement-tier (user principal))
  (let (
    (total-tipped (default-to u0 (map-get? tips-by-sender user)))
  )
    (if (>= total-tipped milestone-platinum) u4
       (if (>= total-tipped milestone-gold) u3
          (if (>= total-tipped milestone-silver) u2
             (if (>= total-tipped milestone-bronze) u1 u0))))
  )
)

(define-read-only (get-milestone-thresholds)
  {
    bronze: milestone-bronze,
    silver: milestone-silver,
    gold: milestone-gold,
    platinum: milestone-platinum
  }
)

(define-public (send-tip-with-referral (amount uint) (referrer principal))
  (let (
    (sender tx-sender)
    (current-tips (default-to u0 (map-get? tips-by-sender sender)))
    (current-count (default-to u0 (map-get? sender-tip-count sender)))
    (tip-id (+ (var-get total-tips-count) u1))
    (reward-amount (/ (* amount (var-get referral-reward-rate)) u100))
    (owner-amount (- amount reward-amount))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq sender contract-owner)) err-same-sender)
    (asserts! (not (is-eq sender referrer)) err-self-referral)
    (match (stx-transfer? owner-amount sender contract-owner)
      success (begin
        (match (stx-transfer? reward-amount sender (as-contract tx-sender))
          reward-success (begin
            (var-set total-tips-received (+ (var-get total-tips-received) amount))
            (var-set total-tips-count tip-id)
            (map-set tips-by-sender sender (+ current-tips amount))
            (map-set sender-tip-count sender (+ current-count u1))
            (map-set tip-history tip-id {
              sender: sender,
              amount: amount,
              stacks-block-height: stacks-block-height,
              timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height))
            })
            (map-set user-referrals sender referrer)
            (map-set referral-rewards referrer (+ (default-to u0 (map-get? referral-rewards referrer)) reward-amount))
            (map-set referral-counts referrer (+ (default-to u0 (map-get? referral-counts referrer)) u1))
            (unwrap-panic (update-user-achievements sender))
            (ok tip-id)
          )
          error err-transfer-failed
        )
      )
      error err-transfer-failed
    )
  )
)

(define-public (withdraw-referral-rewards)
  (let (
    (reward-balance (default-to u0 (map-get? referral-rewards tx-sender)))
  )
    (asserts! (> reward-balance u0) err-no-referral-rewards)
    (match (as-contract (stx-transfer? reward-balance tx-sender tx-sender))
      success (begin
        (map-set referral-rewards tx-sender u0)
        (ok reward-balance)
      )
      error err-transfer-failed
    )
  )
)

(define-public (set-referral-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u50) err-invalid-amount)
    (var-set referral-reward-rate new-rate)
    (ok new-rate)
  )
)

(define-read-only (get-referral-info (user principal))
  {
    referrer: (map-get? user-referrals user),
    reward-balance: (default-to u0 (map-get? referral-rewards user)),
    referral-count: (default-to u0 (map-get? referral-counts user))
  }
)

(define-read-only (get-referral-rate)
  (var-get referral-reward-rate))

(define-read-only (get-referral-earnings (referrer principal))
  (default-to u0 (map-get? referral-rewards referrer)))

(define-read-only (get-referral-count (referrer principal))
  (default-to u0 (map-get? referral-counts referrer)))

(define-public (create-matching-campaign (pool-amount uint) (ratio uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get matching-campaign-active)) err-campaign-active)
    (asserts! (> pool-amount u0) err-invalid-amount)
    (asserts! (> ratio u0) err-invalid-amount)
    (asserts! (<= ratio u200) err-invalid-amount)
    (match (stx-transfer? pool-amount tx-sender (as-contract tx-sender))
      success (begin
        (var-set matching-campaign-active true)
        (var-set matching-pool pool-amount)
        (var-set matching-ratio ratio)
        (var-set total-matched u0)
        (ok pool-amount)
      )
      error err-transfer-failed
    )
  )
)

(define-public (end-matching-campaign)
  (let (
    (remaining-pool (var-get matching-pool))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get matching-campaign-active) err-no-active-campaign)
    (var-set matching-campaign-active false)
    (var-set matching-pool u0)
    (if (> remaining-pool u0)
      (match (as-contract (stx-transfer? remaining-pool tx-sender contract-owner))
        success (ok remaining-pool)
        error err-transfer-failed
      )
      (ok u0)
    )
  )
)

(define-public (send-matched-tip (amount uint))
  (let (
    (sender tx-sender)
    (current-tips (default-to u0 (map-get? tips-by-sender sender)))
    (current-count (default-to u0 (map-get? sender-tip-count sender)))
    (tip-id (+ (var-get total-tips-count) u1))
    (match-amount (/ (* amount (var-get matching-ratio)) u100))
    (available-pool (var-get matching-pool))
    (actual-match (if (> match-amount available-pool) available-pool match-amount))
    (total-amount (+ amount actual-match))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq sender contract-owner)) err-same-sender)
    (asserts! (var-get matching-campaign-active) err-no-active-campaign)
    (asserts! (> available-pool u0) err-campaign-depleted)
    (match (stx-transfer? amount sender contract-owner)
      success (begin
        (var-set total-tips-received (+ (var-get total-tips-received) total-amount))
        (var-set total-tips-count tip-id)
        (var-set matching-pool (- available-pool actual-match))
        (var-set total-matched (+ (var-get total-matched) actual-match))
        (map-set tips-by-sender sender (+ current-tips total-amount))
        (map-set sender-tip-count sender (+ current-count u1))
        (map-set tip-history tip-id {
          sender: sender,
          amount: total-amount,
          stacks-block-height: stacks-block-height,
          timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height))
        })
        (unwrap-panic (update-user-achievements sender))
        (if (is-eq (var-get matching-pool) u0)
          (var-set matching-campaign-active false)
          true
        )
        (ok {tip-id: tip-id, matched: actual-match, total: total-amount})
      )
      error err-transfer-failed
    )
  )
)

(define-read-only (get-matching-campaign-status)
  {
    active: (var-get matching-campaign-active),
    pool-remaining: (var-get matching-pool),
    match-ratio: (var-get matching-ratio),
    total-matched: (var-get total-matched)
  }
)

(define-read-only (calculate-match-preview (tip-amount uint))
  (let (
    (match-amount (/ (* tip-amount (var-get matching-ratio)) u100))
    (available-pool (var-get matching-pool))
    (actual-match (if (> match-amount available-pool) available-pool match-amount))
  )
    {
      user-tip: tip-amount,
      match-amount: actual-match,
      total-value: (+ tip-amount actual-match),
      campaign-active: (var-get matching-campaign-active)
    }
  )
)
