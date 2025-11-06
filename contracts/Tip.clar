(define-constant contract-owner tx-sender)

(define-data-var minimum-tip uint u1000)
(define-data-var paused bool false)

(define-map recipients principal { active: bool, total: uint })

(define-public (register-recipient)
  (let ((existing (map-get? recipients tx-sender)))
    (if (is-some existing)
        (ok true)
        (begin
          (map-set recipients tx-sender { active: true, total: u0 })
          (ok true)))))

(define-public (set-minimum-tip (value uint))
  (if (is-eq tx-sender contract-owner)
      (begin (var-set minimum-tip value) (ok value))
      (err u100)))

(define-public (set-paused (value bool))
  (if (is-eq tx-sender contract-owner)
      (begin (var-set paused value) (ok value))
      (err u101)))

(define-public (tip-to-recipient (recipient principal) (amount uint))
  (let (
        (p (var-get paused))
        (m (var-get minimum-tip))
        (info (map-get? recipients recipient))
      )
    (if p
        (err u102)
        (if (>= amount m)
            (if (is-some info)
                (let ((data (unwrap-panic info)))
                  (if (get active data)
                      (match (stx-transfer? amount tx-sender recipient)
                        success (begin
                          (map-set recipients recipient { active: true, total: (+ (get total data) amount) })
                          (ok true))
                        error (err u104))
                      (err u105)))
                (err u106))
            (err u107)))))

(define-read-only (get-recipient (who principal))
  (map-get? recipients who))

(define-read-only (get-minimum-tip)
  (var-get minimum-tip))

(define-read-only (get-paused)
  (var-get paused))

(define-read-only (get-owner)
  contract-owner)

