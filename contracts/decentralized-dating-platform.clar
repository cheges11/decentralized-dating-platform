;; Decentralized Dating Platform Smart Contract
;; A secure, privacy-focused dating platform on Stacks blockchain

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_INVALID_INPUT (err u400))
(define-constant ERR_PROFILE_NOT_ACTIVE (err u403))

;; Platform fees (in microSTX)
(define-constant PROFILE_CREATION_FEE u1000000) ;; 1 STX
(define-constant LIKE_FEE u100000) ;; 0.1 STX
(define-constant SUPER_LIKE_FEE u500000) ;; 0.5 STX
(define-constant MESSAGE_FEE u50000) ;; 0.05 STX

;; Data structures
(define-map user-profiles
  { user: principal }
  {
    name: (string-ascii 50),
    age: uint,
    bio: (string-utf8 500),
    interests: (string-utf8 200),
    location: (string-ascii 100),
    profile-pic-hash: (string-ascii 64),
    is-active: bool,
    created-at: uint,
    last-active: uint
  }
)

(define-map user-preferences
  { user: principal }
  {
    min-age: uint,
    max-age: uint,
    preferred-location: (string-ascii 100),
    preferred-interests: (string-utf8 200),
    max-distance: uint
  }
)

(define-map likes
  { liker: principal, liked: principal }
  {
    timestamp: uint,
    is-super-like: bool,
    message: (optional (string-utf8 200))
  }
)

(define-map matches
  { user1: principal, user2: principal }
  {
    matched-at: uint,
    is-active: bool,
    last-interaction: uint
  }
)

(define-map conversations
  { match-id: (string-ascii 128) }
  {
    user1: principal,
    user2: principal,
    created-at: uint,
    message-count: uint,
    is-active: bool
  }
)

(define-map messages
  { conversation-id: (string-ascii 128), message-id: uint }
  {
    sender: principal,
    content-hash: (string-ascii 64),
    timestamp: uint,
    is-read: bool
  }
)

;; Revenue tracking
(define-data-var total-revenue uint u0)
(define-data-var total-users uint u0)
(define-data-var total-matches uint u0)

;; Profile management functions
(define-public (create-profile 
  (name (string-ascii 50))
  (age uint)
  (bio (string-utf8 500))
  (interests (string-utf8 200))
  (location (string-ascii 100))
  (profile-pic-hash (string-ascii 64))
)
  (let (
    (user tx-sender)
    (current-block (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Check if profile already exists
    (asserts! (is-none (map-get? user-profiles { user: user })) ERR_ALREADY_EXISTS)
    
    ;; Validate input
    (asserts! (and (> age u17) (< age u100)) ERR_INVALID_INPUT)
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    
    ;; Transfer fee to contract
    (try! (stx-transfer? PROFILE_CREATION_FEE user (as-contract tx-sender)))
    
    ;; Create profile
    (map-set user-profiles
      { user: user }
      {
        name: name,
        age: age,
        bio: bio,
        interests: interests,
        location: location,
        profile-pic-hash: profile-pic-hash,
        is-active: true,
        created-at: current-block,
        last-active: current-block
      }
    )
    
    ;; Update stats
    (var-set total-users (+ (var-get total-users) u1))
    (var-set total-revenue (+ (var-get total-revenue) PROFILE_CREATION_FEE))
    
    (ok true)
  )
)

(define-public (update-profile
  (name (string-ascii 50))
  (bio (string-utf8 500))
  (interests (string-utf8 200))
  (location (string-ascii 100))
)
  (let (
    (user tx-sender)
    (existing-profile (unwrap! (map-get? user-profiles { user: user }) ERR_NOT_FOUND))
    (current-block (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    (map-set user-profiles
      { user: user }
      (merge existing-profile {
        name: name,
        bio: bio,
        interests: interests,
        location: location,
        last-active: current-block
      })
    )
    (ok true)
  )
)

(define-public (set-preferences
  (min-age uint)
  (max-age uint)
  (preferred-location (string-ascii 100))
  (preferred-interests (string-utf8 200))
  (max-distance uint)
)
  (let ((user tx-sender))
    ;; Verify user has profile
    (asserts! (is-some (map-get? user-profiles { user: user })) ERR_NOT_FOUND)
    
    ;; Validate preferences
    (asserts! (and (>= min-age u18) (<= max-age u99) (>= max-age min-age)) ERR_INVALID_INPUT)
    
    (map-set user-preferences
      { user: user }
      {
        min-age: min-age,
        max-age: max-age,
        preferred-location: preferred-location,
        preferred-interests: preferred-interests,
        max-distance: max-distance
      }
    )
    (ok true)
  )
)

;; Interaction functions
(define-public (like-user (target-user principal) (is-super bool) (message (optional (string-utf8 200))))
  (let (
    (liker tx-sender)
    (fee (if is-super SUPER_LIKE_FEE LIKE_FEE))
    (current-block (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Verify both users have active profiles
    (asserts! (get is-active (unwrap! (map-get? user-profiles { user: liker }) ERR_NOT_FOUND)) ERR_PROFILE_NOT_ACTIVE)
    (asserts! (get is-active (unwrap! (map-get? user-profiles { user: target-user }) ERR_NOT_FOUND)) ERR_PROFILE_NOT_ACTIVE)
    
    ;; Can't like yourself
    (asserts! (not (is-eq liker target-user)) ERR_INVALID_INPUT)
    
    ;; Check if already liked
    (asserts! (is-none (map-get? likes { liker: liker, liked: target-user })) ERR_ALREADY_EXISTS)
    
    ;; Transfer fee
    (try! (stx-transfer? fee liker (as-contract tx-sender)))
    
    ;; Record like
    (map-set likes
      { liker: liker, liked: target-user }
      {
        timestamp: current-block,
        is-super-like: is-super,
        message: message
      }
    )
    
    ;; Check for mutual like (match)
    (match (map-get? likes { liker: target-user, liked: liker })
      existing-like (try! (create-match liker target-user))
      true
    )
    
    ;; Update revenue
    (var-set total-revenue (+ (var-get total-revenue) fee))
    
    (ok true)
  )
)

(define-private (create-match (user1 principal) (user2 principal))
  (let (
    (current-block (unwrap-panic (get-block-info? time (- block-height u1))))
    (match-key { user1: user1, user2: user2 })
  )
    ;; Create match record
    (map-set matches
      match-key
      {
        matched-at: current-block,
        is-active: true,
        last-interaction: current-block
      }
    )
    
    ;; Create conversation
    (let ((conversation-id (generate-conversation-id user1 user2)))
      (map-set conversations
        { match-id: conversation-id }
        {
          user1: user1,
          user2: user2,
          created-at: current-block,
          message-count: u0,
          is-active: true
        }
      )
    )
    
    ;; Update match count
    (var-set total-matches (+ (var-get total-matches) u1))
    
    (ok true)
  )
)

(define-private (generate-conversation-id (user1 principal) (user2 principal))
  (concat
    (unwrap-panic (to-consensus-buff? user1))
    (unwrap-panic (to-consensus-buff? user2))
  )
)

(define-public (send-message 
  (recipient principal) 
  (content-hash (string-ascii 64))
)
  (let (
    (sender tx-sender)
    (conversation-id (generate-conversation-id sender recipient))
    (current-block (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Verify match exists
    (asserts! 
      (or 
        (is-some (map-get? matches { user1: sender, user2: recipient }))
        (is-some (map-get? matches { user1: recipient, user2: sender }))
      ) 
      ERR_UNAUTHORIZED
    )
    
    ;; Transfer message fee
    (try! (stx-transfer? MESSAGE_FEE sender (as-contract tx-sender)))
    
    ;; Get conversation and update message count
    (let (
      (conversation (unwrap! (map-get? conversations { match-id: conversation-id }) ERR_NOT_FOUND))
      (new-message-count (+ (get message-count conversation) u1))
    )
      ;; Update conversation
      (map-set conversations
        { match-id: conversation-id }
        (merge conversation {
          message-count: new-message-count,
          last-interaction: current-block
        })
      )
      
      ;; Store message
      (map-set messages
        { conversation-id: conversation-id, message-id: new-message-count }
        {
          sender: sender,
          content-hash: content-hash,
          timestamp: current-block,
          is-read: false
        }
      )
    )
    
    ;; Update revenue
    (var-set total-revenue (+ (var-get total-revenue) MESSAGE_FEE))
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-preferences (user principal))
  (map-get? user-preferences { user: user })
)

(define-read-only (get-like (liker principal) (liked principal))
  (map-get? likes { liker: liker, liked: liked })
)

(define-read-only (get-match (user1 principal) (user2 principal))
  (match (map-get? matches { user1: user1, user2: user2 })
    match-data (some match-data)
    (map-get? matches { user1: user2, user2: user1 })
  )
)

(define-read-only (get-platform-stats)
  {
    total-users: (var-get total-users),
    total-matches: (var-get total-matches),
    total-revenue: (var-get total-revenue)
  }
)

;; Admin functions
(define-public (withdraw-revenue (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)

(define-public (deactivate-profile (user principal))
  (let ((profile (unwrap! (map-get? user-profiles { user: user }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set user-profiles
      { user: user }
      (merge profile { is-active: false })
    )
    (ok true)
  )
)