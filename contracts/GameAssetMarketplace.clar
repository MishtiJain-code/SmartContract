;; GameAsset Marketplace Contract
;; Cross-game NFT marketplace for trading items that work across multiple blockchain games

;; Define the NFT
(define-non-fungible-token game-asset uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-asset-owner (err u101))
(define-constant err-asset-not-found (err u102))
(define-constant err-asset-not-for-sale (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-asset-already-exists (err u106))

;; Data variables
(define-data-var last-asset-id uint u0)

;; Asset metadata structure
(define-map asset-metadata uint {
  name: (string-ascii 64),
  description: (string-ascii 256),
  game-id: (string-ascii 32),
  rarity: (string-ascii 16),
  asset-type: (string-ascii 32),
  image-uri: (string-ascii 256)
})

;; Marketplace listings
(define-map asset-listings uint {
  seller: principal,
  price: uint,
  is-active: bool
})

;; Game compatibility tracking
(define-map asset-compatibility uint (list 10 (string-ascii 32)))

;; Function 1: Mint new game asset
(define-public (mint-game-asset 
  (recipient principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (game-id (string-ascii 32))
  (rarity (string-ascii 16))
  (asset-type (string-ascii 32))
  (image-uri (string-ascii 256))
  (compatible-games (list 10 (string-ascii 32))))
  (let
    ((asset-id (+ (var-get last-asset-id) u1)))
    (begin
      ;; Mint the NFT
      (try! (nft-mint? game-asset asset-id recipient))
      
      ;; Store metadata
      (map-set asset-metadata asset-id {
        name: name,
        description: description,
        game-id: game-id,
        rarity: rarity,
        asset-type: asset-type,
        image-uri: image-uri
      })
      
      ;; Store game compatibility
      (map-set asset-compatibility asset-id compatible-games)
      
      ;; Update last asset ID
      (var-set last-asset-id asset-id)
      
      (ok asset-id))))

;; Function 2: List asset for sale and execute purchase
(define-public (list-and-buy-asset (asset-id uint) (price uint) (buyer principal))
  (let
    ((asset-owner (unwrap! (nft-get-owner? game-asset asset-id) err-asset-not-found))
     (listing (map-get? asset-listings asset-id)))
    (begin
      ;; If asset is not listed, list it for sale (only owner can list)
      (if (is-none listing)
        (begin
          (asserts! (is-eq tx-sender asset-owner) err-not-asset-owner)
          (asserts! (> price u0) err-invalid-price)
          (map-set asset-listings asset-id {
            seller: asset-owner,
            price: price,
            is-active: true
          })
          (ok "Asset listed for sale"))
        ;; If asset is already listed, execute purchase
        (let
          ((listing-data (unwrap-panic listing)))
          (begin
            (asserts! (get is-active listing-data) err-asset-not-for-sale)
            (asserts! (>= price (get price listing-data)) err-insufficient-payment)
            
            ;; Transfer payment to seller
            (try! (stx-transfer? (get price listing-data) buyer (get seller listing-data)))
            
            ;; Transfer NFT to buyer
            (try! (nft-transfer? game-asset asset-id asset-owner buyer))
            
            ;; Remove listing
            (map-delete asset-listings asset-id)
            
            (ok "Asset purchased successfully")))))))

;; Read-only functions
(define-read-only (get-asset-metadata (asset-id uint))
  (map-get? asset-metadata asset-id))

(define-read-only (get-asset-compatibility (asset-id uint))
  (map-get? asset-compatibility asset-id))

(define-read-only (get-asset-listing (asset-id uint))
  (map-get? asset-listings asset-id))

(define-read-only (get-asset-owner (asset-id uint))
  (nft-get-owner? game-asset asset-id))

(define-read-only (get-last-asset-id)
  (var-get last-asset-id))