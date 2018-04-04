#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define canvas-width 600)
(define canvas-height 500)
(define empty-canvas (empty-scene canvas-width canvas-height))

(define particle-image (overlay (circle 5 "solid" "black")  (circle 10 "solid" "red")))

(define rectangle-image (rectangle 150 100 "outline" "blue"))

;;(define rectangle-handle (rectangle 10 10 "outline" "black"))

(define rectangle-image-out (empty-scene 250 200 ))

(define rectangle-bunch (overlay/align  "left" "top" (rectangle 10 10 "outline" "black") (overlay (rectangle 150 100 "outline" "blue")  (rectangle 200 150 "outline" "black")) ))

;(place-image particle-image 75 50 empty-canvas);

(define xy-view (place-image (rectangle 10 10 "outline" "black") 5 5  rectangle-bunch))

(define final-pos (place-image xy-view 300 250 (place-image particle-image 300 250 empty-canvas)))

;(define xy-views  (place-image particle-image 100 75  rectangle-bunch)  rectangle-bunch)

(define test-particle (overlay (place-image particle-image 75 50 rectangle-image) rectangle-bunch))

(define final-image-set (place-image test-particle 300 250 empty-canvas))

(define rectangle-outline (empty-scene 250 200 ))
(define final-image (place-image (rectangle 10 10 "outline" "black") 5 5 (place-image (rectangle 200 150 "outline" "blue")  125 100  rectangle-image-out)))
(define final-scene (place-image final-image 300 250 empty-canvas))

