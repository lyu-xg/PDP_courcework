#lang racket

(require racket/gui/base)
(provide face1 face2)

(define face1 (new image-snip%))
(send face1 load-file "face1.png")
(define face2 (new image-snip%))
(send face2 load-file "face2.png")