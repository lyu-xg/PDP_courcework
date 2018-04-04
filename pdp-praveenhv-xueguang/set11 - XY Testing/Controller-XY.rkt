#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require "Model.rkt")

;; place it common for display purpose.
(define FIELD-WIDTH 150)
(define FIELD-HEIGHT 125)
(define INIT-X (/ FIELD-WIDTH 2))
(define INIT-Y (/ FIELD-HEIGHT 2))
(provide Controller-XY%)

(define Controller-XY%
  (class* object% (Controller<%>)
    
    (init-field model)  ; the model
    
    (init-field [x 300] [y 250])
    
    
    
    (init-field [width 150][height 100])
    
    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])
    
    ;; the position of the particle common for controller
    (field [particle-x FIELD-WIDTH])
    (field [particle-y FIELD-HEIGHT])
    
    (field [particle-vx 0])
    (field [particle-vy 0])
    
    ;;Common for all Handlers
    (field [handle-x 10])
    (field [handle-y 10])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])
    
    (super-new)
    
    ;;registers the current controller in the model. and sends the changes to it
    ;;the model performs the actions and returns the details.
    (send model register this)
    
    ;; receive signal from the controller. getting the position 
    (define/public (receive-signal sig)
      (printf "received signal")
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-x sig))
         (set! particle-y (report-position-y sig))
         ]
        [(report-velocity? sig)
         (set! particle-vx (report-velocity-vx sig))
         (set! particle-vy (report-velocity-vy sig))
         ])) 
    
    ;;==============================================================================
    (define particle-image (overlay (circle 5 "solid" "black")  (circle 10 "solid" "red")))
    
    (define rectangle-image (rectangle 200 150 "outline" "blue"))
    
    (define rectangle-handle (rectangle 10 10 "outline" "black"))

    (define rectangle-bunch (overlay/align  "left" "top" rectangle-handle
     (overlay rectangle-image  (rectangle 250 200 "outline" "black")) ))
    
    (define rectangle-image-out (empty-scene 250 200 ))
    (define test-particle (overlay (place-image particle-image particle-x particle-y rectangle-image) rectangle-bunch))
    (define/public (add-to-scene scene)
      (place-image test-particle 300 250 scene)
      )
    ;;==============================================================================    
    
    (define/public (after-key-event kev) 'a)
    
    (define/public (after-button-down mx my)
      (cond
        [(in-handler? mx my) (set! saved-mx mx) (set! saved-my my) ]
        
        [else this]
        )
      
      )
    
    (define/public (in-handler? mx my)
      (cond
        [(and (and (>= mx (- x 150)) (<= mx (- x 140))) 
              (and (>= my (- y 125)) (<= my (- y 115)))) (set! selected? true)] 
        [else (set! selected? false)]
        ))
    
    (define/public (after-button-up mx my)
      (set! selected? false)
      )
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ x (- mx saved-mx)))
            (set! y (+ y (- my saved-my)))
            (set! saved-mx mx)
            (set! saved-my my)
            )
          this
          ))
    
    
    (define/public (after-move mx my) this )
    
    (define/public (after-tick) this)
    
    (define/public (for-test:is-selected?) selected?)
    
    ))

(begin-for-test
  
  (local
    ( (define c1 (new Controller-XY% [model (make-model)]))
      )
    
    (send c1 after-button-down 152 127)
    (check-equal? (send c1 for-test:is-selected?) true "the handler was selected")
    )
  
  )
