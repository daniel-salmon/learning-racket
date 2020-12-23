#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 1000)
(define HEIGHT 1000)
(define TRAIN-WIDTH 160)
(define TRAIN-HEIGHT 160)
(define SPEED 10)
(define TRAIN . )
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define (move-left w)
  (cond [(< w (* -1 TRAIN-WIDTH)) (+ w WIDTH)]
        [else (- w SPEED)]))

(define (place-train x)
  (place-image TRAIN x (/ HEIGHT 2)
               BACKGROUND))

(define (place-wrapped-train x)
  (place-image TRAIN x (/ HEIGHT 2)
               (place-image TRAIN (+ WIDTH x) (/ HEIGHT 2)
               BACKGROUND)))

(define (draw-train w)
  (cond [(<= w 0) (place-wrapped-train w)]
        [else (place-train w)]))

(big-bang (+ WIDTH TRAIN-WIDTH)
  (on-tick move-left)
  (to-draw draw-train))
