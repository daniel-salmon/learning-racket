#lang racket
(require 2htdp/universe 2htdp/image)

(struct position (x y))
(struct state [position])

(define WIDTH 1000)
(define HEIGHT 1000)
(define GRAVITY 3)
(define SPEED 10)
(define IMAGE-of-UFO .)

(define (gravity s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (state (position x (+ y GRAVITY))))

(define (up s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (state (position x (- y SPEED))))

(define (down s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (state (position x (+ y SPEED))))

(define (left s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (state (position (- x SPEED) y)))

(define (right s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (state (position (+ x SPEED) y)))

(define (move-ufo s key)
  (cond [(key=? key "up") (up s)]
        [(key=? key "down") (down s)]
        [(key=? key "left") (left s)]
        [(key=? key "right") (right s)]
        [else s]))

(define (draw-a-ufo-onto-an-empty-scene s)
  (define x (position-x (state-position s)))
  (define y (position-y (state-position s)))
  (place-image IMAGE-of-UFO x y
               (empty-scene WIDTH HEIGHT)))

(define (saucer-at-bottom s)
  (define y (position-y (state-position s)))
  (>= y (- HEIGHT 50)))

(define (start)
  (big-bang (state (position (/ WIDTH 2) 50))
    (on-tick gravity)
    (on-key move-ufo)
    (to-draw draw-a-ufo-onto-an-empty-scene)
    (stop-when saucer-at-bottom)))

