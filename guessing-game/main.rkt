#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big))
(struct state [guesses interval])

;; Define constants
(define TEXT-SIZE 11)
(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))
(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 1000))
(define HEIGHT 1500)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

;; Define auxiliary functions
(define (guess w)
  (define t (state-interval w))
  (quotient (+ (interval-small t) (interval-big t)) 2))

(define (smaller w)
  (define g (state-guesses w))
  (define t (state-interval w))
  (define new-interval (interval (interval-small t) (max (interval-small t) (sub1 (guess w)))))
  (define new-state (struct-copy state w [interval new-interval]))
  (struct-copy state new-state [guesses (add1 g)]))

(define (bigger w)
  (define g (state-guesses w))
  (define t (state-interval w))
  (define new-interval (interval (min (interval-big t) (add1 (guess w))) (interval-big t)))
  (define new-state (struct-copy state w [interval new-interval]))
  (struct-copy state new-state [guesses (add1 g)]))

;; Define handlers
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (define x (number->string (guess w)))
  (define g (number->string (state-guesses w)))
  (overlay (text (string-append "Your number was:" x "\nWhich took " g " guesses.") SIZE COLOR) MT-SC))

(define (single? w)
  (define t (state-interval w))
  (= (interval-small t) (interval-big t)))

(define (start lower upper)
  (big-bang (state 0 (interval lower upper))
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))
