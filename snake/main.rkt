#lang racket
(require 2htdp/universe 2htdp/image math)

(struct pit (snake goos rocks) #:transparent)
(struct snake (dir segs eaten-goos) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire super) #:transparent)
(struct rock (loc) #:transparent)

(define TICK-RATE 1/10)
(define SIZE 30)
(define SEG-SIZE 15)
(define WIDTH-PX (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

(define ROCK-MOVE-PROB .01)
(define NEW-GOO-PROB .01)
(define SUPER-GOO-PROB .1)
(define MEAN-EXPIRATION-TIME 150)

(define ROCK-IMG .)
(define GOO-IMG .)
(define SUPER-GOO-IMG .)
(define SEG-IMG .)
(define HEAD-IMG .)

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn) (snake-eaten-goos sn)))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))
         (add1 (snake-eaten-goos sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))
         (snake-eaten-goos sn)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-super g)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (define d (poisson-dist MEAN-EXPIRATION-TIME))
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       (sample d)
       (<= (random) SUPER-GOO-PROB)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

(define (age-goo goos)
  (rot (renew goos)))

(define (add-goo goos)
  (if (<= (random) NEW-GOO-PROB)
      (cons (fresh-goo) goos)
      goos))

(define (fresh-rock)
  (rock (posn (add1 (random (sub1 SIZE)))
              (add1 (random (sub1 SIZE))))))

(define (move-rock rock)
  (if (<= (random) ROCK-MOVE-PROB)
      (fresh-rock)
      rock))

(define (move-rocks rocks)
  (cond [(empty? rocks) empty]
        [else (cons (move-rock (first rocks)) (move-rocks (rest rocks)))]))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define rocks (pit-rocks w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit
       (if (goo-super goo-to-eat)
           (grow (grow snake))
           (grow snake))
       (add-goo (age-goo (eat goos goo-to-eat)))
       (move-rocks rocks))
      (pit (slither snake) (add-goo (age-goo goos)) (move-rocks rocks))))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (opposite-dir? d1 d2)
  (cond [(and (string=? d1 "up") (string=? d2 "down")) #t]
        [(and (string=? d1 "down") (string=? d2 "up")) #t]
        [(and (string=? d1 "left") (string=? d2 "right")) #t]
        [(and (string=? d1 "right") (string=? d2 "left")) #t]
        [else #f]))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w) (pit-rocks w))]))

(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (img-list+scene posns imgs scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               (first imgs)
               (img-list+scene (rest posns) (rest imgs) scene))]))

(define (rock-list+scene rocks scene)
  (define (get-posns-from-rock rocks)
    (cond [(empty? rocks) empty]
          [else (cons (rock-loc (first rocks))
                      (get-posns-from-rock (rest rocks)))]))
  (define (get-imgs-from-rock rocks)
    (cond [(empty? rocks) empty]
          [else (cons ROCK-IMG (get-imgs-from-rock (rest rocks)))]))
  (img-list+scene (get-posns-from-rock rocks) (get-imgs-from-rock rocks) scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (define (get-imgs-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (if (goo-super (first goos)) SUPER-GOO-IMG GOO-IMG)
                      (get-imgs-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) (get-imgs-from-goo goos) scene))

(define (snake+scene snake scene)
  (define (get-body-imgs body)
    (cond [(empty? body) empty]
          [else (cons SEG-IMG (get-body-imgs (rest body)))]))
  (define snake-body-scene
    (img-list+scene (snake-body snake) (get-body-imgs (snake-body snake)) scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (rock-list+scene (pit-rocks w)
                                (goo-list+scene (pit-goos w) MT-SCENE))))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (rock-colliding? snake rocks)
  (cond [(empty? rocks) #f]
        [else (define x (posn-x (snake-head snake)))
              (define y (posn-y (snake-head snake)))
              (define rock-x (posn-x (rock-loc (first rocks))))
              (define rock-y (posn-y (rock-loc (first rocks))))
              (or (and (= x rock-x) (= y rock-y)) (rock-colliding? snake (rest rocks)))]))

(define (dead? w)
  (define snake (pit-snake w))
  (define rocks (pit-rocks w))
  (or (self-colliding? snake) (wall-colliding? snake) (rock-colliding? snake rocks)))

(define (render-end w)
  (define n (number->string (snake-eaten-goos (pit-snake w))))
  (overlay (text (string-append "Game Over\nYou've Eaten " n " Goos") ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)) 0)
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo))
                 (list (fresh-rock)
                       (fresh-rock)
                       (fresh-rock)))
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))

