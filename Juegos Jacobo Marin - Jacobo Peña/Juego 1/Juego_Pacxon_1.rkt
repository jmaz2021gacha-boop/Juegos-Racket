#lang racket

(require 2htdp/universe
         2htdp/image)



(define CELL 16)
(define COLS 50)
(define ROWS 35)
(define WIDTH (* COLS CELL))
(define HEIGHT (* ROWS CELL))
(define TICK-RATE 1/16)
(define GOAL 80)

(define EMPTY 'empty)
(define CLAIMED 'claimed)
(define TRAIL 'trail)

(define-struct player (gx gy dir last-dir))
(define-struct enemy (x y dx dy))
(define-struct world (grid player enemies state claimed ticks message))

(define (idx gx gy) (+ gx (* gy COLS)))
(define (inside? gx gy) (and (<= 0 gx) (< gx COLS) (<= 0 gy) (< gy ROWS)))
(define (grid-ref g gx gy) (vector-ref g (idx gx gy)))
(define (grid-set! g gx gy v) (vector-set! g (idx gx gy) v))
(define (copy-grid g) (vector-copy g))

(define (make-start-grid)
  (define g (make-vector (* COLS ROWS) EMPTY))
  (for* ([y (in-range ROWS)]
         [x (in-range COLS)])
    (when (or (= x 0) (= y 0) (= x (sub1 COLS)) (= y (sub1 ROWS)))
      (grid-set! g x y CLAIMED)))
  g)

(define (claimed-percent g)
  (define claimed
    (for/sum ([v (in-vector g)])
      (if (eq? v CLAIMED) 1 0)))
  (floor (* 100 (/ claimed (* COLS ROWS)))))

(define (cell-center n) (+ (* n CELL) (/ CELL 2)))

(define START-PLAYER (make-player 0 (quotient ROWS 2) 'right 'right))

(define START-ENEMIES
  (list (make-enemy (cell-center 16) (cell-center 10) 3 4)
        (make-enemy (cell-center 34) (cell-center 14) -4 3)
        (make-enemy (cell-center 25) (cell-center 26) 4 -3)
        (make-enemy (cell-center 40) (cell-center 24) -3 -4)))

(define (new-game)
  (define g (make-start-grid))
  (make-world g START-PLAYER START-ENEMIES 'playing (claimed-percent g) 0
              "Flechas para moverte. Cierra zonas y alcanza 80%."))

(define (dir->delta d)
  (cond [(eq? d 'left)  (values -1 0)]
        [(eq? d 'right) (values 1 0)]
        [(eq? d 'up)    (values 0 -1)]
        [(eq? d 'down)  (values 0 1)]
        [else           (values 0 0)]))

(define (opposite? a b)
  (or (and (eq? a 'left) (eq? b 'right))
      (and (eq? a 'right) (eq? b 'left))
      (and (eq? a 'up) (eq? b 'down))
      (and (eq? a 'down) (eq? b 'up))))

(define (trail-exists? g)
  (for/or ([v (in-vector g)]) (eq? v TRAIL)))

(define (enemy-cell e)
  (values (max 0 (min (sub1 COLS) (floor (/ (enemy-x e) CELL))))
          (max 0 (min (sub1 ROWS) (floor (/ (enemy-y e) CELL))))))

(define (enemy-safe-cell? g x y)
  (define gx (max 0 (min (sub1 COLS) (floor (/ x CELL)))))
  (define gy (max 0 (min (sub1 ROWS) (floor (/ y CELL)))))
  (not (eq? (grid-ref g gx gy) CLAIMED)))

(define (move-one-enemy g e)
  (define try-x (+ (enemy-x e) (enemy-dx e)))
  (define try-y (+ (enemy-y e) (enemy-dy e)))
  (define next-dx (if (enemy-safe-cell? g try-x (enemy-y e))
                      (enemy-dx e)
                      (- (enemy-dx e))))
  (define next-dy (if (enemy-safe-cell? g (enemy-x e) try-y)
                      (enemy-dy e)
                      (- (enemy-dy e))))
  (define nx (+ (enemy-x e) next-dx))
  (define ny (+ (enemy-y e) next-dy))
  (make-enemy nx ny next-dx next-dy))

(define (move-enemies g enemies)
  (map (lambda (e) (move-one-enemy g e)) enemies))

(define (neighbors gx gy)
  (filter (lambda (p) (inside? (first p) (second p)))
          (list (list (sub1 gx) gy)
                (list (add1 gx) gy)
                (list gx (sub1 gy))
                (list gx (add1 gy)))))

(define (flood-from-enemies g enemies)
  (define seen (make-vector (* COLS ROWS) #f))
  (define (mark! gx gy) (vector-set! seen (idx gx gy) #t))
  (define (seen? gx gy) (vector-ref seen (idx gx gy)))
  (define seeds
    (filter values
            (map (lambda (e)
                   (define-values (gx gy) (enemy-cell e))
                   (and (inside? gx gy)
                        (eq? (grid-ref g gx gy) EMPTY)
                        (list gx gy)))
                 enemies)))
  (let loop ([front seeds])
    (cond
      [(empty? front) seen]
      [else
       (define p (first front))
       (define gx (first p))
       (define gy (second p))
       (if (or (seen? gx gy) (not (eq? (grid-ref g gx gy) EMPTY)))
           (loop (rest front))
           (begin
             (mark! gx gy)
             (loop (append (rest front) (neighbors gx gy)))))])))

(define (capture-closed-area g enemies)
  (define reachable (flood-from-enemies g enemies))
  (define ng (copy-grid g))
  (for* ([y (in-range ROWS)]
         [x (in-range COLS)])
    (define v (grid-ref g x y))
    (when (or (eq? v TRAIL)
              (and (eq? v EMPTY)
                   (not (vector-ref reachable (idx x y)))))
      (grid-set! ng x y CLAIMED)))
  ng)

(define (enemy-touches-trail? g enemies)
  (for/or ([e enemies])
    (define-values (gx gy) (enemy-cell e))
    (and (inside? gx gy) (eq? (grid-ref g gx gy) TRAIL))))

(define (enemy-touches-player? p enemies)
  (define px (cell-center (player-gx p)))
  (define py (cell-center (player-gy p)))
  (for/or ([e enemies])
    (< (sqrt (+ (sqr (- px (enemy-x e))) (sqr (- py (enemy-y e))))) 13)))

(define (lose w)
  (make-world (world-grid w) (world-player w) (world-enemies w) 'lost
              (world-claimed w) (world-ticks w)
              "Perdiste. Presiona espacio para reiniciar."))

(define (win w)
  (make-world (world-grid w) (world-player w) (world-enemies w) 'won
              (world-claimed w) (world-ticks w)
              "Ganaste. Presiona espacio para jugar otra vez."))

(define (move-player w)
  (define p (world-player w))
  (define g (world-grid w))
  (define-values (dx dy) (dir->delta (player-dir p)))
  (define nx (+ (player-gx p) dx))
  (define ny (+ (player-gy p) dy))
  (cond
    [(not (inside? nx ny)) w]
    [else
     (define target (grid-ref g nx ny))
     (cond
       [(eq? target TRAIL) (lose w)]
       [(eq? target EMPTY)
        (define ng (copy-grid g))
        (grid-set! ng nx ny TRAIL)
        (make-world ng (make-player nx ny (player-dir p) (player-dir p))
                    (world-enemies w) 'playing (world-claimed w)
                    (world-ticks w) "")]
       [(eq? target CLAIMED)
        (define entered-from-trail? (trail-exists? g))
        (define ng (if entered-from-trail?
                       (capture-closed-area g (world-enemies w))
                       g))
        (define pct (claimed-percent ng))
        (define nw
          (make-world ng (make-player nx ny (player-dir p) (player-dir p))
                      (world-enemies w) 'playing pct (world-ticks w) ""))
        (if (>= pct GOAL) (win nw) nw)]
       [else w])]))

(define (tick w)
  (cond
    [(not (eq? (world-state w) 'playing)) w]
    [else
     (define moved-player (move-player w))
     (if (eq? (world-state moved-player) 'lost)
         moved-player
         (let* ([new-enemies (move-enemies (world-grid moved-player)
                                           (world-enemies moved-player))]
                [nw (make-world (world-grid moved-player)
                                (world-player moved-player)
                                new-enemies
                                (world-state moved-player)
                                (world-claimed moved-player)
                                (add1 (world-ticks moved-player))
                                (world-message moved-player))])
           (if (or (enemy-touches-trail? (world-grid nw) new-enemies)
                   (enemy-touches-player? (world-player nw) new-enemies))
               (lose nw)
               nw)))]))

(define (handle-key w key)
  (cond
    [(and (member key (list " " "r" "R"))
          (not (eq? (world-state w) 'playing)))
     (new-game)]
    [(member key (list "left" "right" "up" "down"))
     (define new-dir (string->symbol key))
     (define p (world-player w))
     (if (opposite? new-dir (player-last-dir p))
         w
         (make-world (world-grid w)
                     (make-player (player-gx p) (player-gy p)
                                  new-dir (player-last-dir p))
                     (world-enemies w)
                     (world-state w)
                     (world-claimed w)
                     (world-ticks w)
                     (world-message w)))]
    [else w]))

(define empty-cell-img (rectangle CELL CELL "solid" (make-color 18 22 34)))
(define claimed-cell-img (rectangle CELL CELL "solid" (make-color 37 128 86)))
(define trail-cell-img (rectangle CELL CELL "solid" (make-color 248 213 82)))
(define grid-line-img (rectangle CELL CELL "outline" (make-color 34 42 58)))
(define player-img
  (overlay (circle 5 "solid" "black")
           (circle 8 "solid" (make-color 255 226 54))))
(define enemy-img
  (overlay (circle 5 "solid" "white")
           (circle 9 "solid" (make-color 230 54 70))))

(define (cell-img v)
  (overlay grid-line-img
           (cond [(eq? v EMPTY) empty-cell-img]
                 [(eq? v CLAIMED) claimed-cell-img]
                 [(eq? v TRAIL) trail-cell-img]
                 [else empty-cell-img])))

(define (draw-grid g)
  (for*/fold ([scene (empty-scene WIDTH HEIGHT (make-color 12 15 24))])
             ([y (in-range ROWS)]
              [x (in-range COLS)])
    (place-image (cell-img (grid-ref g x y))
                 (cell-center x) (cell-center y)
                 scene)))

(define (draw-enemies enemies scene)
  (foldl (lambda (e sc)
           (place-image enemy-img (enemy-x e) (enemy-y e) sc))
         scene
         enemies))

(define (hud w scene)
  (define pct-text
    (text (string-append "Territorio: "
                         (number->string (world-claimed w))
                         "% / "
                         (number->string GOAL)
                         "%")
          18 "white"))
  (define help-text
    (text "Flechas: mover | Espacio: reiniciar al terminar"
          14 (make-color 210 220 230)))
  (define msg
    (if (string=? (world-message w) "")
        empty-image
        (text (world-message w) 24 "white")))
  (define base
    (place-image pct-text 120 20
                 (place-image help-text (- WIDTH 195) 20 scene)))
  (if (eq? (world-state w) 'playing)
      base
      (place-image
       (overlay
        msg
        (rectangle 560 72 "solid"
                   (make-color 0 0 0 170)))
       (/ WIDTH 2) (/ HEIGHT 2) base)))

(define (draw w)
  (define p (world-player w))
  (define scene
    (draw-enemies
     (world-enemies w)
     (place-image player-img
                  (cell-center (player-gx p))
                  (cell-center (player-gy p))
                  (draw-grid (world-grid w)))))
  (hud w scene))

(big-bang (new-game)
  [to-draw draw]
  [on-tick tick TICK-RATE]
  [on-key handle-key])
