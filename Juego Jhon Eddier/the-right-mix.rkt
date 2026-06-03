#lang racket
(provide iniciar-mix)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define WIDTH 800)
(define HEIGHT 600)
(define MAX-VOL 200)
(define MAX-SHAKE 100) 

(define C-VODKA "ghostwhite")
(define C-JUGO "orange")
(define C-VENENO "darkgreen")
(define C-CURACAO "cyan")
(define C-KAHLUA "saddlebrown")

(define BTN-BG (ellipse 120 50 "solid" "gold"))
(define BTN-POUR (overlay (text "POUR" 20 "black") BTN-BG))
(define BTN-SHAKE (overlay (text "SHAKE" 20 "black") BTN-BG))
(define BTN-SERVE (overlay (text "SERVE" 20 "black") BTN-BG))

(define-struct state (v j p c k sel action shake-lvl phase score))

(define (handle-tick ws)
  (cond
    [(string=? (state-phase ws) "bar")
     (cond
       [(string=? (state-action ws) "pour")
        (let ([total (+ (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws))])
          (if (< total MAX-VOL)
              (add-to-selected ws 2) 
              ws))]
       [(string=? (state-action ws) "shake")
        (if (> (state-shake-lvl ws) MAX-SHAKE)
            (make-state (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws)
                        (state-sel ws) "none" (state-shake-lvl ws) "boom" 0)
            (make-state (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws)
                        (state-sel ws) "shake" (add1 (state-shake-lvl ws)) "bar" 0))]
       [else ws])]
    [else ws]))

(define (add-to-selected ws amt)
  (let ([s (state-sel ws)])
    (make-state
     (if (= s 1) (+ (state-v ws) amt) (state-v ws))
     (if (= s 2) (+ (state-j ws) amt) (state-j ws))
     (if (= s 3) (+ (state-p ws) amt) (state-p ws))
     (if (= s 4) (+ (state-c ws) amt) (state-c ws))
     (if (= s 5) (+ (state-k ws) amt) (state-k ws))
     s (state-action ws) (state-shake-lvl ws) (state-phase ws) 0)))

(define (handle-mouse ws x y event)
  (cond
    [(string=? (state-phase ws) "bar")
     (cond
       [(string=? event "button-down")
        (cond
          [(and (> y 80) (< y 220))
           (cond
             [(and (> x 70) (< x 130))  (set-sel ws 1)]
             [(and (> x 220) (< x 280)) (set-sel ws 2)]
             [(and (> x 370) (< x 430)) (set-sel ws 3)]
             [(and (> x 520) (< x 580)) (set-sel ws 4)]
             [(and (> x 670) (< x 730)) (set-sel ws 5)]
             [else ws])]
          [(and (> y 470) (< y 530))
           (cond
             [(and (> x 140) (< x 260)) (set-act ws "pour")]
             [(and (> x 340) (< x 460)) (set-act ws "shake")]
             [(and (> x 540) (< x 660)) (serve ws)] 
             [else ws])]
          [else ws])]
       [(string=? event "button-up")
        (set-act ws "none")]
       [else ws])]
    [(string=? event "button-down")
     (make-state 0 0 0 0 0 1 "none" 0 "bar" 0)]
    [else ws]))

(define (set-sel ws n)
  (make-state (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws)
              n (state-action ws) (state-shake-lvl ws) "bar" 0))

(define (set-act ws act)
  (make-state (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws)
              (state-sel ws) act (state-shake-lvl ws) "bar" 0))

(define (serve ws)
  (make-state (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws)
              (state-sel ws) "none" (state-shake-lvl ws) "result" (calc-score ws)))

(define (calc-score ws)
  (let* ([total (+ (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws))]
         [shake (state-shake-lvl ws)])
    (if (= total 0)
        0
        (let* ([diff (abs (- (state-v ws) (state-j ws)))]
               [toxins (+ (* 2 (state-p ws)) (state-k ws))]
               [base-score (- 100 diff toxins)])
          (if (and (> shake 30) (< shake MAX-SHAKE))
              (max 0 (min 100 (inexact->exact (round base-score))))
              (max 0 (min 100 (inexact->exact (round (- base-score 40))))))))))

(define (render ws)
  (cond
    [(string=? (state-phase ws) "bar") (draw-bar ws)]
    [(string=? (state-phase ws) "boom") (draw-boom ws)]
    [(string=? (state-phase ws) "result") (draw-result ws)]))

(define (draw-bar ws)
  (let* ([shaking? (string=? (state-action ws) "shake")]
         [ox (if shaking? (- (random 10) 5) 0)]
         [oy (if shaking? (- (random 10) 5) 0)])
    (place-images
     (list (draw-bottle C-VODKA   (= (state-sel ws) 1))
           (draw-bottle C-JUGO    (= (state-sel ws) 2))
           (draw-bottle C-VENENO  (= (state-sel ws) 3))
           (draw-bottle C-CURACAO (= (state-sel ws) 4))
           (draw-bottle C-KAHLUA  (= (state-sel ws) 5))
           (draw-glass ws)
           BTN-POUR BTN-SHAKE BTN-SERVE)
     (list (make-posn (+ 100 ox) (+ 150 oy))
           (make-posn (+ 250 ox) (+ 150 oy))
           (make-posn (+ 400 ox) (+ 150 oy))
           (make-posn (+ 550 ox) (+ 150 oy))
           (make-posn (+ 700 ox) (+ 150 oy))
           (make-posn (+ 400 ox) (+ 350 oy)) 
           (make-posn 200 500)
           (make-posn 400 500)
           (make-posn 600 500))
     (empty-scene WIDTH HEIGHT "darkred"))))

(define (draw-bottle color sel?)
  (overlay
   (rectangle 40 130 "solid" color)
   (rectangle 50 140 (if sel? "solid" "outline") (if sel? "yellow" "transparent"))))

(define (draw-glass ws)
  (above/align "center"
   (rectangle 80 (- MAX-VOL (+ (state-v ws) (state-j ws) (state-p ws) (state-c ws) (state-k ws))) "solid" "transparent")
   (rectangle 80 (state-v ws) "solid" C-VODKA)
   (rectangle 80 (state-j ws) "solid" C-JUGO)
   (rectangle 80 (state-p ws) "solid" C-VENENO)
   (rectangle 80 (state-c ws) "solid" C-CURACAO)
   (rectangle 80 (state-k ws) "solid" C-KAHLUA)))

(define (draw-boom ws)
  (place-images
   (list (text "¡BOOM!" 80 "red")
         (text "Agitaste demasiado y la coctelera explotó." 30 "white")
         (text "Haz clic en cualquier lugar para reintentar" 20 "yellow"))
   (list (make-posn 400 200) (make-posn 400 300) (make-posn 400 400))
   (empty-scene WIDTH HEIGHT "black")))

(define (draw-result ws)
  (place-images
   (list (text "¡AQUÍ TIENES!" 60 "white")
         (text (string-append "Puntuación: " (number->string (state-score ws)) " / 100") 40 "yellow")
         (text "Haz clic en cualquier lugar para reintentar" 20 "white"))
   (list (make-posn 400 200) (make-posn 400 300) (make-posn 400 400))
   (empty-scene WIDTH HEIGHT "black")))

(define (main)
  (big-bang (make-state 0 0 0 0 0 1 "none" 0 "bar" 0)
    (on-tick handle-tick 0.05)
    (on-mouse handle-mouse)
    (to-draw render)))

(define (iniciar-mix)
  (main))