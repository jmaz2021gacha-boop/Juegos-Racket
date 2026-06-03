#lang racket
(require (lib "graphics.ss" "graphics"))

(open-graphics)
; La ventana directa, sin constantes previas
(define w (open-viewport "Juego de Tiro - Version Novato" 1000 600))

; Variables globales para todo
(define pos-ini-x 250)
(define pos-ini-y 435) ; 600 - 150 (suelo) - 15 (radio)
(define arco-x 720)
(define arco-y 240)
(define obs-x -100)
(define obs-y -100)
(define obs-ancho 0)
(define obs-alto 0)

; Función gigante para dibujar todo junto sin mucho orden
(define (DibujarFondo)
  (begin
    ; Cielo
    ((draw-solid-rectangle w) (make-posn 0 0) 1000 450 "steelblue")
    ; Suelo
    ((draw-solid-rectangle w) (make-posn 0 450) 1000 150 "white")
    ; Obstaculo (nivel 2 y 3)
    (if (> obs-x 0)
        ((draw-solid-rectangle w) (make-posn obs-x obs-y) obs-ancho obs-alto "white")
        (void))
    ; Caja (hecho con 3 rectangulos a mano)
    ((draw-solid-rectangle w) (make-posn arco-x arco-y) 6 75 "white")
    ((draw-solid-rectangle w) (make-posn arco-x (+ arco-y 75)) 96 6 "white")
    ((draw-solid-rectangle w) (make-posn (+ arco-x 90) arco-y) 6 81 "white")))


; Puntos de trayectoria
(define (DibujarPuntitos px py vx vy contador)
  (if (<= contador 45)
      (if (< py 450)   ; Mientras no toque el suelo
          (begin
            (if (= (modulo contador 3) 0)
                ((draw-solid-ellipse w) (make-posn (- px 4) (- py 4)) 8 8 "white")
                (void))
            (DibujarPuntitos (+ px vx) (+ py vy) vx (+ vy 0.5) (+ contador 1)))
          (void))
      (void)))


(define (MoverPelota px py vx vy rebotes)
  (begin
    (DibujarFondo)
    ((draw-solid-ellipse w) (make-posn (- px 15) (- py 15)) 30 30 "white")
    (sleep 0.015) 
    
    (cond
      ; 1. GANAR
      ((and (> vy 0) (<= py arco-y) (>= (+ py vy) arco-y) (> px arco-x) (< px (+ arco-x 90)))
       (begin
         (display "¡GANASTE!\n")
         ((draw-solid-rectangle w) (make-posn arco-x arco-y) 96 81 "blue")
         (sleep 1.0)
         (viewport-flush-input w) 
         (PantallaNiveles)))
         
      ; 2. FUERA DE LIMITES
      ((or (< px -15) (> px 1015) (> py 615))
       (begin
         (display "Se salio de la ventana, intenta otra vez\n")
         (viewport-flush-input w)
         (EmpezarJuego)))
      
      ; 3. COLISION OBSTACULO
      ((and (> px obs-x) (< px (+ obs-x obs-ancho)) (> py obs-y) (< py (+ obs-y obs-alto)))
       (MoverPelota (- px vx) (- py vy) (* vx -1) (* vy -1) rebotes))
       
      ; 4. COLISION ARCO
      ((and (or (and (> px arco-x) (< px (+ arco-x 6))) 
                (and (> px (+ arco-x 90)) (< px (+ arco-x 96))))
            (> py arco-y) (< py (+ arco-y 75)))
       (MoverPelota (- px vx) (+ py vy) (* vx -0.65) vy rebotes))

      ; 5. REBOTE SUELO
      ((and (>= (+ py vy) 435) (> vy 0))
       (if (< rebotes 9)
           (MoverPelota (+ px vx) 435 vx (* vy -0.65) (+ rebotes 1))
           (begin 
             (display "Muchos rebotes!\n") 
             (viewport-flush-input w) 
             (EmpezarJuego))))
      
      ; 6. MOVER NORMAL
      (else 
       (MoverPelota (+ px vx) (+ py vy) vx (+ vy 0.5) rebotes)))))



(define (PantallaNiveles)
  (begin
    ((draw-solid-rectangle w) (make-posn 0 0) 1000 600 "lightgray")
    ((draw-string w) (make-posn 400 180) "Elige un nivel:")
    ((draw-solid-rectangle w) (make-posn 250 250) 100 50 "white")
    ((draw-string w) (make-posn 270 280) "NIVEL 1")
    ((draw-solid-rectangle w) (make-posn 450 250) 100 50 "white")
    ((draw-string w) (make-posn 470 280) "NIVEL 2")
    ((draw-solid-rectangle w) (make-posn 650 250) 100 50 "white")
    ((draw-string w) (make-posn 670 280) "NIVEL 3")
    (ClickMenu (get-mouse-click w))))

(define (ClickMenu c)
  (if (left-mouse-click? c)
      (begin
        (cond
          ; Nivel 1
          ((and (>= (posn-x (mouse-click-posn c)) 250) (<= (posn-x (mouse-click-posn c)) 350) 
                (>= (posn-y (mouse-click-posn c)) 250) (<= (posn-y (mouse-click-posn c)) 300))
           (begin 
             (set! pos-ini-x 250) (set! arco-x 720) (set! arco-y 240)
             (set! obs-x -100) (set! obs-ancho 0) (set! obs-alto 0)
             (viewport-flush-input w) (EmpezarJuego)))
          ; Nivel 2
          ((and (>= (posn-x (mouse-click-posn c)) 450) (<= (posn-x (mouse-click-posn c)) 550) 
                (>= (posn-y (mouse-click-posn c)) 250) (<= (posn-y (mouse-click-posn c)) 300))
           (begin 
             (set! pos-ini-x 750) (set! arco-x 220) (set! arco-y 300)
             (set! obs-x 425) (set! obs-y 250) (set! obs-ancho 150) (set! obs-alto 350)
             (viewport-flush-input w) (EmpezarJuego)))
          ; Nivel 3
          ((and (>= (posn-x (mouse-click-posn c)) 650) (<= (posn-x (mouse-click-posn c)) 750) 
                (>= (posn-y (mouse-click-posn c)) 250) (<= (posn-y (mouse-click-posn c)) 300))
           (begin 
             (set! pos-ini-x 800) (set! arco-x 270) (set! arco-y 150)
             (set! obs-x 430) (set! obs-y 200) (set! obs-ancho 110) (set! obs-alto 250)
             (viewport-flush-input w) (EmpezarJuego)))
          (else (PantallaNiveles))))
      (PantallaNiveles)))


(define (ArrastrarRatita)
  (if (not (ready-mouse-release w))
      (begin
        (DibujarFondo)
        (if (query-mouse-posn w)
            (DibujarPuntitos pos-ini-x pos-ini-y 
                             (* (- pos-ini-x (posn-x (query-mouse-posn w))) 0.15) 
                             (* (- pos-ini-y (posn-y (query-mouse-posn w))) 0.15) 1)
            (void))
        ((draw-solid-ellipse w) (make-posn (- pos-ini-x 15) (- pos-ini-y 15)) 30 30 "white")
        (sleep 0.03)
        (ArrastrarRatita))
      ; Disparar
      (if (query-mouse-posn w)
          (MoverPelota pos-ini-x pos-ini-y 
                       (* (- pos-ini-x (posn-x (query-mouse-posn w))) 0.15) 
                       (* (- pos-ini-y (posn-y (query-mouse-posn w))) 0.15) 0)
          (EmpezarJuego))))

(define (EmpezarJuego)
  (begin
    (DibujarFondo)
    ((draw-solid-ellipse w) (make-posn (- pos-ini-x 15) (- pos-ini-y 15)) 30 30 "white")
    (let-click (get-mouse-click w))))


(define (let-click c)
  (if (left-mouse-click? c)
      (if (<= (sqrt (+ (* (- (posn-x (mouse-click-posn c)) pos-ini-x) (- (posn-x (mouse-click-posn c)) pos-ini-x))
                       (* (- (posn-y (mouse-click-posn c)) pos-ini-y) (- (posn-y (mouse-click-posn c)) pos-ini-y)))) 40)
          (begin (viewport-flush-input w) (ArrastrarRatita))
          (EmpezarJuego))
      (EmpezarJuego)))


