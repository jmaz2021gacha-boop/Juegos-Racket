(require 2htdp/universe)
(require 2htdp/image)

;; =========================
;; CONFIG
;; =========================

(define W 800)
(define H 400)

(define GRAVITY 0.5)
(define JUMP -10)
(define SPEED 4)

(define RADIO 15)
(define piso-y 340)

;; =========================
;; ESTADO
;; =========================

(define-struct estado (modo nivel x y vx vy ang mensaje tiempo))

(define inicial (make-estado "menu" 1 100 300 0 0 0 "" 0))

;; =========================
;; OBJETOS FIJOS
;; =========================

(define bandera
  (overlay/align "left" "top"
                 (rectangle 30 20 "solid" "red")
                 (rectangle 5 80 "solid" "black")))

(define poste-x 740)
(define poste-y 260)

;; =========================
;; FONDOS POR NIVEL
;; =========================

(define (fondo nivel)
  (cond
    [(= nivel 1)
     (rectangle W H "solid" (color 135 206 235))] ;; cielo

    [(= nivel 2)
     (rectangle W H "solid" (color 120 20 20))] ;; lava / oscuro

    [(= nivel 3)
     (rectangle W H "solid" (color 10 10 40))])) ;; futurista

;; =========================
;; OBSTÁCULOS
;; =========================

(define (obstaculos nivel)
  (cond
    [(= nivel 1)
     (list (list 300 330 "pincho" 40 20)
           (list 450 330 "pincho" 40 20)
           (list 600 330 "pincho" 40 20))]

    [(= nivel 2)
     (list (list 300 280 "pared" 60 120)
           (list 500 280 "pared" 60 120)
           (list 650 280 "pared" 60 120))]

    [(= nivel 3)
     (list
      (list 250 250 "plataforma" 120 20)
      (list 450 200 "plataforma" 120 20)
      (list 650 150 "plataforma" 120 20)

      (list 250 230 "pincho" 40 20)
      (list 450 180 "pincho" 40 20)
      (list 650 130 "pincho" 40 20))]))

;; =========================
;; COLISIONES
;; =========================

(define (colision? x y obs)
  (let* ([ox (first obs)]
         [oy (second obs)]
         [w (fourth obs)]
         [h (fifth obs)])
    (and (< (abs (- x ox)) (+ RADIO (/ w 2)))
         (< (abs (- y oy)) (+ RADIO (/ h 2))))))

(define (choca? e)
  (ormap
   (λ (obs)
     (let ([tipo (third obs)])
       (and (member tipo (list "pincho" "pared"))
            (colision? (estado-x e) (estado-y e) obs))))
   (obstaculos (estado-nivel e))))

;; =========================
;; PLATAFORMAS
;; =========================

(define (plataforma? obs)
  (string=? (third obs) "plataforma"))

(define (en-plataforma e)
  (ormap
   (λ (obs)
     (if (plataforma? obs)
         (let* ([ox (first obs)]
                [oy (second obs)]
                [w (fourth obs)])
           (and (< (abs (- (estado-x e) ox)) (/ w 2))
                (< (abs (- (- (estado-y e) RADIO) oy)) 10)))
         #f))
   (obstaculos (estado-nivel e))))

;; =========================
;; FÍSICA
;; =========================

(define (fisica e)
  (let* ([vy2 (+ (estado-vy e) GRAVITY)]
         [y2 (+ (estado-y e) vy2)]
         [x2 (+ (estado-x e) (estado-vx e))]
         [ang2 (+ (estado-ang e) (estado-vx e))])

    (cond
      [(> y2 piso-y)
       (make-estado "juego" (estado-nivel e) x2 piso-y (estado-vx e) 0 ang2 (estado-mensaje e) (estado-tiempo e))]

      [(en-plataforma e)
       (make-estado "juego" (estado-nivel e) x2 (estado-y e) (estado-vx e) 0 ang2 (estado-mensaje e) (estado-tiempo e))]

      [else
       (make-estado "juego" (estado-nivel e) x2 y2 (estado-vx e) vy2 ang2 (estado-mensaje e) (estado-tiempo e))])))

;; =========================
;; LÓGICA
;; =========================

(define (reset e)
  (make-estado "juego" (estado-nivel e) 100 300 0 0 0 "¡Chocaste!" 40))

(define (gano? e)
  (and (> (estado-x e) poste-x)
       (> (estado-y e) poste-y)
       (< (estado-y e) (+ poste-y 80))))

(define (next e)
  (cond
    [(= (estado-nivel e) 1)
     (make-estado "juego" 2 100 300 0 0 0 "Pasaste al nivel 2" 60)]

    [(= (estado-nivel e) 2)
     (make-estado "juego" 3 100 300 0 0 0 "Pasaste al nivel 3" 60)]

    [else
     (make-estado "menu" 1 100 300 0 0 0 "🎉 GANASTE TODO 🎉" 200)]))

;; =========================
;; TECLADO
;; =========================

(define (key e k)
  (cond
    [(string=? (estado-modo e) "menu")
     (cond
       [(key=? k "1") (make-estado "juego" 1 100 300 0 0 0 "" 0)]
       [(key=? k "2") (make-estado "juego" 2 100 300 0 0 0 "" 0)]
       [(key=? k "3") (make-estado "juego" 3 100 300 0 0 0 "" 0)]
       [else e])]

    [(string=? (estado-modo e) "juego")
     (cond
       [(key=? k "right")
        (make-estado "juego" (estado-nivel e)
                     (estado-x e) (estado-y e)
                     SPEED (estado-vy e)
                     (estado-ang e)
                     (estado-mensaje e)
                     (estado-tiempo e))]

       [(key=? k "left")
        (make-estado "juego" (estado-nivel e)
                     (estado-x e) (estado-y e)
                     (- SPEED) (estado-vy e)
                     (estado-ang e)
                     (estado-mensaje e)
                     (estado-tiempo e))]

       [(key=? k "up")
        (if (= (estado-vy e) 0)
            (make-estado "juego" (estado-nivel e)
                         (estado-x e) (estado-y e)
                         (estado-vx e) JUMP
                         (estado-ang e)
                         (estado-mensaje e)
                         (estado-tiempo e))
            e)]

       [else e])]

    [else e]))

;; =========================
;; SOLTAR TECLA
;; =========================

(define (key-release e k)
  (if (string=? (estado-modo e) "juego")
      (cond
        [(or (key=? k "right") (key=? k "left"))
         (make-estado "juego"
                      (estado-nivel e)
                      (estado-x e)
                      (estado-y e)
                      0
                      (estado-vy e)
                      (estado-ang e)
                      (estado-mensaje e)
                      (estado-tiempo e))]
        [else e])
      e))

;; =========================
;; TICK
;; =========================

(define (tick e)
  (if (string=? (estado-modo e) "juego")
      (let ([e2 (fisica e)])
        (cond
          [(choca? e2) (reset e2)]
          [(gano? e2) (next e2)]
          [else e2]))
      e))

;; =========================
;; OBSTÁCULOS DIBUJO
;; =========================

(define (dibujar-obstaculos lista escena)
  (foldl
   (λ (obs scn)
     (let* ([x (first obs)]
            [y (second obs)]
            [tipo (third obs)]
            [w (fourth obs)]
            [h (fifth obs)]
            [img (cond
                   [(string=? tipo "pincho") (triangle 20 "solid" "red")]
                   [(string=? tipo "pared") (rectangle w h "solid" "brown")]
                   [(string=? tipo "plataforma") (rectangle w h "solid" "gray")])])
       (place-image img x y scn)))
   escena lista))

;; =========================
;; BOLA CON EFECTO ROTACIÓN
;; =========================

(define (bola e)
  (overlay
   (rotate (estado-ang e)
           (line (* RADIO 2) 0 "black"))
   (circle RADIO "solid" "red")))

;; =========================
;; DIBUJO FINAL
;; =========================

(define (draw e)
  (cond
    [(string=? (estado-modo e) "menu")
     (place-image
      (text "RED BALL\n1-2-3 niveles\nCreado por Jacobo Peña Serna y Jacobo Marín Nieto"
            18 "black")
      400 200
      (fondo 1))]

    [else
     (let* ([escena (fondo (estado-nivel e))]
            [escena1 (place-image (rectangle W 40 "solid" "darkgreen") 400 360 escena)]
            [escena2 (dibujar-obstaculos (obstaculos (estado-nivel e)) escena1)]
            [escena3 (place-image bandera poste-x poste-y escena2)]
            [escena4 (place-image (bola e)
                                  (estado-x e)
                                  (estado-y e)
                                  escena3)])

       escena4)]))

;; =========================
;; RUN
;; =========================

(big-bang inicial
  [on-tick tick]
  [on-key key]
  [on-release key-release]
  [to-draw draw])