#lang racket
(require 2htdp/universe
         2htdp/image
         rsound)

(define main-theme (rs-read "main-theme.wav"))
(define piggies-theme (rs-read "bad-piggies.wav"))
(define celebration (rs-read "victory.wav"))
(define rio2 (rs-read "rio2.wav"))
(define go (rs-read "go.wav"))
(define starw (rs-read "star.wav"))
(define bird-launch (rs-read "bird-launch.wav"))
(define pig-sound (rs-read "pig-sound.wav"))
;; Elementos gráficos
(define bird   (scale 0.07 (bitmap "red.png")))
(define sling  (scale 0.3 (bitmap "resor.png")))
(define block1  (rectangle 20 10 "solid" "brown"))
(define block2  (rectangle 90 10 "solid" "brown"))
(define block3  (rectangle 10 40 "solid" "brown"))
(define rock  (rectangle 15 20 "solid" "grey"))
(define rock1  (rectangle 50 50 "solid" "grey"))
(define rock2  (rectangle 60 100 "solid" "grey"))
(define rock3  (rectangle 20 90 "solid" "grey"))
(define glass  (rectangle 12 50 "outline" "lightblue"))
(define glass1  (rectangle 30 12  "outline" "lightblue"))
(define pig    (scale 0.05 (bitmap "pig.png")))
(define tile (bitmap "piso1.png"))
(define ground (beside tile tile tile))
(define csp (scale 0.5 (bitmap "cesped.png")))
(define hrb (scale 0.4 (bitmap "hierba.png")))
(define cesped (beside csp csp csp csp csp csp csp))
(define hierba (beside hrb hrb hrb hrb hrb hrb hrb))
(define cielo1 (bitmap "cielo1.png"))
(define tronco1 (scale 0.5 (bitmap "tronco1.png")))
(define arbol1 (bitmap "arbol1.png"))
(define fondo1 (place-image tronco1 450 240
                            (place-image tronco1 900 240 (place-image arbol1 450 280
                                                                      (place-image arbol1 900 280  (beside cielo1 cielo1 cielo1))))))
(define cielo2 (scale 1.3 (bitmap "fondo2.png")))
(define fondo2 (beside cielo2 cielo2 cielo2))
(define cielo3 (scale 1.55 (bitmap "fondo3.png")))
(define fondo3 (place-image tronco1 450 300
                            (place-image tronco1 900 300 (place-image arbol1 450 330
                                                                      (place-image arbol1 900 330 (beside cielo3 cielo3))))))
(define carga (scale 0.75 (bitmap "carga.png")))
(define fnivel (scale 0.11 (bitmap "fnivel.png")))

;; Botones
(define (boton texto)
  (overlay (text texto 20 "white")
           (rectangle 200 40 "solid" "blue")))

(define boton-menu    (boton "Menú de niveles"))
(define boton-restart (boton "Reiniciar nivel"))
(define boton-exit    (boton "Cerrar juego"))

(define-struct posn (x y))
(define-struct world
  (x y vx vy objs score birds-left angle power launched ready estado nivel menu? timer))

(define boton-nivel
  (overlay
   (text "≡" 25 "white")
   (circle 15 "solid" "gold")))

(define linea-menu
  (rectangle 5 10 "solid" "white"))
(define linea-m
  (rectangle 5 2 "solid" "gold"))

(define icono-menu
  (beside linea-menu linea-m linea-menu))

(define boton-menu-top
  (overlay
   icono-menu
   (circle 15 "solid" "gold")))

(define icono-reset
  (circle 10 "outline" "white"))

(define boton-reset
  (overlay
   icono-reset
   (circle 15 "solid" "gold")))

(define triangulo
  (triangle 12 "solid" "white"))

(define icono-next
  (beside triangulo triangulo))

(define boton-next2
  (overlay
   icono-next
   (circle 15 "solid" "gold")))

(define icono-close
  (text "X" 20 "white"))

(define boton-close
  (overlay
   icono-close
   (circle 15 "solid" "gold")))

(define (fondo-nivel nivel)
  (cond
    [(= nivel 1) fondo1]
    [(= nivel 2) fondo2]
    [(= nivel 3) fondo3]
    [else fondo1]))

;; Escenarios (niveles)
(define escenario1
  (list (list 'sling (make-posn 100 300))
        (list 'block1 (make-posn 750 235))
        (list 'block2 (make-posn 750 247))
        (list 'block3 (make-posn 730 221))
        (list 'block3 (make-posn 770 221))
        (list 'block3 (make-posn 730 180))
        (list 'block3 (make-posn 770 180))
        (list 'block1 (make-posn 740 154))
        (list 'block1 (make-posn 761 154))
        (list 'block3 (make-posn 750 128))
        (list 'glass  (make-posn 740 298))
        (list 'glass  (make-posn 760 298))
        (list 'pig    (make-posn 750 220))
        (list 'block3 (make-posn 710 273))
        (list 'block3 (make-posn 790 273))
        (list 'block3 (make-posn 710 314))
        (list 'block3 (make-posn 790 314))
        (list 'block2 (make-posn 710 340))
        (list 'block2 (make-posn 790 340))
        (list 'block1 (make-posn 685 345))
        (list 'block1 (make-posn 825 345))
        (list 'block1 (make-posn 750 329))
        (list 'block1 (make-posn 750 267))
        ))

(define escenario2
  (list (list 'sling (make-posn 100 300))
        (list 'block2 (make-posn 850 340))
        (list 'block3 (make-posn 825 320))
        (list 'block3 (make-posn 875 320))
        (list 'rock   (make-posn 800 340))
        (list 'rock   (make-posn 910 340))
        (list 'glass1 (make-posn 825 291))
        (list 'glass1 (make-posn 875 291))
        (list 'glass1 (make-posn 850 276))
        (list 'glass  (make-posn 850 310))
        (list 'glass  (make-posn 825 254))
        (list 'glass  (make-posn 875 254))
        (list 'glass1 (make-posn 825 221))
        (list 'glass1 (make-posn 875 221))
        (list 'pig    (make-posn 850 255))
        (list 'pig    (make-posn 825 200))
        (list 'pig    (make-posn 875 200))))
      
      
(define escenario3
  (list (list 'sling (make-posn 100 300))
        (list 'block1 (make-posn 500 340))
        (list 'block1 (make-posn 540 340))
        (list 'glass (make-posn 520 320))
        (list 'pig   (make-posn 520 280))
        (list 'rock   (make-posn 490 340))
        (list 'rock1   (make-posn 450 230))
        (list 'rock1   (make-posn 600 230))
        (list 'rock2   (make-posn 600 300))
        (list 'rock2   (make-posn 450 300))
        (list 'rock3   (make-posn 600 190))))

;; Estado inicial del mundo
(define (nuevo-mundo nivel)

  (define escenario
    (cond
      [(= nivel 1) escenario1]
      [(= nivel 2) escenario2]
      [else escenario3]))

  (make-world  0 0 0 0 escenario  0  4 45  8  #f  #f  "jugando" nivel #f 0))
(define initial-world (nuevo-mundo 1))

;; Colisiones
(define (colision? w obj)
  (and (member (first obj)  '(pig block1 block2 block3 rock rock1 rock2 rock3 glass glass1))
       (< (abs (- (world-x w) (posn-x (second obj)))) 20)
       (< (abs (- (world-y w) (posn-y (second obj)))) 20)))

(define (puntaje obj)
  (match (first obj)
    ['pig 5000]
    ['block1 500]
    ['block2 550]
    ['block3 600]
    ['rock 700]
    ['rock1 800]
    ['rock2 900]
    ['rock3 930]
    ['glass 300]
    ['glass1 320]
    [_ 0]))

;; Actualización física
(define (update w)
  (cond

    ;; =====================
    ;; CUENTA REGRESIVA VICTORIA
    ;; =====================

    [(equal? (world-estado w) "victoria")

     (if (> (world-timer w) 0)

         (make-world
          (world-x w)
          (world-y w)
          (world-vx w)
          (world-vy w)
          (world-objs w)
          (world-score w)
          (world-birds-left w)
          (world-angle w)
          (world-power w)
          (world-launched w)
          (world-ready w)
          "victoria"
          (world-nivel w)
          (world-menu? w)
          (- (world-timer w) 1))

         (make-world
          (world-x w)
          (world-y w)
          (world-vx w)
          (world-vy w)
          (world-objs w)
          (world-score w)
          (world-birds-left w)
          (world-angle w)
          (world-power w)
          #f
          (world-ready w)
          "final"
          (world-nivel w)
          (world-menu? w)
          0))]

    ;; =====================
    ;; JUEGO NORMAL
    ;; =====================

    [(and (world-launched w)
          (equal? (world-estado w) "jugando"))

     (let* ([objetos-golpeados
             (filter (λ (obj) (colision? w obj))
                     (world-objs w))]

            [nuevos-objs
             (filter (λ (obj) (not (colision? w obj)))
                     (world-objs w))]

            [nuevo-score
             (+ (world-score w)
                (apply + (map puntaje objetos-golpeados)))]

            [pigs-restantes
             (filter (λ (obj)
                       (equal? (first obj) 'pig))
                     nuevos-objs)])

       (cond

         [(empty? pigs-restantes)
          (play celebration)

          (make-world
           (world-x w)
           (world-y w)
           (world-vx w)
           (world-vy w)
           nuevos-objs
           (+ nuevo-score
              (* (world-birds-left w) 10000))
           (world-birds-left w)
           (world-angle w)
           (world-power w)
           #f
           (world-ready w)
           "victoria"
           (world-nivel w)
           (world-menu? w)
           120)]

         [(and (< (world-y w) 0)
               (<= (world-birds-left w) 1))

          (make-world
           (world-x w)
           (world-y w)
           (world-vx w)
           (world-vy w)
           nuevos-objs
           nuevo-score
           0
           (world-angle w)
           (world-power w)
           #f
           (world-ready w)
           "final"
           (world-nivel w)
           (world-menu? w)
           0)]

         [(and (< (world-y w) 0)
               (> (world-birds-left w) 1))

          (make-world
           0 0 0 0
           nuevos-objs
           nuevo-score
           (- (world-birds-left w) 1)
           45
           8
           #f
           #f
           "jugando"
           (world-nivel w)
           (world-menu? w)
           0)]

         [else

          (make-world
           (+ (world-x w) (world-vx w))
           (+ (world-y w) (world-vy w))
           (world-vx w)
           (+ (world-vy w) 1)
           nuevos-objs
           nuevo-score
           (world-birds-left w)
           (world-angle w)
           (world-power w)
           #t
           (world-ready w)
           "jugando"
           (world-nivel w)
           (world-menu? w)
           0)]))]

    ;; =====================
    ;; CUALQUIER OTRO CASO
    ;; =====================

    [else w]))

;; Control con teclado
(define (control w key)
  (cond
    [(and (key=? key "c") (equal? (world-estado w) "jugando"))
     (make-world 112 270 0 0 (world-objs w) (world-score w) (world-birds-left w)
                 (world-angle w) (world-power w) #f #t "jugando" (world-nivel w) (world-menu? w) 0)]
    [(key=? key "up")   (make-world (world-x w) (world-y w) (world-vx w) (world-vy w)
                                    (world-objs w) (world-score w) (world-birds-left w)
                                    (+ (world-angle w) 5) (world-power w) (world-launched w) (world-ready w) (world-estado w) (world-nivel w) (world-menu? w) 0)]
    [(key=? key "down") (make-world (world-x w) (world-y w) (world-vx w) (world-vy w)
                                    (world-objs w) (world-score w) (world-birds-left w)
                                    (- (world-angle w) 5) (world-power w) (world-launched w) (world-ready w) (world-estado w) (world-nivel w) (world-menu? w) 0)]
    [(key=? key "right")(make-world (world-x w) (world-y w) (world-vx w) (world-vy w)
                                    (world-objs w) (world-score w) (world-birds-left w)
                                    (world-angle w) (+ (world-power w) 1) (world-launched w) (world-ready w) (world-estado w) (world-nivel w) (world-menu? w) 0)]
    [(key=? key "left") (make-world (world-x w) (world-y w) (world-vx w) (world-vy w)
                                    (world-objs w) (world-score w) (world-birds-left w)
                                    (world-angle w) (max 1 (- (world-power w) 1)) (world-launched w) (world-ready w) (world-estado w) (world-nivel w) (world-menu? w) 0)]
    [(and (key=? key " ") (world-ready w) (equal? (world-estado w) "jugando"))
     (begin
       (play bird-launch) (play pig-sound)
       (make-world (world-x w) (world-y w)
                   (* (world-power w) (cos (/ (* pi (world-angle w)) 180)))
                   (* (world-power w) (- (sin (/ (* pi (world-angle w)) 180))))
                   (world-objs w) (world-score w) (world-birds-left w)
                   (world-angle w) (world-power w) #t (world-ready w) "jugando" (world-nivel w) (world-menu? w) 0))]
    [else w]))

;; Manejo de clics en pantalla final
(stop)(play main-theme)
(define (mouse w x y event)
  (displayln (list x y))

  (if (string=? event "button-down")
      (cond
        [(and (equal? (world-estado w) "jugando")
              (> x 15) (< x 65)
              (> y 15) (< y 65))
         (make-world
          (world-x w)
          (world-y w)
          (world-vx w)
          (world-vy w)
          (world-objs w)
          (world-score w)
          (world-birds-left w)
          (world-angle w)
          (world-power w)
          (world-launched w)
          (world-ready w)
          (world-estado w)
          (world-nivel w)
          (not (world-menu? w)) 0)]
        ;; CERRAR JUEGO
        [(and (world-menu? w)
              (> x 165) (< x 190)
              (> y 5) (< y 35))
         (begin
           (stop)
           (make-world
            (world-x w)
            (world-y w)
            (world-vx w)
            (world-vy w)
            (world-objs w)
            (world-score w)
            (world-birds-left w)
            (world-angle w)
            (world-power w)
            #f
            #f
            "cerrar"
            (world-nivel w)
            #f 0))]
        ;; next
        [(and (world-menu? w)
              (> x 85) (< x 115)
              (> y 190) (< y 210))
         (nuevo-mundo (+ (world-nivel w) 1))]

        ;; reset
        [(and (world-menu? w)
              (> x 85) (< x 115)
              (> y 90) (< y 110))
         (begin  (displayln "RESET")
                 (nuevo-mundo (world-nivel w)))]
        ;; menu niveles
        [(and (world-menu? w)
              (> x 85) (< x 115)
              (> y 290) (< y 310))
         (begin (stop) (play rio2)
                (make-world
                 (world-x w)
                 (world-y w)
                 (world-vx w)
                 (world-vy w)
                 (world-objs w)
                 (world-score w)
                 (world-birds-left w)
                 (world-angle w)
                 (world-power w)
                 #f
                 #f
                 "menu"
                 (world-nivel w)
                 #f 0))]

        [(and (equal? (world-estado w) "final")
              (> x 345) (< x 415)
              (> y 295) (< y 365))
         (begin (stop) (play rio2)
                ;; abrir menú
                (make-world
                 (world-x w)
                 (world-y w)
                 (world-vx w)
                 (world-vy w)
                 (world-objs w)
                 (world-score w)
                 (world-birds-left w)
                 (world-angle w)
                 (world-power w)
                 #f
                 #f
                 "menu"
                 (world-nivel w) (world-menu? w) 0
                 ))]

        [(and (equal? (world-estado w) "final")
              (> x 465) (< x 535)
              (> y 295) (< y 365))
         (nuevo-mundo (world-nivel w))]

        [(and (equal? (world-estado w) "final")
              (> x 585) (< x 655)
              (> y 295) (< y 365))
         (nuevo-mundo (+ 1 (world-nivel w)))]

        [(and (equal? (world-estado w) "menu")
              (> x 260) (< x 340)
              (> y 160) (< y 240))
         (begin (stop) (play piggies-theme)
                (nuevo-mundo 1))]

        [(and (equal? (world-estado w) "menu")
              (> x 460) (< x 540)
              (> y 160) (< y 240))
         (begin (stop) (play go)
                (nuevo-mundo 2))]

        [(and (equal? (world-estado w) "menu")
              (> x 660) (< x 740)
              (> y 160) (< y 240))
         (begin (stop) (play starw)
                (nuevo-mundo 3))]

        [else w])

      w))

;; Dibujo de objetos
(define (dibujar-objeto obj escena)
  (match obj
    [(list 'sling p)
     (place-image sling (posn-x p) (posn-y p) escena)]

    [(list 'block1 p)
     (place-image block1 (posn-x p) (posn-y p) escena)]

    [(list 'block2 p)
     (place-image block2 (posn-x p) (posn-y p) escena)]

    [(list 'block3 p)
     (place-image block3 (posn-x p) (posn-y p) escena)]

    [(list 'rock p)
     (place-image rock (posn-x p) (posn-y p) escena)]

    [(list 'rock1 p)
     (place-image rock1 (posn-x p) (posn-y p) escena)]

    [(list 'rock2 p)
     (place-image rock2 (posn-x p) (posn-y p) escena)]

    [(list 'rock3 p)
     (place-image rock3 (posn-x p) (posn-y p) escena)]

    [(list 'glass p)
     (place-image glass (posn-x p) (posn-y p) escena)]

    [(list 'glass1 p)
     (place-image glass1 (posn-x p) (posn-y p) escena)]


    [(list 'pig p)
     (place-image pig (posn-x p) (posn-y p) escena)]))
;; Previsualización de trayectoria (línea roja punteada)
(define (trayectoria w escena)
  (if (and (world-ready w) (not (world-launched w)))
      (let loop ([x (world-x w)] [y (world-y w)]
                                 [vx (* (world-power w) (cos (/ (* pi (world-angle w)) 180)))]
                                 [vy (* (world-power w) (- (sin (/ (* pi (world-angle w)) 180))))]
                                 [esc escena] [steps 30])
        (if (zero? steps)
            esc
            (loop (+ x vx) (+ y vy) vx (+ vy 1)
                  (place-image (circle 2 "outline" "red") x y esc)
                  (- steps 1))))
      escena))

(define (boton-circular color txt)
  (overlay
   (text txt 16 "white")
   (circle 35 "solid" color)))

(define boton-levels
  (boton-circular "blue" "MENU"))

(define boton-restart2
  (boton-circular "gray" "RESET"))

(define boton-next
  (boton-circular "gold" "NEXT"))

;; Dibujo completo
(define (draw w)
  (cond
    [(equal? (world-estado w) "jugando")
     
     (define escena-base
       (foldl dibujar-objeto
              (place-image
               (fondo-nivel (world-nivel w))
               500 200
               (empty-scene 1000 400))
              (world-objs w)))

     (define escena-con-ground (place-image ground 500 450
                                            (place-image cesped 500 340
                                                         (place-image hierba 500 330 escena-base))))

     (define escena-con-bird
       (if (world-ready w)
           (place-image bird (world-x w) (world-y w) escena-con-ground)
           escena-con-ground))
     (define escena-con-trayectoria (trayectoria w escena-con-bird))

     (define escena-con-score (place-image (text (string-append "Score: " (number->string (world-score w))) 20 "black")
                                           900 30 escena-con-trayectoria))
     
     (define escena-final
       (place-image
        (text
         (string-append
          "Angle: "
          (number->string (world-angle w))
          "°  Power: "
          (number->string (world-power w)))
         15 "white") 500 370
                     escena-con-score))

     (define escena-con-menu
       (place-image  boton-menu-top 20 20 escena-final))

     (if (world-menu? w)
         (let ([panel
                (place-image
                 (rectangle 200 400 "solid" "black")
                 100 200 
                 escena-con-menu)])

           (place-image boton-menu-top 20 20
                        (place-image boton-reset 100 100
                                     (place-image boton-nivel 100 200
                                                  (place-image boton-next2 100 300
                                                               (place-image boton-close 180 20
                                                                            panel))))))
         escena-con-menu)]
    
    [(equal? (world-estado w) "final")
     (define fondo (place-image
                    (rectangle 320 420 "solid" "black")
                    500 200 (empty-scene 1000 400)))

     (define escena1 (place-image
                      (text "LEVEL CLEARED!" 35 "white") 500 50 (place-image fnivel 500 200 fondo)))

     (define estrella
       (star 40 "solid" "yellow"))

     (define escena2 (place-image estrella 400 130
                                  (place-image estrella 500 100
                                               (place-image estrella 600 130 escena1))))

     (define escena3 (place-image (text
                                   (number->string (world-score w)) 40 "white") 500 220 escena2))

     (place-image boton-next 620 330
                  (place-image boton-restart2 500 330
                               (place-image boton-levels 380 330 escena3)))]

    [(equal? (world-estado w) "menu")
     (define fondo
       (empty-scene 1000 400))
     
     (define titulo
       (place-image
        (text "SELECT LEVEL" 40 "blue")
        500 50 (place-image fnivel 500 150 
                            fondo)))
     
     (define (nivel-boton n x y escena)
       (place-image
        (overlay
         (text (number->string n) 25 "white")
         (rectangle 80 80 "solid" "darkblue"))x y  escena))
 
     (nivel-boton 3 700 200
                  (nivel-boton 2 500 200
                               (nivel-boton 1 300 200
                                            titulo)))]

    [(equal? (world-estado w) "victoria")
     (define fondo
       (rectangle 1000 400 "solid" "black"))
     (place-image
      (text
       (string-append
        "CARGANDO"
        (make-string (remainder (quotient (world-timer w) 10) 4) #\.))
       30
       "white")   500 170
                  (place-image
                   carga 500 200
                   (place-image
                    fondo
                    500 200
    
                    (empty-scene 1000 400))))]
    ))

;; Ejecutar simulación
(big-bang initial-world
  [on-tick update]
  [to-draw draw]
  [on-key control]
  [on-mouse mouse]
  [stop-when
   (lambda (w)
     (equal? (world-estado w) "cerrar"))

   (lambda (w)
     (begin
       (stop)
       (empty-scene 1 1)))])
