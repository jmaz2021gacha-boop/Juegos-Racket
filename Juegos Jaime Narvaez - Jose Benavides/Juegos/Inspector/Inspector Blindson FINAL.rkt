(require 2htdp/universe)
(require 2htdp/image)

(define ANCHO 1300)
(define ALTO 700)

(define FONDO
  (bitmap "C:/Users/Olga Lucia/Desktop/Programacion/Juegos/Inspector/nivel1.png")
  )

(define-struct jugador (x y dx dy salto?))

(define INSPECTOR
  (make-jugador 200 100 0 0 #f)
  )

(define JUGADOR-W 40)
(define JUGADOR-H 40)

(define-struct plataforma (x y w h color))

(define plataformas
  (list
   (make-plataforma 650 615 460 40 (color 255 255 255 0))
   (make-plataforma 190 150 370 20 (color 255 255 255 0))
   (make-plataforma 650 150 460 20 (color 255 255 255 0))
   (make-plataforma 1085 150 320 20 (color 255 255 255 0))
   (make-plataforma 650 660 650 20 (color 255 255 255 0))

   (make-plataforma 90 22 180 60 (color 255 255 255 0))
   (make-plataforma 282 11 200 60 (color 255 255 255 0))
   (make-plataforma 880 22 1000 60 (color 255 255 255 0))

   (make-plataforma 650 100 280 100 (color 255 255 255 0))
   (make-plataforma 40 100 20 100 (color 255 255 255 0))

   (make-plataforma 345 600 50 900 (color 255 255 255 0))
   (make-plataforma 452 312 50 340 (color 255 255 255 0))
   (make-plataforma 860 312 78 340 (color 255 255 255 0))
   (make-plataforma 950 600 50 900 (color 255 255 255 0))

   (make-plataforma 635 565 100 100 (color 255 255 255 0))
   (make-plataforma 803 568 41 50 (color 255 255 255 0))
   (make-plataforma 740 570 150 20 (color 255 255 255 0))
   )
  )

(define escaleras
  (list
   (make-plataforma 398 365 1 455 (color 255 255 255 0))
   (make-plataforma 904 365 1 455 (color 255 255 255 0))
   )
  )

(define-struct puerta (x y color))

(define puerta-inspector
  (make-puerta 1158 116 (color 255 255 255 0))
  )

(define-struct mundo (estado inspector))

(define INICIO
  (make-mundo "inicio" INSPECTOR)
  )

(define (dentro-boton? x y bx by bw bh)
  (and (>= x bx)
       (<= x (+ bx bw))
       (>= y by)
       (<= y (+ by bh))
       )
  )

(define (en-escalera? j lista)
  (cond
    [(empty? lista) #f]
    [(and (< (abs (- (jugador-x j) (plataforma-x (first lista)))) (+ (/ JUGADOR-W 2) (/ (plataforma-w (first lista)) 2)))
          (< (abs (- (jugador-y j) (plataforma-y (first lista)))) (+ (/ JUGADOR-H 2) (/ (plataforma-h (first lista)) 2)))
          )
     #t]
    [else (en-escalera? j (rest lista))]))

(define (dibujar-jugador j escena)
  (place-image (rectangle 3 9 "solid" (color 153 153 153)) (jugador-x j) (+ (jugador-y j) 17)
  (place-image (rectangle 3 3 "solid" (color 0 0 0)) (jugador-x j) (+ (jugador-y j) 8)
  (place-image (rectangle 3 3 "solid" (color 0 0 0)) (jugador-x j) (+ (jugador-y j) 4)
  (place-image (rectangle 5 9 "solid" (color 0 0 0)) (- (jugador-x j) 4) (- (jugador-y j) 10)
  (place-image (rectangle 5 9 "solid" (color 0 0 0)) (+ (jugador-x j) 6) (- (jugador-y j) 10)
  (place-image (rectangle 24 12 "solid" (color 25 25 25)) (jugador-x j) (+ (jugador-y j) 17)
  (place-image (rectangle 24 22 "solid" (color 153 153 153)) (jugador-x j) (+ (jugador-y j) 11)
  (place-image (rectangle 24 44 "solid" (color 255 128 51)) (jugador-x j) (jugador-y j) escena)
  ))))))))

(define (dibujar-plataforma p escena)
  (place-image (rectangle (plataforma-w p) (plataforma-h p) "solid" (plataforma-color p))  (plataforma-x p) (plataforma-y p) escena)
  )

(define (dibujar-plataformas lista escena)
  (cond
    [(empty? lista) escena]
    [else
     (dibujar-plataformas (rest lista) (dibujar-plataforma (first lista) escena))]))

(define (dibujar-puerta p escena)
  (place-image (rectangle 70 50 "solid" (puerta-color p)) (puerta-x p) (puerta-y p) escena)
  )

(define (pantalla-inicio)
  (place-image
    (rectangle 360 100 "solid" (color 255 255 255 0)) 650 610 (bitmap "C:/Users/Olga Lucia/Desktop/Programacion/Juegos/Inspector/Pantalla carga.jpeg")))

(define (pantalla-nivel1 mundo)
  (define escena1 (dibujar-plataformas plataformas FONDO))
  (define escena2 (dibujar-puerta puerta-inspector escena1))
  (define escena3 (dibujar-plataformas escaleras escena2))
  (define escena4 (dibujar-jugador (mundo-inspector mundo) escena3))
  escena4)

(define FONDO2
  (place-image (rectangle 640 225 "solid" (color 39 149 245 100)) 660 555 (bitmap "C:/Users/Olga Lucia/Desktop/Programacion/Juegos/Inspector/nivel2.png"))
  )

(define plataformas2
  (list
   (make-plataforma 112 410 240 50 (color 255 255 255 0))
   (make-plataforma 1170 410 320 50 (color 255 255 255 0))
   (make-plataforma 650 675 700 20 (color 255 255 255 0))

   (make-plataforma 40 400 20 400 (color 255 255 255 0))

   (make-plataforma 230 585 200 300 (color 255 255 255 0))
   (make-plataforma 1280 312 50 340 (color 255 255 255 0))
   (make-plataforma 1030 605 78 340 (color 255 255 255 0))
   )
  )

(define escaleras2
  (list
   (make-plataforma 950 530 1 200
                    (color 255 255 255 0))))

(define puerta2
  (make-puerta 1125 365
               (color 255 255 255 0)))

(define ZONA-LENTA-X 660)
(define ZONA-LENTA-Y 555)
(define ZONA-LENTA-W 640)
(define ZONA-LENTA-H 225)

(define (en-zona-lenta? j)
  (and
   (<= (- ZONA-LENTA-X (/ ZONA-LENTA-W 2)) (jugador-x j) (+ ZONA-LENTA-X (/ ZONA-LENTA-W 2)))
   (<= (- ZONA-LENTA-Y (/ ZONA-LENTA-H 2)) (jugador-y j) (+ ZONA-LENTA-Y (/ ZONA-LENTA-H 2)))
   )
  )

(define VELOCIDAD-LENTA 2)
(define VELOCIDAD-SALTO-LENTO -5)
(define GRAVEDAD-LENTA 0.3)

(define (pantalla-nivel2 mundo)
  (define escena1 (dibujar-plataformas  plataformas2 FONDO2))
  (define escena2 (dibujar-puerta puerta2 escena1))
  (define escena3 (dibujar-plataformas escaleras2 escena2))
  (define escena4 (dibujar-jugador (mundo-inspector mundo) escena3))
  escena4)

(define (actualizar-jugador2 j)
  (define gravedad
    (if (en-zona-lenta? j)
        GRAVEDAD-LENTA
        GRAVEDAD)
    )
  (define nuevo
    (make-jugador (+ (jugador-x j) (jugador-dx j)) (+ (jugador-y j) (jugador-dy j)) (jugador-dx j) (+ (jugador-dy j) gravedad) (jugador-salto? j)
                  )
    )
  (define (resolver lista jugador)
    (cond
      [(empty? lista) jugador]
      [(colisiona? jugador (first lista))
       (resolver (rest lista) (resolver-colision jugador (first lista)))
       ]
      [else (resolver (rest lista) jugador)]
      )
    )
  (if (en-escalera? nuevo escaleras2)
      (make-jugador (jugador-x nuevo) (jugador-y nuevo) (jugador-dx nuevo) 0 (jugador-salto? nuevo)) (resolver plataformas2 nuevo))
  )

(define (gano2? mundo)
  (define inspector (mundo-inspector mundo))
  (and
   (cerca? (jugador-x inspector) (puerta-x puerta2))
   (cerca? (jugador-y inspector) (puerta-y puerta2))
   )
  )

(define (pantalla-final)
  (overlay (text "Gracias por jugarme" 100 "red") (bitmap "C:/Users/Olga Lucia/Desktop/Programacion/Juegos/Inspector/Pantalla final.png"))
  )

(define (dibujar mundo)
  (cond
    [(string=? (mundo-estado mundo) "inicio")
     (pantalla-inicio)]
    [(string=? (mundo-estado mundo) "nivel1")
     (pantalla-nivel1 mundo)]
    [(string=? (mundo-estado mundo) "nivel2")
     (pantalla-nivel2 mundo)]
    [(string=? (mundo-estado mundo) "final")
     (pantalla-final)]))

(define GRAVEDAD 1)
(define VELOCIDAD-SALTO -15)

(define (colisiona? j p)
  (and
   (< (abs (- (jugador-x j) (plataforma-x p))) (+ (/ JUGADOR-W 2) (/ (plataforma-w p) 2)))
   (< (abs (- (jugador-y j) (plataforma-y p))) (+ (/ JUGADOR-H 2) (/ (plataforma-h p) 2)))
   )
  )

(define (resolver-colision j p)
  (define dx (- (jugador-x j) (plataforma-x p)))
  (define dy (- (jugador-y j) (plataforma-y p)))
  (define overlap-x (- (+ (/ JUGADOR-W 2) (/ (plataforma-w p) 2)) (abs dx)))
  (define overlap-y (- (+ (/ JUGADOR-H 2) (/ (plataforma-h p) 2)) (abs dy)))
  (cond
    [(< overlap-x overlap-y)
     (if (< dx 0)
         (make-jugador (- (plataforma-x p) (/ (plataforma-w p) 2) (/ JUGADOR-W 2)) (jugador-y j) 0 (jugador-dy j) (jugador-salto? j))
         (make-jugador (+ (plataforma-x p) (/ (plataforma-w p) 2) (/ JUGADOR-W 2)) (jugador-y j) 0 (jugador-dy j) (jugador-salto? j))
         )
     ]
    [else (if (< dy 0)
              (make-jugador (jugador-x j) (- (plataforma-y p) (/ (plataforma-h p) 2) (/ JUGADOR-H 2)) (jugador-dx j) 0 #f)
              (make-jugador (jugador-x j) (+ (plataforma-y p) (/ (plataforma-h p) 2) (/ JUGADOR-H 2)) (jugador-dx j) 0 (jugador-salto? j))
              )
          ]
    )
  )

(define (actualizar-jugador j)
  (define nuevo (make-jugador (+ (jugador-x j) (jugador-dx j)) (+ (jugador-y j) (jugador-dy j)) (jugador-dx j) (+ (jugador-dy j) GRAVEDAD) (jugador-salto? j)))
  (define (resolver lista jugador)
    (cond [(empty? lista) jugador]
          [(colisiona? jugador (first lista))
           (resolver (rest lista) (resolver-colision jugador (first lista)))
           ]
          [else (resolver (rest lista) jugador)]
          )
    )
  (if (en-escalera? nuevo escaleras)
      (make-jugador (jugador-x nuevo) (jugador-y nuevo) (jugador-dx nuevo) 0 (jugador-salto? nuevo)) (resolver plataformas nuevo)))

(define (actualizar mundo)
  (cond
    [(string=? (mundo-estado mundo) "nivel1")
     (if (gano? mundo)
         (make-mundo  "nivel2" (make-jugador 200 400 0 0 #f))
         (make-mundo "nivel1" (actualizar-jugador (mundo-inspector mundo))))]
    [(string=? (mundo-estado mundo) "nivel2")
     (if (gano2? mundo)
         (make-mundo "final" (mundo-inspector mundo))
         (make-mundo "nivel2" (actualizar-jugador2 (mundo-inspector mundo)))
         )
     ]
    [else mundo]
    )
  )

(define VELOCIDAD 5)
(define (tecla mundo t)
  (define inspector (mundo-inspector mundo))
  (define vel (if (and (string=? (mundo-estado mundo) "nivel2") (en-zona-lenta? inspector))
                  VELOCIDAD-LENTA
                  VELOCIDAD)
    )
  (define salto (if (and (string=? (mundo-estado mundo) "nivel2") (en-zona-lenta? inspector))
                    VELOCIDAD-SALTO-LENTO
        VELOCIDAD-SALTO))
  (cond
    [(or (key=? t "left") (key=? t "a"))
     (make-mundo (mundo-estado mundo)
                 (make-jugador (jugador-x inspector) (jugador-y inspector) (- vel) (jugador-dy inspector) (jugador-salto? inspector)))]
    [(or (key=? t "right") (key=? t "d"))
     (make-mundo (mundo-estado mundo) (make-jugador (jugador-x inspector) (jugador-y inspector) vel (jugador-dy inspector) (jugador-salto? inspector)))]
    [(and (or (key=? t "w") (key=? t "up")) (or (en-escalera? inspector escaleras) (en-escalera? inspector escaleras2)))
     (make-mundo (mundo-estado mundo) (make-jugador (jugador-x inspector) (- (jugador-y inspector) VELOCIDAD) (jugador-dx inspector) 0 (jugador-salto? inspector)))]
    [(and (or (key=? t "s") (key=? t "down")) (or (en-escalera? inspector escaleras) (en-escalera? inspector escaleras2)))
     (make-mundo (mundo-estado mundo) (make-jugador (jugador-x inspector) (+ (jugador-y inspector) VELOCIDAD) (jugador-dx inspector) 0 (jugador-salto? inspector)))]
    [(and (or (key=? t "w") (key=? t "up")) (not (jugador-salto? inspector)))
     (make-mundo (mundo-estado mundo) (make-jugador (jugador-x inspector) (jugador-y inspector) (jugador-dx inspector) salto #t))]
    [else mundo]
    )
  )

(define (soltar mundo t)
  (define inspector (mundo-inspector mundo))
  (cond
    [(or (key=? t "right") (key=? t "d") (key=? t "left") (key=? t "a"))
     (make-mundo (mundo-estado mundo) (make-jugador (jugador-x inspector) (jugador-y inspector) 0 (jugador-dy inspector) (jugador-salto? inspector)))]
    [else mundo]
    )
  )

(define (cerca? a b)
  (< (abs (- a b)) 40))

(define (gano? mundo)
  (define inspector (mundo-inspector mundo))
  (and (cerca? (jugador-x inspector) (puerta-x puerta-inspector))
       (cerca? (jugador-y inspector) (puerta-y puerta-inspector))
       )
  )

(define (mouse mundo x y evento)
  (cond
    [(and (string=? (mundo-estado mundo) "inicio") (string=? evento "button-down") (dentro-boton? x y 470 560 360 100)) (make-mundo "nivel1" INSPECTOR)]
    [else mundo]))

(big-bang INICIO
  [to-draw dibujar]
  [on-tick actualizar]
  [on-key tecla]
  [on-release soltar]
  [on-mouse mouse])