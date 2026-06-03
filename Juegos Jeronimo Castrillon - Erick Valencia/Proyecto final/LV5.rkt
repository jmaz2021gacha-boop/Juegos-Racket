#lang racket

(require 2htdp/universe)
(require 2htdp/image)

;; =========================================================
;; SPRITES
;; =========================================================
   
(define sprite-idle
  (scale 1.25
         (bitmap "idle.LV.png")))

(define sprite-walk1
  (scale 1.25
         (bitmap "step.LV.png")))

(define sprite-walk2
  (scale 1.25
         (bitmap "idle.LV.png")))

(define sprite-jump
  (scale 1.25
         (bitmap "step.LV.png")))


;; =========================================================
;; CONFIGURACIÓN GENERAL
;; =========================================================

(define ANCHO 1000)
(define ALTO 700)

(define COLOR-FONDO "white")
(define COLOR-BORDES "gray")

;; Física
(define GRAVEDAD 0.7)
(define VELOCIDAD-MOVIMIENTO 5)
(define FUERZA-SALTO -14)

;; =========================================================
;; HITBOXES DEL MAPA
;;
;; Cada pared tiene:
;; x y ancho alto
;;
;; IMPORTANTE:
;; Aquí luego puedes agregar MÁS plataformas
;; o hacer niveles diferentes.
;; =========================================================

(define-struct bloque (x y w h))

(define-struct pincho (x y w h))

;; Suelo
(define suelo
  (bloque 0 560 200 140))

(define suelo2
  (bloque 600 560 500 140))

(define suelo3
  (bloque 0 560 120 140))

(define suelo4
  (bloque 400 560 80 140))

(define suelo5
  (bloque 0 560 150 140))

;; Techo
(define techo
  (bloque 0 0 1000 120))

;; Pared izquierda
(define pared-izquierda
  (bloque 0 0 50 600))

;; Pared derecha
(define pared-derecha
  (bloque 950 0 50 700))

;; Vacío
(define vacio
  (bloque 0 700 1000 200))

;; =========================================================
;; PLATAFORMA
;; =========================================================

(define plataforma-1
  (bloque 300 450 250 25))

(define pincho-1
  (pincho 370 413 40 40))

(define pincho-2
  (pincho 600 520 40 40))

(define pincho-3
  (pincho 700 520 40 40))

;; =========================================================
;; BLOQUES DEL NIVEL
;; =========================================================

(define bloques-nivel-1
  (list suelo
        suelo2
        techo
        pared-izquierda
        pared-derecha

        ;; Plataforma
        plataforma-1))

;; =========================================================
;; NIVEL 2
;; =========================================================

(define plataforma-2
  (bloque 150 470 200 25))

(define plataforma-3
  (bloque 500 400 200 25))

(define plataforma-4
  (bloque 750 300 150 25))

(define bloques-nivel-2
  (list suelo3
        suelo4
        techo
        pared-izquierda
        pared-derecha

        plataforma-2
        plataforma-3
        plataforma-4))

;; =========================================================
;; NIVEL 3
;; =========================================================

(define plataforma-5
  (bloque 180 500 180 25))

(define plataforma-6
  (bloque 450 420 180 25))

(define plataforma-7
  (bloque 700 330 100 25))

(define plataforma-8
  (bloque 500 220 120 25))

(define bloques-nivel-3
  (list

   ;; suelo separado   
   suelo5

   ;; bordes
   techo
   pared-izquierda
   pared-derecha

   ;; plataformas
   plataforma-5
   plataforma-6
   plataforma-7
   plataforma-8))


;;==============
;;pinchos
;;==============

(define pinchos-nivel-1
  (list pincho-1
        pincho-2
        pincho-3))


(define pinchos-nivel-2

  (list

   (pincho 400 520 40 40)
   (pincho 440 520 40 40)

   (pincho 650 360 40 40)

   (pincho 795 260 40 40)))


(define pinchos-nivel-3

  (list
   
   ;; plataforma 1
   (pincho 180 460 40 40)
   (pincho 320 460 40 40)

   ;; plataforma 2
   (pincho 520 380 40 40)

   ;; plataforma 3
   (pincho 700 290 40 40)
   (pincho 760 290 40 40)

   ))

;; =========================================================
;; PORTAL / META
;; =========================================================

(define-struct meta (x y w h))

(define meta-nivel-1
  (meta 850 490 60 70))

(define meta-nivel-2
  (meta 840 220 60 80))

(define meta-nivel-3
  (meta 530 140 60 80))

;; =========================================================
;; PERSONAJE
;;
;; x y     -> posición
;; vx vy   -> velocidad
;; suelo?  -> está tocando el suelo
;;
;; =========================================================

(define-struct jugador
  (x y
     vx vy

     suelo?

     izquierda?
     derecha?

     mirando
     frame

     nivel))

(define jugador-inicial
  (jugador 100 400

           0 0

           #f

           #f
           #f

           "izquierda"

           0

           1))

(define jugador-inicial2
  (jugador 100 400

           0 0

           #f

           #f
           #f

           "izquierda"

           0

           2))

(define jugador-inicial3
  (jugador 100 400

           0 0

           #f

           #f
           #f

           "izquierda"

           0

           3))

;; =========================================================
;; VELOCIDAD SEGÚN TECLAS
;; =========================================================

(define (calcular-vx j)

  (cond

    [(jugador-izquierda? j)
     (- VELOCIDAD-MOVIMIENTO)]

    [(jugador-derecha? j)
     VELOCIDAD-MOVIMIENTO]

    [else 0]))


;; =========================================================
;; FUNCIONES DE COLISIÓN
;; =========================================================

;; Detecta si dos rectángulos chocan
(define (colision? x1 y1 w1 h1 x2 y2 w2 h2)
  (and (< x1 (+ x2 w2))
       (> (+ x1 w1) x2)
       (< y1 (+ y2 h2))
       (> (+ y1 h1) y2)))

;; Tamaño del jugador
(define JUGADOR-W 20)
(define JUGADOR-H 40)

;; =========================================================
;; MOVIMIENTO Y FÍSICA
;; =========================================================

;; =========================================================
;; COLISIONES HORIZONTALES
;; =========================================================

(define (resolver-horizontal j bloques)

  (foldl
   (lambda (b actual)

     (define px (jugador-x actual))
     (define py (jugador-y actual))

     (define bx (bloque-x b))
     (define by (bloque-y b))
     (define bw (bloque-w b))
     (define bh (bloque-h b))

     (if (colision?
          px py
          JUGADOR-W JUGADOR-H
          bx by bw bh)

         (cond

           ;; chocando hacia derecha
           [(> (jugador-vx actual) 0)

            (jugador
             (- bx JUGADOR-W)
             py
             0
             (jugador-vy actual)
             (jugador-suelo? actual)

             (jugador-izquierda? actual)
             (jugador-derecha? actual)

             (jugador-mirando actual)
             (jugador-frame actual)

             (jugador-nivel actual))]

           ;; chocando hacia izquierda
           [(< (jugador-vx actual) 0)

            (jugador
             (+ bx bw)
             py
             0
             (jugador-vy actual)
             (jugador-suelo? actual)

             (jugador-izquierda? actual)
             (jugador-derecha? actual)

             (jugador-mirando actual)
             (jugador-frame actual)

             (jugador-nivel actual))]

           [else actual])

         actual))

   j
   bloques))

;; =========================================================
;; COLISIONES VERTICALES
;; =========================================================

(define (resolver-vertical j bloques)

  (foldl
   (lambda (b actual)

     (define px (jugador-x actual))
     (define py (jugador-y actual))

     (define bx (bloque-x b))
     (define by (bloque-y b))
     (define bw (bloque-w b))
     (define bh (bloque-h b))

     (if (colision?
          px py
          JUGADOR-W JUGADOR-H
          bx by bw bh)

         (cond

           ;; caer sobre suelo
           [(> (jugador-vy actual) 0)

            (jugador
             px
             (- by JUGADOR-H)
             (jugador-vx actual)
             0
             #t

             (jugador-izquierda? actual)
             (jugador-derecha? actual)

             (jugador-mirando actual)
             (jugador-frame actual)
             (jugador-nivel actual))]

           ;; golpear techo
           [(< (jugador-vy actual) 0)

            (jugador
             px
             (+ by bh)
             (jugador-vx actual)
             0
             #f

             (jugador-izquierda? actual)
             (jugador-derecha? actual)

             (jugador-mirando actual)
             (jugador-frame actual)
             (jugador-nivel actual))]

           [else actual])

         actual))

   j
   bloques))

;; =========================================================
;; ACTUALIZAR JUGADOR
;; =========================================================

(define (bloques-actuales j)

  (cond

    [(= (jugador-nivel j) 1)
     bloques-nivel-1]

    [(= (jugador-nivel j) 2)
     bloques-nivel-2]

    [else
     bloques-nivel-3]))

;;pinchos segun nivel
(define (pinchos-actuales j)

  (cond

    [(= (jugador-nivel j) 1)
     pinchos-nivel-1]

    [(= (jugador-nivel j) 2)
     pinchos-nivel-2]

    [else
     pinchos-nivel-3]))

;;metas segun nivel
(define (meta-actual j)

  (cond

    [(= (jugador-nivel j) 1)
     meta-nivel-1]
    
    [(= (jugador-nivel j) 2)
     meta-nivel-2]
    
    [(= (jugador-nivel j) 3)
     meta-nivel-3]))

(define (jugador-murio? j)

  (ormap
   (lambda (p)

     (define margen 10)

     (colision?
      (jugador-x j)
      (jugador-y j)
      JUGADOR-W
      JUGADOR-H

      (+ (pincho-x p) margen)
      (+ (pincho-y p) margen)

      (- (pincho-w p)
         (* margen 2))

      (- (pincho-h p)
         (* margen 2))))

   (pinchos-actuales j)))

;;vacio
(define (cayo-al-vacio? j)

  (colision?
   (jugador-x j)
   (jugador-y j)
   JUGADOR-W
   JUGADOR-H

   (bloque-x vacio)
   (bloque-y vacio)
   (bloque-w vacio)
   (bloque-h vacio)))


(define (toco-meta? j)

  (define m (meta-actual j))

  (colision?
   (jugador-x j)
   (jugador-y j)
   JUGADOR-W
   JUGADOR-H

   (meta-x m)
   (meta-y m)
   (meta-w m)
   (meta-h m)))


(define (actualizar-jugador j)
  
  (define nuevo-vx
  (calcular-vx j))

;; Dirección a la que mira
(define mirando-nuevo

  (cond
    [(< nuevo-vx 0) "derecha"]
    [(> nuevo-vx 0) "izquierda"]
    [else (jugador-mirando j)]))

;; Animación
(define nuevo-frame
  (+ (jugador-frame j) 2))

  ;; -------------------------
  ;; MOVIMIENTO HORIZONTAL
  ;; -------------------------

  (define mov-x
  (jugador
   (+ (jugador-x j)
      nuevo-vx)

   (jugador-y j)

   nuevo-vx
   (jugador-vy j)

   (jugador-suelo? j)

   (jugador-izquierda? j)
   (jugador-derecha? j)

   mirando-nuevo
   nuevo-frame

   (jugador-nivel j)))

  (define despues-horizontal
    (resolver-horizontal mov-x (bloques-actuales j)))

  ;; -------------------------
  ;; GRAVEDAD
  ;; -------------------------

  (define nuevo-vy
    (+ (jugador-vy despues-horizontal)
       GRAVEDAD))

  ;; -------------------------
  ;; MOVIMIENTO VERTICAL
  ;; -------------------------

  (define mov-y
  (jugador
   (jugador-x despues-horizontal)

   (+ (jugador-y despues-horizontal)
      nuevo-vy)

   (jugador-vx despues-horizontal)
   nuevo-vy

   #f

   (jugador-izquierda? despues-horizontal)
   (jugador-derecha? despues-horizontal)

   (jugador-mirando despues-horizontal)
   (jugador-frame despues-horizontal)

   (jugador-nivel despues-horizontal)))

  ;; -------------------------
  ;; RESOLVER VERTICAL
  ;; -------------------------

 (define final
  (resolver-vertical mov-y (bloques-actuales j)))

(cond ((= (jugador-nivel final) 1)
       (cond
         
         ;; morir
         [(or (jugador-murio? final)
              (cayo-al-vacio? final))
          
          jugador-inicial]
         
         ;; pasar nivel
         [(toco-meta? final)
          
          (jugador
           100
           400
           
           0
           0
           
           #f
           
           #f
           #f
           
           "izquierda"
           
           0
           
           (+ (jugador-nivel final) 1))]
         
         [else final]))
      ((= (jugador-nivel final) 2)
       (cond
         
         ;; morir
         [(or (jugador-murio? final)
              (cayo-al-vacio? final))
          
          jugador-inicial2]
         
         ;; pasar nivel
         [(toco-meta? final)
          
          (jugador
           100
           400
           
           0
           0
           
           #f
           
           #f
           #f
           
           "izquierda"
           
           0
           
           (+ (jugador-nivel final) 1))]
         
         [else final]))
      ((= (jugador-nivel final) 3)
       (cond
         
         ;; morir
         [(or (jugador-murio? final)
              (cayo-al-vacio? final))
          
          jugador-inicial3]
         
         ;; pasar nivel
         [(toco-meta? final)
          
          (jugador
           100
           400
           
           0
           0
           
           #f
           
           #f
           #f
           
           "izquierda"
           
           0
           
           (+ (jugador-nivel final) 1))]
         
         [else final]))))
      
;; =========================================================
;; TECLADO
;; =========================================================

(define (tecla j tecla)

  (cond

    ;; izquierda
    [(key=? tecla "left")

     (jugador
      (jugador-x j)
      (jugador-y j)

      (jugador-vx j)
      (jugador-vy j)

      (jugador-suelo? j)

      #t
      (jugador-derecha? j)

      (jugador-mirando j)
      (jugador-frame j)

      (jugador-nivel j))]

    ;; derecha
    [(key=? tecla "right")

     (jugador
      (jugador-x j)
      (jugador-y j)

      (jugador-vx j)
      (jugador-vy j)

      (jugador-suelo? j)

      (jugador-izquierda? j)
      #t

      (jugador-mirando j)
      (jugador-frame j)

      (jugador-nivel j))]

    ;; salto
    [(and (key=? tecla "up")
          (jugador-suelo? j))

     (jugador
      (jugador-x j)
      (jugador-y j)

      (jugador-vx j)
      FUERZA-SALTO

      #f

      (jugador-izquierda? j)
      (jugador-derecha? j)

      (jugador-mirando j)
      (jugador-frame j)

      (jugador-nivel j))]

    [else j]))

;; Cuando se suelta una tecla
(define (soltar-tecla j tecla)

  (cond

    ;; soltar izquierda
    [(key=? tecla "left")

     (jugador
      (jugador-x j)
      (jugador-y j)

      (jugador-vx j)
      (jugador-vy j)

      (jugador-suelo? j)

      #f
      (jugador-derecha? j)

      (jugador-mirando j)
      (jugador-frame j)

      (jugador-nivel j))]

    ;; soltar derecha
    [(key=? tecla "right")

     (jugador
      (jugador-x j)
      (jugador-y j)

      (jugador-vx j)
      (jugador-vy j)

      (jugador-suelo? j)

      (jugador-izquierda? j)
      #f

      (jugador-mirando j)
      (jugador-frame j)

      (jugador-nivel j))]

    [else j]))

;; =========================================================
;; DIBUJAR BLOQUES
;; =========================================================
(define (dibujar-meta m escena)

  (place-image
   (rectangle
    (meta-w m)
    (meta-h m)
    "solid"
    "yellow")

   (+ (meta-x m)
      (/ (meta-w m) 2))

   (+ (meta-y m)
      (/ (meta-h m) 2))

   escena))


(define (dibujar-pincho p escena)

  (place-image
   (triangle
    (pincho-w p)
    "solid"
    "black")

   (+ (pincho-x p)
      (/ (pincho-w p) 2))

   (+ (pincho-y p)
      (/ (pincho-h p) 2))

   escena))

(define (dibujar-bloque b escena)

  (place-image
   (rectangle
    (bloque-w b)
    (bloque-h b)
    "solid"
    COLOR-BORDES)

   (+ (bloque-x b)
      (/ (bloque-w b) 2))

   (+ (bloque-y b)
      (/ (bloque-h b) 2))

   escena))


;; =========================================================
;; ELEGIR SPRITE
;; =========================================================

(define (sprite-actual j)

  (cond

    ;; EN EL AIRE
    [(not (jugador-suelo? j))

     sprite-jump]

    ;; CAMINANDO
    [(not (= (jugador-vx j) 0))

     (if (< (modulo (jugador-frame j) 20) 10)
         sprite-walk1
         sprite-walk2)]

    ;; QUIETO
    [else
     sprite-idle]))

;; =========================================================
;; DIBUJAR TODO
;; =========================================================


(define (dibujar j)

  ;; GANASTE
  (if (> (jugador-nivel j) 3)

      (place-image
       (text "GANASTE" 60 "green")
       (/ ANCHO 2)
       (/ ALTO 2)
       (empty-scene ANCHO ALTO "black"))

      ;; juego normal
      (local

        [(define escena
           (empty-scene ANCHO ALTO COLOR-FONDO))

         (define escena-con-bloques
           (foldl dibujar-bloque
                  escena
                  (bloques-actuales j)))

         (define escena-con-pinchos
           (foldl dibujar-pincho
                  escena-con-bloques
                  (pinchos-actuales j)))

         (define escena-con-meta
           (dibujar-meta
            (meta-actual j)
            escena-con-pinchos))]

        ;; resultado final
        (place-image

         (if (string=? (jugador-mirando j) "izquierda")
             (flip-horizontal
              (sprite-actual j))
             (sprite-actual j))

         (+ (jugador-x j) 10)
         (+ (jugador-y j) 20)

         escena-con-meta))))

;; =========================================================
;; INICIAR JUEGO
;; =========================================================

(big-bang jugador-inicial

  ;; Actualización por frame
  (on-tick actualizar-jugador)

  ;; Presionar teclas
  (on-key tecla)

  ;; Soltar teclas
  (on-release soltar-tecla)

  ;; Dibujar
  (to-draw dibujar))