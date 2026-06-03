#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 1094)
(define HEIGHT 827)

;; Física general
(define GRAVEDAD 0.9)
(define SALTO -12.7)

;; Tamaño de personajes
(define PLAYER-WIDTH 36)
(define PLAYER-HEIGHT 36)

;; Movimiento horizontal con aceleración
(define VEL-X-MAX 7.3)
(define ACEL-X 0.85)
(define FRENADO-X 0.65)

;; Metas invisibles. Usan las coordenadas donde antes estaban las puertas.
(define META-FIREBOY-X 950)
(define META-FIREBOY-Y 150)
(define META-WATERGIRL-X 1020)
(define META-WATERGIRL-Y 150)
(define RADIO-META 30)

;; Márgenes de colisión ajustados
(define MARGEN-RAMPA 10)
(define MARGEN-AGARRE-BORDE 10)
(define MARGEN-PIE-PELIGRO 6)
;; Temporizador y paneles de fin de nivel
(define FPS 30)
(define PANEL-Y-INICIAL -300)
(define PANEL-Y-FINAL (/ HEIGHT 2))
(define VEL-PANEL 18)

;; Coordenadas del temporizador.
;; Ajusta TIMER-X y TIMER-Y si no queda exactamente encima del contador viejo.
(define TIMER-X (/ WIDTH 2))
(define TIMER-Y 25)
(define TIMER-TAPA-W 150)
(define TIMER-TAPA-H 45)

;; Coordenadas de botones de pantallas.
;; Todas están en coordenadas de la ventana WIDTH x HEIGHT.
(define BOTON-PLAY-X (/ WIDTH 2))
(define BOTON-PLAY-Y 500)
(define BOTON-PLAY-W 230)
(define BOTON-PLAY-H 90)

;; Botones de la pantalla de niveles.
;; Calculados para la imagen niveles.png escalada a WIDTH x HEIGHT.
(define BOTON-NIVEL1-X 548)
(define BOTON-NIVEL1-Y 713)
(define BOTON-NIVEL2-X 582)
(define BOTON-NIVEL2-Y 625)
(define BOTON-NIVEL-R 42)

(define BOTON-VOLVER-X 80)
(define BOTON-VOLVER-Y 725)
(define BOTON-VOLVER-W 150)
(define BOTON-VOLVER-H 110)

;; Botones dentro de los paneles de 632x395.
;; Coordenadas locales: x,y desde la esquina superior izquierda del panel.
(define PANEL-MUERTE-MENU-X 149)
(define PANEL-MUERTE-MENU-Y 239)
(define PANEL-MUERTE-REINTENTAR-X 268)
(define PANEL-MUERTE-REINTENTAR-Y 239)
(define PANEL-MUERTE-SALIR-X 389)
(define PANEL-MUERTE-SALIR-Y 239)
(define PANEL-BOTON-W 96)
(define PANEL-BOTON-H 36)

(define PANEL-VICTORIA-CONTINUAR-X 268)
(define PANEL-VICTORIA-CONTINUAR-Y 263)
(define PANEL-VICTORIA-CONTINUAR-W 96)
(define PANEL-VICTORIA-CONTINUAR-H 36)

(define (cargar-imagen ruta fallback)
  (with-handlers ([exn:fail? (lambda (e) fallback)])
    (bitmap/file ruta)))

(define (ajustar-a-pantalla img)
  (scale/xy (/ WIDTH (image-width img))
            (/ HEIGHT (image-height img))
            img))

(define fondo-fallback
  (rectangle WIDTH HEIGHT "solid" (make-color 60 50 30)))

(define nivel1
  (cargar-imagen "imagenes/level1.png" fondo-fallback))

(define nivel2
  (cargar-imagen
   "imagenes/level2.png"
   (overlay (text "Nivel 2 sin mapa de colisiones definido" 30 "white")
            fondo-fallback)))

(define pantalla-inicio
  (ajustar-a-pantalla
   (cargar-imagen "imagenes/inicio.png" fondo-fallback)))

(define pantalla-niveles
  (ajustar-a-pantalla
   (cargar-imagen "imagenes/niveles.png" fondo-fallback)))

(define panel-victoria1
  (cargar-imagen "imagenes/victoria1.png"
                 (rectangle 632 395 "solid" "gray")))

(define panel-victoria2
  (cargar-imagen "imagenes/victoria2.png"
                 (rectangle 632 395 "solid" "gray")))

(define panel-muerte
  (cargar-imagen "imagenes/muerte.png"
                 (rectangle 632 395 "solid" "gray")))

(define (imagen-nivel nivel-id)
  (cond
    [(= nivel-id 2) nivel2]
    [else nivel1]))

;; ============================================================
;; ESTRUCTURAS
;; ============================================================

(struct rampa      (x y width height direccion) #:transparent)
(struct jugador1   (x y vel-x vel-y color dir-x) #:transparent)
(struct jugador2   (x y vel-x vel-y color dir-x) #:transparent)
(struct plataforma (x y width height tipo)       #:transparent)
(struct diamante   (x y tipo)                    #:transparent)
(struct palanca    (x y activada pisada-antes)   #:transparent)
(struct boton      (x y color activo)            #:transparent)
(struct plat-movil (x y width height color bajada y-arriba y-abajo) #:transparent)
(struct cubo       (x y vel-x)                   #:transparent)

;; ============================================================
;; RAMPAS
;; direccion "der" = /  sube de izquierda a derecha (pie izquierdo más bajo)
;; direccion "izq" = \  baja de izquierda a derecha (pie derecho más bajo)
;; ============================================================

(define rampas
  (list
   (rampa 998 720 28 27 "der")
   (rampa 937 632 25 23 "izq")
   (rampa 798 632 32 20 "der")
   (rampa 687 630 32 17 "izq")
   (rampa 505 594 56 53 "izq")
   (rampa 577 438 29 26 "izq")
   (rampa 786 283 52 56 "izq")
   (rampa 857 799 32 18 "der")
   (rampa 745 799 32 18 "izq")
   (rampa 631 799 32 18 "der")
   (rampa 518 799 32 18 "izq")
   (rampa 291 125 20 21 "der")
   (rampa 378 129 28 31 "izq")
   (rampa 440 159 23 22 "izq")
   ))

;; Dado el X del jugador, devuelve la Y del suelo de la rampa en ese punto
;; Retorna #f si el jugador está fuera del ancho de la rampa
(define (suelo-rampa-en-x r px)
  (define izq (- (rampa-x r) (/ (rampa-width r) 2)))
  (define der (+ (rampa-x r) (/ (rampa-width r) 2)))
  (define bot (+ (rampa-y r) (/ (rampa-height r) 2)))
  (define top (- (rampa-y r) (/ (rampa-height r) 2)))
  (if (and (>= px izq) (<= px der))
      (let ([t (/ (- px izq) (rampa-width r))])
        (cond
          [(string=? (rampa-direccion r) "der")
           ;; / : izquierda es baja, derecha es alta
           (- bot (* t (rampa-height r)))]
          [(string=? (rampa-direccion r) "izq")
           ;; \ : izquierda es alta, derecha es baja
           (+ top (* t (rampa-height r)))]
          [else bot]))
      #f))

;; Detecta si el PIE del jugador está realmente cerca de la superficie de la rampa.
;; Antes se permitía llegar hasta el fondo de la rampa, lo que generaba saltos y atraviesos.
(define (jugador-sobre-rampa? px py r)
  (define pie-y (+ py (/ PLAYER-HEIGHT 2)))
  (define suelo (suelo-rampa-en-x r px))
  (and suelo
       (<= (abs (- pie-y suelo)) MARGEN-RAMPA)))

(define (jugador-aterriza-en-rampa? px y-anterior y-nueva r)
  (define suelo (suelo-rampa-en-x r px))
  (define pie-antes (+ y-anterior (/ PLAYER-HEIGHT 2)))
  (define pie-despues (+ y-nueva (/ PLAYER-HEIGHT 2)))
  (and suelo
       ;; No permite corregir desde muy por debajo de la rampa.
       (<= pie-antes (+ suelo MARGEN-RAMPA))
       ;; Permite seguir la pendiente cuando el suelo cambia por el movimiento horizontal.
       (>= pie-despues (- suelo MARGEN-RAMPA))))

(define (rampa-que-toca px py)
  (findf (lambda (r) (jugador-sobre-rampa? px py r)) rampas))

;; Velocidad de resbale según la dirección de la rampa
;; Solo se aplica si el jugador está efectivamente sobre la rampa
(define (vel-resbale-si-rampa px py)
  (define r (rampa-que-toca px py))
  (if r
      (cond
        ;; rampa / : parte alta a la derecha → resbala hacia la izquierda
        [(string=? (rampa-direccion r) "der") -3]
        ;; rampa \ : parte alta a la izquierda → resbala hacia la derecha
        [(string=? (rampa-direccion r) "izq")  3]
        [else 0])
      0))

;; ============================================================
;; PLATAFORMAS ESTATICAS
;; ============================================================

(define plataformas
  (list
   (plataforma 250  800 500  22 "suelo")
   (plataforma 689  801  81  22 "suelo")
   (plataforma 983  801 215  22 "suelo")
   (plataforma 190  687 340  23 "suelo")

   (plataforma 0      0  45 1575 "pared")
   (plataforma 1090   0  45 1575 "pared")

   (plataforma 0  0 2150  45 "techo")
   (plataforma 1040 712  55  15 "suelo")
   (plataforma 50   576 850  22 "suelo")
   (plataforma 603  632 132  22 "suelo")
   (plataforma 869  632 108  22 "suelo")
   (plataforma 352  435 415  22 "suelo")
   (plataforma 830  464 470  22 "suelo")
   (plataforma 885  311 133  5 "suelo")
   (plataforma 885  365 133  5 "techo")
   (plataforma 885  337 133  47 "pared")
   (plataforma 663  264 195  15 "suelo")
   (plataforma 400  324 760  22 "suelo")
   (plataforma 95   208 143  15 "suelo")
   (plataforma 332  120  60  13 "suelo")
   (plataforma 411  146  31  6 "suelo")
   (plataforma 705  183 730  23 "suelo")
   (plataforma 990 766  10  48 "pared")
   (plataforma 567 287  10  45 "pared")
   (plataforma 406 250 134  10 "techo")
   (plataforma 406 220 134  48 "pared")
   (plataforma 161 265  10  98 "pared")
   ;; Zonas peligrosas
   (plataforma 573  797  80  15 "lava")
   (plataforma 800  797  80  15 "agua")
   (plataforma 745  630  80  15 "acido")))

;; ============================================================
;; OBJETOS INTERACTIVOS
;; ============================================================

(define diamantes-iniciales
  (list
   (diamante 330  78 "rojo")
   (diamante 220 380 "rojo")
   (diamante 572 755 "rojo")
   (diamante 670 130 "azul")
   (diamante 645 415 "azul")
   (diamante 795 755 "azul")
   (diamante  75 165 "azul")))

(define palanca-inicial   (palanca 260 560 #f #f))
(define botones-iniciales (list (boton 303 420 "amarillo" #f)
                                (boton 878 310 "rosa"     #f)))
(define plats-moviles-iniciales
  (list (plat-movil  83 435 117 18 "amarilla" #f 435 525)
        (plat-movil 1010 355 110 18 "rosa"    #f 355 444)))
(define cubo-inicial (cubo 640 237 0))

;; ============================================================
;; ESTADO DEL MUNDO
;; ============================================================

(struct estado
  (p1 p2 diamantes palanca-st botones plats-moviles cubo-st ticks modo panel-y nivel-id)
  #:transparent)

(define jugadorr1 (jugador1 85 661 0 0 "red" 0))
(define jugadorr2 (jugador2 85 780 0 0 "blue" 0))

(define (crear-estado-nivel nivel-id)
  (estado jugadorr1 jugadorr2
          diamantes-iniciales
          palanca-inicial botones-iniciales
          plats-moviles-iniciales cubo-inicial
          0
          "jugando"
          PANEL-Y-INICIAL
          nivel-id))

(define estado-inicial
  (estado jugadorr1 jugadorr2
          diamantes-iniciales
          palanca-inicial botones-iniciales
          plats-moviles-iniciales cubo-inicial
          0
          "inicio"
          PANEL-Y-INICIAL
          1))

(define estado-niveles
  (struct-copy estado estado-inicial [modo "niveles"]))

;; ============================================================
;; COLISION AABB GENERICA
;; ============================================================

(define (colision-aabb? ax ay aw ah bx by bw bh)
  (and (< (abs (- ax bx)) (/ (+ aw bw) 2))
       (< (abs (- ay by)) (/ (+ ah bh) 2))))

(define (todas-las-plats plats-mov)
  (append plataformas
          (map (lambda (pm)
                 (plataforma (plat-movil-x pm) (plat-movil-y pm)
                             (plat-movil-width pm) (plat-movil-height pm) "movil"))
               plats-mov)))

(define (jugador-toca-plat? cx cy plat)
  (colision-aabb? cx cy PLAYER-WIDTH PLAYER-HEIGHT
                  (plataforma-x plat) (plataforma-y plat)
                  (plataforma-width plat) (plataforma-height plat)))

(define (tipo-en? tipo tipos)
  (ormap (lambda (t) (string=? tipo t)) tipos))

(define (plataforma-piso? plat)
  (tipo-en? (plataforma-tipo plat)
            (list "suelo" "movil" "lava" "agua" "acido" "cubo")))

(define (plataforma-solida? plat)
  (tipo-en? (plataforma-tipo plat)
            (list "suelo" "movil" "lava" "agua" "acido" "pared" "techo" "cubo")))

(define (bloquea-horizontal? plat)
  (tipo-en? (plataforma-tipo plat)
            (list "pared" "cubo" "movil")))

(define (solape-eje a ta b tb)
  (- (min (+ a (/ ta 2)) (+ b (/ tb 2)))
     (max (- a (/ ta 2)) (- b (/ tb 2)))))

(define (solape-horizontal-jugador-plat? cx plat margen)
  (> (solape-eje cx PLAYER-WIDTH
                 (plataforma-x plat) (plataforma-width plat))
     margen))

(define (jugador-en-suelo? cx cy plats)
  (define pie-y (+ cy (/ PLAYER-HEIGHT 2)))
  (ormap (lambda (plat)
           (define top (- (plataforma-y plat) (/ (plataforma-height plat) 2)))
           (and (plataforma-piso? plat)
                (solape-horizontal-jugador-plat? cx plat 4)
                (<= (abs (- pie-y top)) 6)))
         plats))

(define (aterrizando-sobre? cx y-anterior y-nueva plat)
  (define pie-antes (+ y-anterior (/ PLAYER-HEIGHT 2)))
  (define pie-despues (+ y-nueva (/ PLAYER-HEIGHT 2)))
  (define top (- (plataforma-y plat) (/ (plataforma-height plat) 2)))
  (define bottom (+ (plataforma-y plat) (/ (plataforma-height plat) 2)))
  (define solape-x
    (solape-eje cx PLAYER-WIDTH (plataforma-x plat) (plataforma-width plat)))

  ;; Caso normal: el personaje cae desde arriba y cruza el borde superior.
  (define cruza-desde-arriba?
    (and (<= pie-antes (+ top 4))
         (>= pie-despues (- top 2))))

  ;; Caso de borde: permite alcanzar una plataforma por el costado,
  ;; pero solo si los pies están cerca de la parte superior.
  (define agarre-lateral-valido?
    (and (> solape-x 1)
         (>= pie-despues (- top MARGEN-AGARRE-BORDE))
         (<= pie-despues (+ top MARGEN-AGARRE-BORDE))))

  ;; Caso especial para agua/lava/ácido:
  ;; al bajar por las rampas laterales, los pies pueden quedar un poco
  ;; por debajo del borde superior del líquido. Sin esto, lo atraviesa.
  (define liquido?
    (tipo-en? (plataforma-tipo plat) (list "lava" "agua" "acido")))

  (define entrada-liquido-desde-rampa?
    (and liquido?
         (> solape-x 1)
         (>= pie-despues (- top 2))
         (<= pie-despues (+ bottom 10))))

  (and (plataforma-piso? plat)
       (> solape-x 1)
       (or cruza-desde-arriba?
           agarre-lateral-valido?
           entrada-liquido-desde-rampa?)))

(define (golpea-techo? cx y-anterior y-nueva plat)
  (define cabeza-antes (- y-anterior (/ PLAYER-HEIGHT 2)))
  (define cabeza-despues (- y-nueva (/ PLAYER-HEIGHT 2)))
  (define bottom (+ (plataforma-y plat) (/ (plataforma-height plat) 2)))
  (and (plataforma-solida? plat)
       (solape-horizontal-jugador-plat? cx plat 8)
       (>= cabeza-antes (- bottom 3))
       (<= cabeza-despues bottom)))

(define (jugador-toca-peligro? cx cy tipos-peligro plats)
  (define pie-y (+ cy (/ PLAYER-HEIGHT 2)))
  (ormap (lambda (plat)
           (define top (- (plataforma-y plat) (/ (plataforma-height plat) 2)))
           (and (tipo-en? (plataforma-tipo plat) tipos-peligro)
                (or
                 ;; Contacto real con el volumen del peligro.
                 (colision-aabb? cx cy PLAYER-WIDTH PLAYER-HEIGHT
                                 (plataforma-x plat) (plataforma-y plat)
                                 (plataforma-width plat) (plataforma-height plat))
                 ;; Contacto de pies: cuando el personaje queda exactamente sobre el peligro,
                 ;; el AABB estricto no cuenta colisión, pero sí debe morir.
                 (and (solape-horizontal-jugador-plat? cx plat 1)
                      (<= (abs (- pie-y top)) MARGEN-PIE-PELIGRO)))))
         plats))

(define (limitar v min-v max-v)
  (min max-v (max min-v v)))

(define (frenar-a-cero v cantidad)
  (cond
    [(> v cantidad) (- v cantidad)]
    [(< v (- cantidad)) (+ v cantidad)]
    [else 0]))

(define (actualizar-vel-x vx dir-x)
  (if (= dir-x 0)
      (frenar-a-cero vx FRENADO-X)
      (limitar (+ vx (* dir-x ACEL-X))
               (- VEL-X-MAX)
               VEL-X-MAX)))

;; ============================================================
;; RENDER PLATAFORMAS ESTATICAS
;; ============================================================

(define (dibujar-plataformas lista escena)
  (foldl (lambda (plat esc)
           (place-image
            (rectangle (plataforma-width plat) (plataforma-height plat) "solid"
                       (cond
                         [(string=? (plataforma-tipo plat) "suelo") "transparent"]
                         [(string=? (plataforma-tipo plat) "pared") "transparent"]
                         [(string=? (plataforma-tipo plat) "techo") "transparent"]
                         [(string=? (plataforma-tipo plat) "lava")  "orangered"]
                         [(string=? (plataforma-tipo plat) "agua")  "dodgerblue"]
                         [(string=? (plataforma-tipo plat) "acido") "limegreen"]
                         [else "purple"]))
            (plataforma-x plat) (plataforma-y plat) esc))
         escena lista))

;; ============================================================
;; RENDER RAMPAS — dibuja el triángulo con add-line
;; ============================================================

(define (dibujar-rampas lista escena)
  (foldl (lambda (r esc)
           (define w     (rampa-width r))
           (define h     (rampa-height r))
           (define x-izq (- (rampa-x r) (/ w 2)))
           (define x-der (+ (rampa-x r) (/ w 2)))
           (define y-top (- (rampa-y r) (/ h 2)))
           (define y-bot (+ (rampa-y r) (/ h 2)))
           (define color "transparent")
           (if (string=? (rampa-direccion r) "der")
               ;; /  : vértices inf-izq, inf-der, sup-der
               (add-line
                (add-line
                 (add-line esc x-izq y-bot x-der y-bot color)  ;; base
                 x-der y-bot x-der y-top color)                ;; lado derecho
                x-izq y-bot x-der y-top color)                 ;; hipotenusa
               ;; \  : vértices sup-izq, inf-izq, inf-der
               (add-line
                (add-line
                 (add-line esc x-izq y-top x-izq y-bot color)  ;; lado izquierdo
                 x-izq y-bot x-der y-bot color)                ;; base
                x-izq y-top x-der y-bot color)))               ;; hipotenusa
         escena lista))

;; ============================================================
;; RENDER PLATAFORMAS MOVILES
;; ============================================================

(define (dibujar-plats-moviles lista escena)
  (foldl (lambda (pm esc)
           (place-image
            (rectangle (plat-movil-width pm) (plat-movil-height pm) "solid"
                       (cond
                         [(string=? (plat-movil-color pm) "amarilla") "gold"]
                         [(string=? (plat-movil-color pm) "rosa")     "violet"]
                         [else "gray"]))
            (plat-movil-x pm) (plat-movil-y pm) esc))
         escena lista))

;; ============================================================
;; RENDER PALANCA
;; ============================================================

(define (dibujar-palanca pal escena)
  (place-image
   (overlay (rotate (if (palanca-activada pal) -30 30)
                    (rectangle 6 22 "solid" "goldenrod"))
            (rectangle 20 10 "solid" "darkgoldenrod"))
   (palanca-x pal) (palanca-y pal) escena))

;; ============================================================
;; RENDER BOTONES
;; ============================================================

(define (dibujar-botones lista escena)
  (foldl (lambda (b esc)
           (place-image
            (rectangle 40 10 "solid"
                       (cond
                         [(and (string=? (boton-color b) "amarillo") (boton-activo b)) "yellow"]
                         [(and (string=? (boton-color b) "rosa")     (boton-activo b)) "violet"]
                         [(string=? (boton-color b) "amarillo") "olive"]
                         [else "purple"]))
            (boton-x b) (boton-y b) esc))
         escena lista))

;; ============================================================
;; RENDER CUBO
;; ============================================================

(define (dibujar-cubo c escena)
  (place-image (overlay (rectangle 36 36 "outline" "darkgray")
                        (rectangle 40 40 "solid" "gray"))
               (cubo-x c) (cubo-y c) escena))

;; ============================================================
;; RENDER DIAMANTES
;; ============================================================

(define (dibujar-diamantes lista escena)
  (foldl (lambda (d esc)
           (place-image (rotate 45 (square 14 "solid"
                                           (if (string=? (diamante-tipo d) "rojo") "red" "cyan")))
                        (diamante-x d) (diamante-y d) esc))
         escena lista))

;; ============================================================
;; RENDER PUERTAS
;; ============================================================

;; ============================================================
;; RENDER JUGADORES
;; ============================================================

(define (cara-fireboy)
  (overlay (beside (circle 4 "solid" "white")
                   (rectangle 6 1 "solid" "transparent")
                   (circle 4 "solid" "white"))
           (circle 18 "solid" "red")))

(define (cara-watergirl)
  (overlay (beside (circle 4 "solid" "white")
                   (rectangle 6 1 "solid" "transparent")
                   (circle 4 "solid" "white"))
           (circle 18 "solid" "blue")))

(define (crear_jug1 p scene)
  (place-image (cara-fireboy) (jugador1-x p) (jugador1-y p) scene))

(define (crear_jug2 p scene)
  (place-image (cara-watergirl) (jugador2-x p) (jugador2-y p) scene))


;; ============================================================
;; TEMPORIZADOR
;; ============================================================

(define (dos-digitos n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (formatear-tiempo ticks)
  (define total-segundos (quotient ticks FPS))
  (define minutos (quotient total-segundos 60))
  (define segundos (remainder total-segundos 60))
  (string-append (dos-digitos minutos)
                 ":"
                 (dos-digitos segundos)))

(define (dibujar-temporizador ticks escena)
  (place-image
   (text (formatear-tiempo ticks) 34 "yellow")
   TIMER-X TIMER-Y
   (place-image
    (rectangle TIMER-TAPA-W TIMER-TAPA-H "solid" "black")
    TIMER-X TIMER-Y
    escena)))
;; ============================================================
;; PANTALLA DE VICTORIA
;; ============================================================

;; ============================================================
;; PANELES DE VICTORIA / MUERTE
;; ============================================================

(define (diamantes-completos? est)
  (empty? (estado-diamantes est)))

(define (panel-actual est)
  (cond
    [(string=? (estado-modo est) "muerte")
     panel-muerte]

    [(and (string=? (estado-modo est) "victoria")
          (diamantes-completos? est))
     panel-victoria1]

    [(string=? (estado-modo est) "victoria")
     panel-victoria2]

    [else
     panel-victoria2]))

(define (dibujar-panel-final est)
  (place-image
   (panel-actual est)
   (/ WIDTH 2)
   (estado-panel-y est)
   (empty-scene WIDTH HEIGHT "black")))

;; ============================================================
;; RENDER PRINCIPAL
;; ============================================================

(define (dibujar-menu-inicio est)
  pantalla-inicio)

(define (dibujar-menu-niveles est)
  pantalla-niveles)

(define (dibujar-juego est)
  (dibujar-temporizador
   (estado-ticks est)
   (crear_jug1 (estado-p1 est)
    (crear_jug2 (estado-p2 est)
     (dibujar-palanca (estado-palanca-st est)
      (dibujar-botones (estado-botones est)
       (dibujar-cubo (estado-cubo-st est)
        (dibujar-diamantes (estado-diamantes est)
         (dibujar-plats-moviles (estado-plats-moviles est)
          (dibujar-rampas rampas
           (dibujar-plataformas plataformas
            (place-image (imagen-nivel (estado-nivel-id est))
                         (/ WIDTH 2) (/ HEIGHT 2)
                         (empty-scene WIDTH HEIGHT)))))))))))))

(define (dibujarmundo est)
  (cond
    [(string=? (estado-modo est) "inicio")
     (dibujar-menu-inicio est)]

    [(string=? (estado-modo est) "niveles")
     (dibujar-menu-niveles est)]

    [(or (string=? (estado-modo est) "victoria")
         (string=? (estado-modo est) "muerte"))
     (dibujar-panel-final est)]

    [else
     (dibujar-juego est)]))

;; ============================================================
;; FISICA JUGADOR 1 — FIREBOY (teclas WASD)
;; ============================================================

;; Mueve horizontalmente; bloquea contra paredes
(define (mover-x1 p plats)
  (define nvx (actualizar-vel-x (jugador1-vel-x p) (jugador1-dir-x p)))
  (define nx (+ (jugador1-x p) nvx))
  (define jt (struct-copy jugador1 p [x nx] [vel-x nvx]))
  (define choca?
    (ormap (lambda (plat)
             (and (bloquea-horizontal? plat)
                  (jugador-toca-plat? (jugador1-x jt) (jugador1-y jt) plat)))
           plats))
  (if choca?
      (struct-copy jugador1 p [vel-x 0])
      jt))

;; Aplica gravedad y resuelve colisiones verticales.
;; Importante: aterriza solo si el pie cruza la parte superior de la plataforma.
(define (mover-y1 p plats)
  (define nvy (+ (jugador1-vel-y p) GRAVEDAD))
  (define ny  (+ (jugador1-y p) nvy))
  (define px  (jugador1-x p))

  (define rampa-c
    (findf (lambda (r)
             (and (>= nvy 0)
                  (jugador-aterriza-en-rampa? px (jugador1-y p) ny r)))
           rampas))
  (define suelo-r  (if rampa-c (suelo-rampa-en-x rampa-c px) #f))

  (define piso-c
    (findf (lambda (plat)
             (aterrizando-sobre? px (jugador1-y p) ny plat))
           plats))

  (define techo-c
    (findf (lambda (plat)
             (golpea-techo? px (jugador1-y p) ny plat))
           plats))

  (cond
    [(and rampa-c suelo-r (>= nvy 0))
     (struct-copy jugador1 p
                  [y (- suelo-r (/ PLAYER-HEIGHT 2))]
                  [vel-y 0])]

    [(and piso-c (> nvy 0))
     (struct-copy jugador1 p
                  [y (- (plataforma-y piso-c)
                        (/ (+ PLAYER-HEIGHT (plataforma-height piso-c)) 2))]
                  [vel-y 0])]

    [(and techo-c (< nvy 0))
     (struct-copy jugador1 p
                  [y (+ (plataforma-y techo-c)
                        (/ (+ PLAYER-HEIGHT (plataforma-height techo-c)) 2))]
                  [vel-y 0])]

    [else
     (struct-copy jugador1 p [y ny] [vel-y nvy])]))

;; Fireboy muere en agua y ácido
(define (jugador1-muere? p plats)
  (jugador-toca-peligro? (jugador1-x p) (jugador1-y p)
                         (list "agua" "acido")
                         plats))

(define (actualizar_jugador p plats)
  (define movido (mover-y1 (mover-x1 p plats) plats))
  ;; Resbale: solo sobre rampa, sin input horizontal y sin plataforma plana debajo
  (define resbale (vel-resbale-si-rampa (jugador1-x movido) (jugador1-y movido)))
  (define plano-debajo? (jugador-en-suelo? (jugador1-x movido) (jugador1-y movido) plats))
  (define con-resbale
    (if (and (not (= resbale 0))
             (= (jugador1-dir-x movido) 0)
             (= (jugador1-vel-x movido) 0)
             (not plano-debajo?))
        (struct-copy jugador1 movido [vel-x resbale])
        movido))
    con-resbale)

;; Teclas de movimiento Fireboy: A/D para moverse, W para saltar
(define (mov_jugador1 p key plats)
  (cond
    [(key=? key "a")
     (struct-copy jugador1 p [dir-x -1])]
    [(key=? key "d")
     (struct-copy jugador1 p [dir-x 1])]
    [(and (key=? key "w")
          (or (jugador-en-suelo? (jugador1-x p) (jugador1-y p) plats)
              (rampa-que-toca (jugador1-x p) (jugador1-y p))))
     (struct-copy jugador1 p [vel-y SALTO])]
    [else p]))

(define (parar_jugador p key)
  (cond
    [(and (key=? key "a") (= (jugador1-dir-x p) -1))
     (struct-copy jugador1 p [dir-x 0])]
    [(and (key=? key "d") (= (jugador1-dir-x p) 1))
     (struct-copy jugador1 p [dir-x 0])]
    [else p]))

;; ============================================================
;; FISICA JUGADOR 2 — WATERGIRL (flechas del teclado)
;; ============================================================

(define (mover-x2 p plats)
  (define nvx (actualizar-vel-x (jugador2-vel-x p) (jugador2-dir-x p)))
  (define nx (+ (jugador2-x p) nvx))
  (define jt (struct-copy jugador2 p [x nx] [vel-x nvx]))
  (define choca?
    (ormap (lambda (plat)
             (and (bloquea-horizontal? plat)
                  (jugador-toca-plat? (jugador2-x jt) (jugador2-y jt) plat)))
           plats))
  (if choca?
      (struct-copy jugador2 p [vel-x 0])
      jt))

(define (mover-y2 p plats)
  (define nvy (+ (jugador2-vel-y p) GRAVEDAD))
  (define ny  (+ (jugador2-y p) nvy))
  (define px  (jugador2-x p))

  (define rampa-c
    (findf (lambda (r)
             (and (>= nvy 0)
                  (jugador-aterriza-en-rampa? px (jugador2-y p) ny r)))
           rampas))
  (define suelo-r  (if rampa-c (suelo-rampa-en-x rampa-c px) #f))

  (define piso-c
    (findf (lambda (plat)
             (aterrizando-sobre? px (jugador2-y p) ny plat))
           plats))

  (define techo-c
    (findf (lambda (plat)
             (golpea-techo? px (jugador2-y p) ny plat))
           plats))

  (cond
    [(and rampa-c suelo-r (>= nvy 0))
     (struct-copy jugador2 p
                  [y (- suelo-r (/ PLAYER-HEIGHT 2))]
                  [vel-y 0])]

    [(and piso-c (> nvy 0))
     (struct-copy jugador2 p
                  [y (- (plataforma-y piso-c)
                        (/ (+ PLAYER-HEIGHT (plataforma-height piso-c)) 2))]
                  [vel-y 0])]

    [(and techo-c (< nvy 0))
     (struct-copy jugador2 p
                  [y (+ (plataforma-y techo-c)
                        (/ (+ PLAYER-HEIGHT (plataforma-height techo-c)) 2))]
                  [vel-y 0])]

    [else
     (struct-copy jugador2 p [y ny] [vel-y nvy])]))

;; Watergirl muere en lava y ácido
(define (jugador2-muere? p plats)
  (jugador-toca-peligro? (jugador2-x p) (jugador2-y p)
                         (list "lava" "acido")
                         plats))

(define (actualizar_jugador2 p plats)
  (define movido (mover-y2 (mover-x2 p plats) plats))
  (define resbale (vel-resbale-si-rampa (jugador2-x movido) (jugador2-y movido)))
  (define plano-debajo? (jugador-en-suelo? (jugador2-x movido) (jugador2-y movido) plats))
  (define con-resbale
    (if (and (not (= resbale 0))
             (= (jugador2-dir-x movido) 0)
             (= (jugador2-vel-x movido) 0)
             (not plano-debajo?))
        (struct-copy jugador2 movido [vel-x resbale])
        movido))
    con-resbale)

;; Teclas de movimiento Watergirl: flechas izq/der/arriba
(define (mov_jugador2 p key plats)
  (cond
    [(key=? key "left")
     (struct-copy jugador2 p [dir-x -1])]
    [(key=? key "right")
     (struct-copy jugador2 p [dir-x 1])]
    [(and (key=? key "up")
          (or (jugador-en-suelo? (jugador2-x p) (jugador2-y p) plats)
              (rampa-que-toca (jugador2-x p) (jugador2-y p))))
     (struct-copy jugador2 p [vel-y SALTO])]
    [else p]))

(define (parar_jugador2 p key)
  (cond
    [(and (key=? key "left") (= (jugador2-dir-x p) -1))
     (struct-copy jugador2 p [dir-x 0])]
    [(and (key=? key "right") (= (jugador2-dir-x p) 1))
     (struct-copy jugador2 p [dir-x 0])]
    [else p]))

;; ============================================================
;; LOGICA PALANCA — toggle sin mutación global
;; ============================================================

(define (jugador-sobre-palanca? px py pal)
  (and (< (abs (- px (palanca-x pal))) 25)
       (< (abs (- py (palanca-y pal))) 25)))

(define (actualizar-palanca pal p1 p2)
  (define pisando?
    (or (jugador-sobre-palanca? (jugador1-x p1) (jugador1-y p1) pal)
        (jugador-sobre-palanca? (jugador2-x p2) (jugador2-y p2) pal)))
  (cond
    [(and pisando? (not (palanca-pisada-antes pal)))
     (palanca (palanca-x pal) (palanca-y pal) (not (palanca-activada pal)) #t)]
    [(not pisando?)
     (palanca (palanca-x pal) (palanca-y pal) (palanca-activada pal) #f)]
    [else pal]))

;; ============================================================
;; LOGICA BOTONES
;; ============================================================

(define (jugador-sobre-boton? px py b)
  (colision-aabb? px py PLAYER-WIDTH PLAYER-HEIGHT
                  (boton-x b) (boton-y b) 40 10))

(define (actualizar-botones botones p1 p2)
  (map (lambda (b)
         (boton (boton-x b) (boton-y b) (boton-color b)
                (or (jugador-sobre-boton? (jugador1-x p1) (jugador1-y p1) b)
                    (jugador-sobre-boton? (jugador2-x p2) (jugador2-y p2) b))))
       botones))

;; ============================================================
;; LOGICA PLATAFORMAS MOVILES
;; ============================================================

(define VEL-PLAT 1.5)

(define (mover-hacia pm objetivo-y)
  (define dy (- objetivo-y (plat-movil-y pm)))
  (define paso (cond [(> dy VEL-PLAT)        VEL-PLAT]
                     [(< dy (- VEL-PLAT)) (- VEL-PLAT)]
                     [else dy]))
  (plat-movil (plat-movil-x pm) (+ (plat-movil-y pm) paso)
              (plat-movil-width pm) (plat-movil-height pm)
              (plat-movil-color pm) (plat-movil-bajada pm)
              (plat-movil-y-arriba pm) (plat-movil-y-abajo pm)))

(define (actualizar-plats-moviles plats pal botones)
  (map (lambda (pm)
         (cond
           [(string=? (plat-movil-color pm) "amarilla")
            (mover-hacia pm (if (palanca-activada pal)
                                (plat-movil-y-abajo pm)
                                (plat-movil-y-arriba pm)))]
           [(string=? (plat-movil-color pm) "rosa")
            (mover-hacia pm (if (ormap boton-activo botones)
                                (plat-movil-y-abajo pm)
                                (plat-movil-y-arriba pm)))]
           [else pm]))
       plats))

;; ============================================================
;; LOGICA CUBO EMPUJABLE
;; ============================================================

(define CUBO-W 40)
(define CUBO-H 40)
(define CUBO-GRAVEDAD 3.5)

(define (cubo->plataforma c)
  (plataforma (cubo-x c) (cubo-y c) CUBO-W CUBO-H "cubo"))

(define (jugador-empuja-cubo? px py pvel-x cx cy)
  (define px-siguiente (+ px pvel-x))
  (and (not (= pvel-x 0))
       (> (solape-eje py PLAYER-HEIGHT cy CUBO-H) 8)
       (< (abs (- px-siguiente cx)) (/ (+ PLAYER-WIDTH CUBO-W) 2))
       (or (and (> pvel-x 0) (< px cx))
           (and (< pvel-x 0) (> px cx)))))

(define (empuje-jugador1 p c)
  (define vx (actualizar-vel-x (jugador1-vel-x p) (jugador1-dir-x p)))
  (if (jugador-empuja-cubo? (jugador1-x p) (jugador1-y p) vx
                             (cubo-x c) (cubo-y c))
      vx
      0))

(define (empuje-jugador2 p c)
  (define vx (actualizar-vel-x (jugador2-vel-x p) (jugador2-dir-x p)))
  (if (jugador-empuja-cubo? (jugador2-x p) (jugador2-y p) vx
                             (cubo-x c) (cubo-y c))
      vx
      0))

(define (cubo-aterriza-sobre? cx y-anterior y-nueva plat)
  (define pie-antes (+ y-anterior (/ CUBO-H 2)))
  (define pie-despues (+ y-nueva (/ CUBO-H 2)))
  (define top (- (plataforma-y plat) (/ (plataforma-height plat) 2)))
  (and (plataforma-piso? plat)
       (> (solape-eje cx CUBO-W (plataforma-x plat) (plataforma-width plat)) 8)
       (<= pie-antes (+ top 3))
       (>= pie-despues top)))

(define (actualizar-cubo c p1 p2 plats)
  (define empuje-directo
    (+ (empuje-jugador1 p1 c)
       (empuje-jugador2 p2 c)))
  (define empuje
    (if (= empuje-directo 0)
        (* (cubo-vel-x c) 0.82)
        (limitar empuje-directo (- VEL-X-MAX) VEL-X-MAX)))

  (define intento-x (+ (cubo-x c) empuje))

  (define choca-pared?
    (ormap (lambda (plat)
             (and (string=? (plataforma-tipo plat) "pared")
                  (colision-aabb? intento-x (cubo-y c) CUBO-W CUBO-H
                                  (plataforma-x plat) (plataforma-y plat)
                                  (plataforma-width plat) (plataforma-height plat))))
           plats))

  (define nuevo-x (if choca-pared? (cubo-x c) intento-x))
  (define nuevo-vx (if choca-pared? 0 empuje))

  (define intento-y (+ (cubo-y c) CUBO-GRAVEDAD))
  (define piso
    (findf (lambda (plat)
             (cubo-aterriza-sobre? nuevo-x (cubo-y c) intento-y plat))
           plats))

  (define nuevo-y
    (if piso
        (- (plataforma-y piso)
           (/ (plataforma-height piso) 2)
           (/ CUBO-H 2))
        intento-y))

  (cubo nuevo-x nuevo-y nuevo-vx))

;; ============================================================
;; LOGICA DIAMANTES
;; ============================================================

(define RADIO-DIAMANTE 22)

(define (cerca-diamante? px py dx dy)
  (< (sqrt (+ (expt (- px dx) 2) (expt (- py dy) 2))) RADIO-DIAMANTE))

(define (actualizar-diamantes diams p1 p2)
  (filter (lambda (d)
            (not (or (and (string=? (diamante-tipo d) "rojo")
                          (cerca-diamante? (jugador1-x p1) (jugador1-y p1)
                                           (diamante-x d) (diamante-y d)))
                     (and (string=? (diamante-tipo d) "azul")
                          (cerca-diamante? (jugador2-x p2) (jugador2-y p2)
                                           (diamante-x d) (diamante-y d))))))
          diams))

;; ============================================================
;; LOGICA DE VICTORIA POR COORDENADAS
;; ============================================================

(define (cerca-meta? px py mx my)
  (and (< (abs (- px mx)) RADIO-META)
       (< (abs (- py my)) RADIO-META)))

(define (nivel-completo? p1 p2)
  (and (cerca-meta? (jugador1-x p1) (jugador1-y p1)
                    META-FIREBOY-X META-FIREBOY-Y)
       (cerca-meta? (jugador2-x p2) (jugador2-y p2)
                    META-WATERGIRL-X META-WATERGIRL-Y)))

;; ============================================================
;; HANDLERS DEL BIG-BANG
;; ============================================================

(define (plats-totales est)
  (cons (cubo->plataforma (estado-cubo-st est))
        (todas-las-plats (estado-plats-moviles est))))

(define (mov_ambos est key)
  (cond
    ;; Reinicio rápido para probar después de morir o ganar.
    [(and (or (string=? (estado-modo est) "muerte")
              (string=? (estado-modo est) "victoria"))
          (key=? key "r"))
     (crear-estado-nivel (estado-nivel-id est))]

    [(key=? key "escape")
     estado-inicial]

    [(not (string=? (estado-modo est) "jugando"))
     est]

    [else
     (define plats (plats-totales est))
     (struct-copy estado est
                  [p1 (mov_jugador1 (estado-p1 est) key plats)]
                  [p2 (mov_jugador2 (estado-p2 est) key plats)])]))

(define (actualizar-panel-y y)
  (min PANEL-Y-FINAL (+ y VEL-PANEL)))

(define (en-rect-centro? x y cx cy w h)
  (and (<= (abs (- x cx)) (/ w 2))
       (<= (abs (- y cy)) (/ h 2))))

(define (en-circulo? x y cx cy r)
  (<= (+ (sqr (- x cx)) (sqr (- y cy)))
      (sqr r)))

(define (en-boton-panel? est x y local-x local-y w h)
  (define panel (panel-actual est))
  (define panel-left (- (/ WIDTH 2) (/ (image-width panel) 2)))
  (define panel-top  (- (estado-panel-y est) (/ (image-height panel) 2)))
  (and (>= x (+ panel-left local-x))
       (<= x (+ panel-left local-x w))
       (>= y (+ panel-top local-y))
       (<= y (+ panel-top local-y h))))

(define (manejar-mouse est x y evento)
  (if (not (string=? evento "button-down"))
      est
      (cond
        [(string=? (estado-modo est) "inicio")
         (if (en-rect-centro? x y
                              BOTON-PLAY-X BOTON-PLAY-Y
                              BOTON-PLAY-W BOTON-PLAY-H)
             estado-niveles
             est)]

        [(string=? (estado-modo est) "niveles")
         (cond
           [(en-circulo? x y BOTON-NIVEL1-X BOTON-NIVEL1-Y BOTON-NIVEL-R)
            (crear-estado-nivel 1)]

           [(en-circulo? x y BOTON-NIVEL2-X BOTON-NIVEL2-Y BOTON-NIVEL-R)
            (crear-estado-nivel 2)]

           [(en-rect-centro? x y
                             BOTON-VOLVER-X BOTON-VOLVER-Y
                             BOTON-VOLVER-W BOTON-VOLVER-H)
            estado-inicial]

           [else est])]

        [(string=? (estado-modo est) "muerte")
         (cond
           [(en-boton-panel? est x y
                             PANEL-MUERTE-MENU-X PANEL-MUERTE-MENU-Y
                             PANEL-BOTON-W PANEL-BOTON-H)
            estado-niveles]

           [(en-boton-panel? est x y
                             PANEL-MUERTE-REINTENTAR-X PANEL-MUERTE-REINTENTAR-Y
                             PANEL-BOTON-W PANEL-BOTON-H)
            (crear-estado-nivel (estado-nivel-id est))]

           [(en-boton-panel? est x y
                             PANEL-MUERTE-SALIR-X PANEL-MUERTE-SALIR-Y
                             PANEL-BOTON-W PANEL-BOTON-H)
            estado-inicial]

           [else est])]

        [(string=? (estado-modo est) "victoria")
         (if (en-boton-panel? est x y
                               PANEL-VICTORIA-CONTINUAR-X
                               PANEL-VICTORIA-CONTINUAR-Y
                               PANEL-VICTORIA-CONTINUAR-W
                               PANEL-VICTORIA-CONTINUAR-H)
             estado-niveles
             est)]

        [else est])))

(define (actualizar_ambos est)
  (cond
    [(or (string=? (estado-modo est) "victoria")
         (string=? (estado-modo est) "muerte"))
     (struct-copy estado est
                  [panel-y (actualizar-panel-y (estado-panel-y est))])]

    [(not (string=? (estado-modo est) "jugando"))
     est]

    [else
     (let* ([p1             (estado-p1 est)]
            [p2             (estado-p2 est)]
            [nueva-pal      (actualizar-palanca       (estado-palanca-st est) p1 p2)]
            [nuevos-bots    (actualizar-botones       (estado-botones est) p1 p2)]
            [nuevas-pm      (actualizar-plats-moviles (estado-plats-moviles est) nueva-pal nuevos-bots)]
            [plats-cubo     (todas-las-plats nuevas-pm)]
            [nuevo-cubo     (actualizar-cubo          (estado-cubo-st est) p1 p2 plats-cubo)]
            [plats-final    (cons (cubo->plataforma nuevo-cubo) (todas-las-plats nuevas-pm))]
            [nuevo-p1       (actualizar_jugador  p1 plats-final)]
            [nuevo-p2       (actualizar_jugador2 p2 plats-final)]
            [nuevos-diams   (actualizar-diamantes (estado-diamantes est) nuevo-p1 nuevo-p2)]
            [murio?
             (or (jugador1-muere? nuevo-p1 plats-final)
                 (jugador2-muere? nuevo-p2 plats-final))]
            [gano?
             (nivel-completo? nuevo-p1 nuevo-p2)]
            [nuevo-modo
             (cond
               [murio? "muerte"]
               [gano?  "victoria"]
               [else   "jugando"])]
            [nuevos-ticks
             (if (string=? nuevo-modo "jugando")
                 (add1 (estado-ticks est))
                 (estado-ticks est))]
            [nuevo-panel-y
             (if (string=? nuevo-modo "jugando")
                 (estado-panel-y est)
                 PANEL-Y-INICIAL)])
       (struct-copy estado est
                    [p1 nuevo-p1]
                    [p2 nuevo-p2]
                    [diamantes nuevos-diams]
                    [palanca-st nueva-pal]
                    [botones nuevos-bots]
                    [plats-moviles nuevas-pm]
                    [cubo-st nuevo-cubo]
                    [ticks nuevos-ticks]
                    [modo nuevo-modo]
                    [panel-y nuevo-panel-y]))]))

(define (parar_ambos est key)
  (if (not (string=? (estado-modo est) "jugando"))
      est
      (struct-copy estado est
                   [p1 (parar_jugador  (estado-p1 est) key)]
                   [p2 (parar_jugador2 (estado-p2 est) key)])))

;; ============================================================
;; RUN
;; ============================================================

(big-bang estado-inicial
  (to-draw    dibujarmundo)
  (on-key     mov_ambos)
  (on-tick    actualizar_ambos 1/30)
  (on-release parar_ambos)
  (on-mouse   manejar-mouse))