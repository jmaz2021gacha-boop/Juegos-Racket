#lang racket

(require 2htdp/universe)
(require 2htdp/image)

;; =====================================================
;; CONFIGURACION
;; =====================================================

(define ANCHO 1000)
(define ALTO 700)

(define VELOCIDAD 2)
(define GIRO 3)
(define VELOCIDAD-BALA 8)
(define COOLDOWN-DISPARO 60)
(define RADIO-BALA 4)
(define SUBPASOS 4)

;; =====================================================
;; ESTRUCTURAS
;; =====================================================
(struct muro (x y ancho alto) #:transparent)

(struct tanque
  (x y angulo velocidad vida color recarga)
  #:mutable
  #:transparent)

(struct bala (x y dx dy vida)
  #:mutable
  #:transparent)

(struct mundo (jugador1 jugador2 teclas balas muros ganador mapa)
  #:mutable
  #:transparent)

;; =====================================================
;; MAPAS
;; =====================================================

;; El codigo guardaba muros como: x y ancho alto,
;; usando x/y como esquina superior izquierda. Aqui se convierten a centro.
(define (muros x y ancho alto)
  (muro (+ x (/ ancho 2))
        (+ y (/ alto 2))
        ancho
        alto))

(define muros-mapa1
  (list
   (muros 100 100 10 500)
   (muros 100 600 810 10)
   (muros 100 100 800 10)
   (muros 900 100 10 500)
   (muros 200 100 8 300)
   (muros 200 200 150 8)
   (muros 500 500 8 100)
   (muros 500 500 300 8)
   (muros 800 300 8 208)
   (muros 100 500 300 8)
   (muros 600 100 8 100)
   (muros 450 200 358 8)
   (muros 200 400 308 8)
   (muros 400 300 8 100)))

(define muros-mapa2
  (list
   (muros 300 100 300 10)
   (muros 300 100 10 500)
   (muros 300 600 310 10)
   (muros 600 100 10 500)
   (muros 300 400 100 8)
   (muros 500 100 8 200)
   (muros 300 500 100 8)
   (muros 500 500 100 8)
   (muros 300 200 100 8)
   (muros 400 200 8 120)))

(define muros-mapa3
  (list
   (muros 100 100 10 500)
   (muros 100 600 810 10)
   (muros 100 100 800 10)
   (muros 900 100 10 500)
   (muros 200 100 8 200)
   (muros 400 500 150 8)
   (muros 500 300 8 100)
   (muros 500 400 8 100)
   (muros 800 300 8 208)
   (muros 100 500 200 8)
   (muros 600 100 8 100)
   (muros 450 200 358 8)
   (muros 200 400 208 8)
   (muros 400 300 8 100)
   (muros 600 500 8 100)))

(define (mapa-aleatorio)
  (+ (random 3) 1))

(define (muros-del-mapa numero-mapa)
  (cond
    [(= numero-mapa 1) muros-mapa1]
    [(= numero-mapa 2) muros-mapa2]
    [(= numero-mapa 3) muros-mapa3]
    [else muros-mapa1]))

(define (posiciones-iniciales numero-mapa)
  (cond
    [(= numero-mapa 2)
     (list 350 150 550 550)]
    [else
     (list 150 150 850 550)]))

;; =====================================================
;; ESTADO INICIAL
;; =====================================================

(define (crear-tanque x y angulo color)
  (tanque x y angulo 0 1 color 0))

(define (crear-mundo)
  (define numero-mapa (mapa-aleatorio))
  (define posiciones (posiciones-iniciales numero-mapa))
  (define j1-x (list-ref posiciones 0))
  (define j1-y (list-ref posiciones 1))
  (define j2-x (list-ref posiciones 2))
  (define j2-y (list-ref posiciones 3))
  (mundo
   (crear-tanque j1-x j1-y 0 "blue")
   (crear-tanque j2-x j2-y 180 "red")
   '()
   '()
   (muros-del-mapa numero-mapa)
   #f
   numero-mapa))

;; =====================================================
;; BALAS
;; =====================================================

(define (crear-bala t)
  (define ang (degrees->radians (- (tanque-angulo t))))
  (define distancia-cañon 20)
  (bala
   (+ (tanque-x t) (* distancia-cañon (cos ang)))
   (- (tanque-y t) (* distancia-cañon (sin ang)))
   (* VELOCIDAD-BALA (cos ang))
   (* VELOCIDAD-BALA (- (sin ang)))
   300))

(define (disparar un-mundo t)
  (when (= (tanque-recarga t) 0)
    (set-mundo-balas!
     un-mundo
     (cons (crear-bala t)
           (mundo-balas un-mundo)))
    (set-tanque-recarga! t COOLDOWN-DISPARO)))

(define (actualizar-recarga t)
  (when (> (tanque-recarga t) 0)
    (set-tanque-recarga! t (- (tanque-recarga t) 1))))

;; =====================================================
;; DIBUJO
;; =====================================================

(define fondo
  (empty-scene ANCHO ALTO))

(define (dibujar-muro m escena)
  (place-image
   (rectangle (muro-ancho m) (muro-alto m) "solid" "gray")
   (muro-x m)
   (muro-y m)
   escena))

(define (dibujar-muros lista escena)
  (cond
    [(empty? lista) escena]
    [else
     (dibujar-muros (rest lista)
                    (dibujar-muro (first lista) escena))]))

(define (dibujar-bala b escena)
  (place-image
   (circle RADIO-BALA "solid" "black")
   (bala-x b)
   (bala-y b)
   escena))

(define (dibujar-balas lista escena)
  (cond
    [(empty? lista) escena]
    [else
     (dibujar-balas (rest lista)
                    (dibujar-bala (first lista) escena))]))

(define (imagen-tanque color)
  (overlay
   (rectangle 24 8 "solid" "black")
   (rectangle 30 20 "solid" color)))

(define (dibujar-tanque t escena)
  (place-image
   (rotate (- (tanque-angulo t))
           (imagen-tanque (tanque-color t)))
   (tanque-x t)
   (tanque-y t)
   escena))

(define (dibujar un-mundo)
  (define escena
    (dibujar-balas
     (mundo-balas un-mundo)
     (dibujar-tanque
      (mundo-jugador2 un-mundo)
      (dibujar-tanque
       (mundo-jugador1 un-mundo)
       (dibujar-muros (mundo-muros un-mundo) fondo)))))

  (define escena-con-mapa
    (place-image
     (text (string-append "Mapa " (number->string (mundo-mapa un-mundo))) 18 "black")
     60
     25
     escena))

  (if (mundo-ganador un-mundo)
      (place-image
       (above
        (text (string-append "GANA " (mundo-ganador un-mundo)) 40 "black")
        (text "Presiona R para reiniciar" 25 "black"))
       (/ ANCHO 2)
       (/ ALTO 2)
       escena-con-mapa)
      escena-con-mapa))

;; =====================================================
;; COLISIONES
;; =====================================================

(define (choca? x y ancho alto m)
  (and
   (< (- x (/ ancho 2)) (+ (muro-x m) (/ (muro-ancho m) 2)))
   (> (+ x (/ ancho 2)) (- (muro-x m) (/ (muro-ancho m) 2)))
   (< (- y (/ alto 2)) (+ (muro-y m) (/ (muro-alto m) 2)))
   (> (+ y (/ alto 2)) (- (muro-y m) (/ (muro-alto m) 2)))))

(define (choca-muros? x y lista-muros)
  (cond
    [(empty? lista-muros) #f]
    [(choca? x y 20 20 (first lista-muros)) #t]
    [else (choca-muros? x y (rest lista-muros))]))

(define (bala-choca-muro? b m)
  (and
   (> (+ (bala-x b) RADIO-BALA) (- (muro-x m) (/ (muro-ancho m) 2)))
   (< (- (bala-x b) RADIO-BALA) (+ (muro-x m) (/ (muro-ancho m) 2)))
   (> (+ (bala-y b) RADIO-BALA) (- (muro-y m) (/ (muro-alto m) 2)))
   (< (- (bala-y b) RADIO-BALA) (+ (muro-y m) (/ (muro-alto m) 2)))))

(define (muro-impactado b lista)
  (cond
    [(empty? lista) #f]
    [(bala-choca-muro? b (first lista)) (first lista)]
    [else (muro-impactado b (rest lista))]))

(define (rebote-bala-muro b m)
  (define overlap-x
    (- (+ (/ (muro-ancho m) 2) RADIO-BALA)
       (abs (- (bala-x b) (muro-x m)))))
  (define overlap-y
    (- (+ (/ (muro-alto m) 2) RADIO-BALA)
       (abs (- (bala-y b) (muro-y m)))))
  (if (< overlap-x overlap-y)
      (set-bala-dx! b (- (bala-dx b)))
      (set-bala-dy! b (- (bala-dy b)))))

(define (bala-choca-tanque? b t)
  (and
   (> (+ (bala-x b) RADIO-BALA)
      (- (tanque-x t) 15))
   (< (- (bala-x b) RADIO-BALA)
      (+ (tanque-x t) 15))
   (> (+ (bala-y b) RADIO-BALA)
      (- (tanque-y t) 10))
   (< (- (bala-y b) RADIO-BALA)
      (+ (tanque-y t) 10))))

(define (revisar-impactos mundo)
  (for-each
   (lambda (b)
     (when (bala-choca-tanque?
            b
            (mundo-jugador1 mundo))
       (set-mundo-ganador!
        mundo "ROJO"))
     (when (bala-choca-tanque?
            b
            (mundo-jugador2 mundo))
       (set-mundo-ganador!
        mundo
        "AZUL")))
   (mundo-balas mundo)))

;; =====================================================
;; TECLADO
;; =====================================================

(define (tecla-presionada? tecla lista)
  (member tecla lista))

(define (agregar-tecla tecla lista)
  (if (member tecla lista)
      lista
      (cons tecla lista)))

(define (quitar-tecla tecla lista)
  (filter (lambda (x) (not (equal? x tecla)))
          lista))

(define (al-presionar un-mundo tecla)
  (if (equal? tecla "r")
      (crear-mundo)
      (begin
        (set-mundo-teclas!
         un-mundo
         (agregar-tecla tecla (mundo-teclas un-mundo)))

        (when (equal? tecla "q")
          (disparar un-mundo (mundo-jugador1 un-mundo)))

        (when (equal? tecla "\r")
          (disparar un-mundo (mundo-jugador2 un-mundo)))

        un-mundo)))

(define (al-soltar un-mundo tecla)
  (set-mundo-teclas!
   un-mundo
   (quitar-tecla tecla (mundo-teclas un-mundo)))
  un-mundo)

;; =====================================================
;; MOVIMIENTO
;; =====================================================

(define (intentar-mover-tanque! t nx ny lista-muros)
  (unless (choca-muros? nx ny lista-muros)
    (set-tanque-x! t nx)
    (set-tanque-y! t ny)))

(define (mover-adelante! t lista-muros)
  (define ang (degrees->radians (- (tanque-angulo t))))
  (define nx (+ (tanque-x t) (* VELOCIDAD (cos ang))))
  (define ny (- (tanque-y t) (* VELOCIDAD (sin ang))))
  (intentar-mover-tanque! t nx ny lista-muros))

(define (mover-atras! t lista-muros)
  (define ang (degrees->radians (- (tanque-angulo t))))
  (define nx (- (tanque-x t) (* VELOCIDAD (cos ang))))
  (define ny (+ (tanque-y t) (* VELOCIDAD (sin ang))))
  (intentar-mover-tanque! t nx ny lista-muros))

(define (mover-tanque1! t teclas lista-muros)
  (when (tecla-presionada? "w" teclas)
    (mover-adelante! t lista-muros))
  (when (tecla-presionada? "s" teclas)
    (mover-atras! t lista-muros))
  (when (tecla-presionada? "d" teclas)
    (set-tanque-angulo! t (+ (tanque-angulo t) GIRO)))
  (when (tecla-presionada? "a" teclas)
    (set-tanque-angulo! t (- (tanque-angulo t) GIRO))))

(define (mover-tanque2! t teclas lista-muros)
  (when (tecla-presionada? "up" teclas)
    (mover-adelante! t lista-muros))
  (when (tecla-presionada? "down" teclas)
    (mover-atras! t lista-muros))
  (when (tecla-presionada? "right" teclas)
    (set-tanque-angulo! t (+ (tanque-angulo t) GIRO)))
  (when (tecla-presionada? "left" teclas)
    (set-tanque-angulo! t (- (tanque-angulo t) GIRO))))

(define (mover-bala-subpaso b lista-muros)
  (define nx (+ (bala-x b) (/ (bala-dx b) SUBPASOS)))
  (define ny (+ (bala-y b) (/ (bala-dy b) SUBPASOS)))
  (define bala-futura
    (bala nx ny (bala-dx b) (bala-dy b) (bala-vida b)))
  (define impacto (muro-impactado bala-futura lista-muros))
  (if impacto
      (rebote-bala-muro b impacto)
      (begin
        (set-bala-x! b nx)
        (set-bala-y! b ny))))

(define (mover-bala-n b lista-muros n)
  (unless (= n 0)
    (mover-bala-subpaso b lista-muros)
    (mover-bala-n b lista-muros (- n 1))))

(define (actualizar-bala b lista-muros)
  (mover-bala-n b lista-muros SUBPASOS)

  (when (or (< (bala-x b) 0)
            (> (bala-x b) ANCHO))
    (set-bala-dx! b (- (bala-dx b))))

  (when (or (< (bala-y b) 0)
            (> (bala-y b) ALTO))
    (set-bala-dy! b (- (bala-dy b))))

  (set-bala-vida! b (- (bala-vida b) 1)))

(define (actualizar-balas lista lista-muros)
  (cond
    [(empty? lista) '()]
    [else
     (let ([b (first lista)])
       (actualizar-bala b lista-muros)
       (if (<= (bala-vida b) 0)
           (actualizar-balas (rest lista) lista-muros)
           (cons b (actualizar-balas (rest lista) lista-muros))))]))

;; =====================================================
;; ACTUALIZACION
;; =====================================================

(define (actualizar un-mundo)
  (if (mundo-ganador un-mundo)
      un-mundo
      (begin
        (mover-tanque1!
         (mundo-jugador1 un-mundo)
         (mundo-teclas un-mundo)
         (mundo-muros un-mundo))

        (mover-tanque2!
         (mundo-jugador2 un-mundo)
         (mundo-teclas un-mundo)
         (mundo-muros un-mundo))

        (actualizar-recarga (mundo-jugador1 un-mundo))
        (actualizar-recarga (mundo-jugador2 un-mundo))

        (set-mundo-balas!
         un-mundo
         (actualizar-balas
          (mundo-balas un-mundo)
          (mundo-muros un-mundo)))

        (revisar-impactos un-mundo)
        un-mundo)))

;; =====================================================
;; JUEGO
;; =====================================================

(big-bang (crear-mundo)
  [to-draw dibujar]
  [on-key al-presionar]
  [on-release al-soltar]
  [on-tick actualizar (/ 1 60)])