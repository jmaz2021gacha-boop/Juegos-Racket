(require 2htdp/universe)
(require 2htdp/image)

(define CELDA    64)
(define ESPACIO   8)
(define PASO     (+ CELDA ESPACIO))
(define MARGEN   64)
(define ANCHO   640)
(define ALTO    720)
(define ANCHO-LINEA 12)

(define FONDO        (make-color  28  32  42))
(define CELDA-GRIS   (make-color  58  65  82))
(define COLOR-TEXTO  (make-color 200 205 215))
(define CAPA-OSCURA  (make-color  20  22  32 220))

(define COLORES
  (vector
   (make-color 220  75  55)
   (make-color 120 195  60)
   (make-color  65 145 230)
   (make-color 225 150  40)
   (make-color 165  75 225)))

(define MATRICES-NIVELES
  (list
   (vector (vector -1 -1  0  0 -1 -1)
           (vector  0  0  0  0  0 -1)
           (vector  0 -1  0  0  0  0)
           (vector  0  0  1  0  0 -1) 
           (vector -1  0  0  0  0 -1)
           (vector -1  0  0  0  0 -1))
   
   (vector (vector -1  0 -1 -1  0  0  0)
           (vector  0  0 -1  1  0  0  0) 
           (vector  0 -1 -1  0  0  0  0)
           (vector  0 -1 -1 -1  0  0  0)
           (vector  0  0 -1  0  0 -1  0)
           (vector -1  0 -1  0  0  0  0)
           (vector -1  0  0  0  0  0 -1))
   
   (vector (vector  0  0  0  0  0  0)
           (vector  0  0  0 -1  0  0)
           (vector  0 -1  0  0  0 -1)
           (vector  0  0  0  0  0  0) 
           (vector  0  0 -1  0 -1  0)
           (vector  0  0 -1  0  0  0)
           (vector -1 -1 -1  0  0 -1)
           (vector -1  1  0  0  0  0))
   
   (vector (vector  0  0  0  0  0  0  0)
           (vector  0 -1 -1 -1 -1  0  0)
           (vector -1 -1 -1 -1  0  0 -1) 
           (vector -1 -1  0  0  0  0  0)
           (vector  0  0  0  1 -1  0  0)
           (vector  0 -1 -1  0  0  0  0)
           (vector  0  0  0 -1  0  0  0)
           (vector  0  0  0  0  0 -1 -1))

   (vector (vector  0  0  0  0  0  0  0)
           (vector  0  0 -1 -1 -1 -1  0)
           (vector  0  0  0  0 -1 -1  0)
           (vector  0 -1  0  0 -1  0  0) 
           (vector  0  0  0  0  0  0 -1)
           (vector -1  0  0  0 -1  1  0)
           (vector  0  0 -1  0  0 -1  0)
           (vector  0  0  0 -1  0  0  0))
   ))

(define-struct estado-juego (nivel celdas visitadas celda-inicio arrastrando mouse-x mouse-y fase confeti reloj))
(define-struct particula (x y vx vy color tamano vida))
(define (obtener-ultimo lista)
  (cond
    [(null? lista) '()]
    [(null? (cdr lista)) (car lista)]
    [else (obtener-ultimo (cdr lista))]))
(define (eliminar-ultimo lista)
  (cond
    [(null? lista) '()]
    [(null? (cdr lista)) '()]
    [else (cons (car lista) (eliminar-ultimo (cdr lista)))]))
(define (convertir-matriz matriz)
  (define filas (vector-length matriz))
  (define columnas (vector-length (vector-ref matriz 0)))
  (define (recorrer f c)
    (cond
      [(>= f filas) '()]
      [(>= c columnas) (recorrer (+ f 1) 0)]
      [else
       (let ([valor (vector-ref (vector-ref matriz f) c)])
         (if (>= valor 0)
             (cons (list f c) (recorrer f (+ c 1)))
             (recorrer f (+ c 1))))]))(recorrer 0 0))

(define (buscar-inicio-matriz matriz)
  (define filas (vector-length matriz))
  (define columnas (vector-length (vector-ref matriz 0)))
  (define (recorrer f c)
    (cond
      [(>= f filas) '()]
      [(>= c columnas) (recorrer (+ f 1) 0)]
      [else
       (let ([valor (vector-ref (vector-ref matriz f) c)])
         (if (= valor 1)
             (list f c)
             (recorrer f (+ c 1))))]))(recorrer 0 0))

(define (nuevo-juego lv)
  (define matriz (list-ref MATRICES-NIVELES lv))
  (define celdas-validas (convertir-matriz matriz))
  (define celda-inicial (buscar-inicio-matriz matriz))
  (make-estado-juego lv celdas-validas (list celda-inicial) celda-inicial #f 0 0 "jugando" '() 0))

(define (estado-menu)
  (make-estado-juego 0 '() '() '() #f 0 0 "menu" '() 0))

(define (coordenada-x col) (+ MARGEN (* col PASO) (/ CELDA 2)))
(define (coordenada-y fila) (+ MARGEN (* fila PASO) (/ CELDA 2) 55))

(define (colisiona? x y fila col)
  (define origen-x (+ MARGEN (* col PASO)))
  (define origen-y (+ MARGEN (* fila PASO) 55))
  (and (>= x origen-x) (< x (+ origen-x CELDA))
       (>= y origen-y) (< y (+ origen-y CELDA))))

(define (buscar-colision x y celdas)
  (cond
    [(null? celdas) #f]
    [(colisiona? x y (caar celdas) (cadar celdas)) (car celdas)]
    [else (buscar-colision x y (cdr celdas))]))

(define (adyacente? a b)
  (= 1 (+ (abs (- (car a) (car b)))
          (abs (- (cadr a) (cadr b))))))

(define (siguiente-valido? c visitadas celdas)
  (and (member c celdas)
       (not (member c visitadas))
       (or (null? visitadas)
           (adyacente? c (obtener-ultimo visitadas)))))

(define (color-aleatorio)
  (define COLORES-CONFETI
    (list (make-color 255  70  70) (make-color 255 210   0)
          (make-color  60 200  60) (make-color  60 160 255)
          (make-color 210  60 255) (make-color 255 130   0)))
  (list-ref COLORES-CONFETI (random (length COLORES-CONFETI))))

(define (generar-confetti-recursivo n)
  (if (<= n 0)
      '()
      (cons (make-particula (+ 60 (random (- 640 120))) (+ 80 (random 150))
                             (- (random 7) 3) (+ 1 (random 5))
                             (color-aleatorio) (+ 5 (random 9)) (+ 90 (random 50)))
            (generar-confetti-recursivo (- n 1)))))

(define (crear-confetti) (generar-confetti-recursivo 90))

(define (actualizar-confetti lista-p)
  (cond
    [(null? lista-p) '()]
    [(> (particula-vida (car lista-p)) 0)
     (let ([p (car lista-p)])
       (cons (make-particula (+ (particula-x p) (particula-vx p)) (+ (particula-y p) (particula-vy p))
                              (particula-vx p) (+ (particula-vy p) 0.12)
                              (particula-color p) (particula-tamano p) (- (particula-vida p) 1))
             (actualizar-confetti (cdr lista-p))))]
    [else (actualizar-confetti (cdr lista-p))]))

(define (dibujar-celda img fila col color)
  (define px (coordenada-x col))
  (define py (coordenada-y fila))
  (place-image
   (overlay
    (rectangle (- CELDA 6) (- CELDA 6) 'solid color)
    (rectangle CELDA CELDA 'solid (make-color (color-red FONDO) (color-green FONDO) (color-blue FONDO))))
   px py img))

(define (dibujar-segmento img x1 y1 x2 y2 col)
  (add-line img x1 y1 x2 y2 (pen col ANCHO-LINEA 'solid 'round 'round)))

(define (crear-boton etiqueta color-b ancho-b alto-b)
  (overlay
   (text etiqueta 17 'white)
   (rectangle (- ancho-b 4) (- alto-b 4) 'solid color-b)
   (rectangle ancho-b alto-b 'solid (make-color 18 20 30))))

(define (dibujar-botones-menu img indice)
  (if (>= indice 5)
      img
      (let ([col (vector-ref COLORES indice)])
        (dibujar-botones-menu
         (place-image
          (crear-boton (string-append "Nivel " (number->string (+ indice 1))) col 200 48)
          320 (+ 240 (* indice 64)) img)
         (+ indice 1)))))

(define (dibujar-menu _estado)
  (define base (rectangle 640 ALTO 'solid FONDO))
  (define base1
    (place-image
     (above
      (text "FILL THE LINE" 36 (make-color 255 215 70))
      (square 8 'solid (make-color 0 0 0 0))
      (text "Traza una línea que llene todos los cuadros" 14 COLOR-TEXTO))
     320 110 base))
  (dibujar-botones-menu base1 0))

(define (dibujar-celdas-grises img lista)
  (if (null? lista) img (dibujar-celdas-grises (dibujar-celda img (caar lista) (cadar lista) CELDA-GRIS) (cdr lista))))

(define (dibujar-lineas-rastro img lista col)
  (if (or (null? lista) (null? (cdr lista)))
      img
      (let ([a (car lista)] [b (cadr lista)])
        (dibujar-lineas-rastro (dibujar-segmento img (coordenada-x (cadr a)) (coordenada-y (car a)) (coordenada-x (cadr b)) (coordenada-y (car b)) col) (cdr lista) col))))

(define (dibujar-celdas-visitadas img lista color-v)
  (if (null? lista) 
      img 
      (dibujar-celdas-visitadas (dibujar-celda img (caar lista) (cadar lista) color-v) (cdr lista) color-v)))

(define (dibujar-particulas img lista)
  (if (null? lista)
      img
      (let ([p (car lista)])
        (dibujar-particulas (place-image (square (particula-tamano p) 'solid (particula-color p)) (particula-x p) (particula-y p) img) (cdr lista)))))

(define (dibujar-juego estado)
  (define lv     (estado-juego-nivel estado))
  (define celdas (estado-juego-celdas estado))
  (define vis    (estado-juego-visitadas estado))
  (define col    (vector-ref COLORES lv))
  (define arrast (estado-juego-arrastrando estado))
  (define inicio (estado-juego-celda-inicio estado))

  (define base (rectangle 640 ALTO 'solid FONDO))
  (define base1 (place-image (text (string-append "← Volver | Nivel " (number->string (+ lv 1))) 20 COLOR-TEXTO) 320 30 base))

  (define n-vis (length vis))
  (define n-all (length celdas))
  (define ancho-barra (if (= n-all 0) 0 (inexact->exact (floor (* 540 (/ n-vis n-all))))))
  (define base2
    (place-image
     (beside (rectangle (max 1 ancho-barra) 5 'solid col) (rectangle (max 1 (- 540 ancho-barra)) 5 'solid CELDA-GRIS))
     320 50 base1))
  (define base3 (dibujar-celdas-grises base2 celdas))
  (define base3-inicio (if (null? inicio) base3 (dibujar-celda base3 (car inicio) (cadr inicio) "white")))
  (define base4 (dibujar-lineas-rastro base3-inicio vis col))
  (define base5
    (if (and arrast (pair? vis))
        (let ([lc (obtener-ultimo vis)])
          (dibujar-segmento base4 (coordenada-x (cadr lc)) (coordenada-y (car lc)) (estado-juego-mouse-x estado) (estado-juego-mouse-y estado)
                            (make-color (color-red col) (color-green col) (color-blue col) 160)))base4))
  (define color-v (make-color (color-red col) (color-green col) (color-blue col) 230))
  (define base6 (dibujar-celdas-visitadas base5 vis color-v))
  (define base7 (dibujar-particulas base6 (estado-juego-confeti estado)))base7)

(define INTERSECCION-Y  (/ ALTO 2))
(define BOTON-GANAR-Y (+ INTERSECCION-Y 38))
(define BOTON-PERDER-Y (+ INTERSECCION-Y 25))
(define BOTON-IZQ-X (- 320 82))
(define BOTON-DER-X (+ 320 82))
(define ANCHO-B 148)
(define ALTO-B  46)

(define (en-boton? x y boton-x boton-y)
  (and (>= x (- boton-x (/ ANCHO-B 2))) (< x (+ boton-x (/ ANCHO-B 2)))
       (>= y (- boton-y (/ ALTO-B 2))) (< y (+ boton-y (/ ALTO-B 2)))))

(define (dibujar-capa-intermedia estado)
  (define base  (dibujar-juego estado))
  (define fase  (estado-juego-fase estado))
  (define col   (vector-ref COLORES (estado-juego-nivel estado)))

  (define panel
    (cond
      [(string=? fase "ganado")
       (overlay
        (above
         (text "¡Nivel Completado!" 26 (make-color 255 215 70))
         (square 12 'solid (make-color 0 0 0 0))
         (text "¡Todos los cuadros llenos!" 14 COLOR-TEXTO)
         (square 18 'solid (make-color 0 0 0 0))
         (beside
          (crear-boton "Reintentar" (make-color 75 125 200) ANCHO-B ALTO-B)
          (square 16 'solid (make-color 0 0 0 0))
          (crear-boton "Siguiente ▶" col ANCHO-B ALTO-B)))
        (rectangle 360 175 'solid CAPA-OSCURA)
        (rectangle 364 179 'solid (make-color 75 85 110)))]
      [else
       (overlay
        (above
         (text "¡Inténtalo de nuevo!" 24 (make-color 220 100 80))
         (square 20 'solid (make-color 0 0 0 0))
         (crear-boton "Reintentar" (make-color 200 90 75) 160 ALTO-B))
        (rectangle 340 160 'solid CAPA-OSCURA)
        (rectangle 344 164 'solid (make-color 75 85 110)))]))

  (place-image panel 320 INTERSECCION-Y base))

(define (dibujar estado)
  (cond
    [(string=? (estado-juego-fase estado) "menu")    (dibujar-menu estado)]
    [(string=? (estado-juego-fase estado) "jugando") (dibujar-juego estado)]
    [else                                            (dibujar-capa-intermedia estado)]))

(define (en-boton-menu? x y i)
  (define boton-y (+ 240 (* i 64)))
  (and (>= x (- 320 100)) (< x (+ 320 100))
       (>= y (- boton-y 24)) (< y (+ boton-y 24))))

(define (buscar-click-menu x y indice)
  (cond
    [(>= indice 5) #f]
    [(en-boton-menu? x y indice) indice]
    [else (buscar-click-menu x y (+ indice 1))]))

(define (obtener-elemento-lista lista n)
  (if (= n 0)
      (car lista)
      (obtener-elemento-lista (cdr lista) (- n 1))))

(define (manejador-mouse estado x y evento)
  (define fase (estado-juego-fase estado))

  (cond
    [(string=? fase "menu")
     (if (string=? evento "button-down")
         (let ([lvl-click (buscar-click-menu x y 0)])
           (if lvl-click (nuevo-juego lvl-click) estado))
         estado)]

    [(and (string=? fase "ganado") (string=? evento "button-down"))
     (cond
       [(en-boton? x y BOTON-IZQ-X BOTON-GANAR-Y) (nuevo-juego (estado-juego-nivel estado))]
       [(en-boton? x y BOTON-DER-X BOTON-GANAR-Y) (nuevo-juego (modulo (+ (estado-juego-nivel estado) 1) 5))]
       [else estado])]

    [(and (string=? fase "perdido") (string=? evento "button-down"))
     (if (en-boton? x y 320 BOTON-PERDER-Y)
         (nuevo-juego (estado-juego-nivel estado))
         estado)]

    [(string=? fase "jugando")
     (cond
       [(and (string=? evento "button-down") (< x 120) (< y 50))
        (estado-menu)]

       [(string=? evento "button-down")
        (define golpe (buscar-colision x y (estado-juego-celdas estado)))
        (if (and golpe (equal? golpe (estado-juego-celda-inicio estado)))
            (make-estado-juego (estado-juego-nivel estado) (estado-juego-celdas estado) (estado-juego-visitadas estado) (estado-juego-celda-inicio estado) #t x y (estado-juego-fase estado) (estado-juego-confeti estado) (estado-juego-reloj estado))
            estado)]

       [(and (string=? evento "drag") (estado-juego-arrastrando estado))
        (define golpe     (buscar-colision x y (estado-juego-celdas estado)))
        (define visitadas (estado-juego-visitadas estado))
        (define s2 (make-estado-juego (estado-juego-nivel estado) (estado-juego-celdas estado) (estado-juego-visitadas estado) (estado-juego-celda-inicio estado) (estado-juego-arrastrando estado) x y (estado-juego-fase estado) (estado-juego-confeti estado) (estado-juego-reloj estado)))
        (cond
          [(and golpe (>= (length visitadas) 2)
                (equal? golpe (obtener-elemento-lista visitadas (- (length visitadas) 2))))
           (make-estado-juego (estado-juego-nivel s2) (estado-juego-celdas s2) (eliminar-ultimo visitadas) (estado-juego-celda-inicio s2) (estado-juego-arrastrando s2) x y (estado-juego-fase s2) (estado-juego-confeti s2) (estado-juego-reloj s2))]
          
          [(and golpe (siguiente-valido? golpe visitadas (estado-juego-celdas estado)))
           (make-estado-juego (estado-juego-nivel s2) (estado-juego-celdas s2) (append visitadas (list golpe)) (estado-juego-celda-inicio s2) (estado-juego-arrastrando s2) x y (estado-juego-fase s2) (estado-juego-confeti s2) (estado-juego-reloj s2))]
          [else s2])]

       [(and (string=? evento "button-up") (estado-juego-arrastrando estado))
        (define s2 (make-estado-juego (estado-juego-nivel estado) (estado-juego-celdas estado) (estado-juego-visitadas estado) (estado-juego-celda-inicio estado) #f (estado-juego-mouse-x estado) (estado-juego-mouse-y estado) (estado-juego-fase estado) (estado-juego-confeti estado) (estado-juego-reloj estado)))
        (cond
          [(= (length (estado-juego-visitadas s2)) (length (estado-juego-celdas s2)))
           (make-estado-juego (estado-juego-nivel s2) (estado-juego-celdas s2) (estado-juego-visitadas s2) (estado-juego-celda-inicio s2) (estado-juego-arrastrando s2) (estado-juego-mouse-x s2) (estado-juego-mouse-y s2) "ganado" (crear-confetti) (estado-juego-reloj s2))]
          [(> (length (estado-juego-visitadas s2)) 0)
           (make-estado-juego (estado-juego-nivel s2) (estado-juego-celdas s2) (estado-juego-visitadas s2) (estado-juego-celda-inicio s2) (estado-juego-arrastrando s2) (estado-juego-mouse-x s2) (estado-juego-mouse-y s2) "perdido" (estado-juego-confeti s2) (estado-juego-reloj s2))]
          [else s2])]
       [(string=? evento "move")
        (make-estado-juego (estado-juego-nivel estado) (estado-juego-celdas estado) (estado-juego-visitadas estado) (estado-juego-celda-inicio estado) (estado-juego-arrastrando estado) x y (estado-juego-fase estado) (estado-juego-confeti estado) (estado-juego-reloj estado))][else estado])][else estado]))

(define (manejador-reloj estado)
  (if (string=? (estado-juego-fase estado) "ganado")
      (make-estado-juego (estado-juego-nivel estado) (estado-juego-celdas estado) (estado-juego-visitadas estado) (estado-juego-celda-inicio estado) (estado-juego-arrastrando estado) (estado-juego-mouse-x estado) (estado-juego-mouse-y estado) (estado-juego-fase estado) (actualizar-confetti (estado-juego-confeti estado)) (+ (estado-juego-reloj estado) 1))estado))

(big-bang (estado-menu)
  [to-draw  dibujar]
  [on-mouse manejador-mouse]
  [on-tick  manejador-reloj 1/30]
  [name     "Fill the Line"])