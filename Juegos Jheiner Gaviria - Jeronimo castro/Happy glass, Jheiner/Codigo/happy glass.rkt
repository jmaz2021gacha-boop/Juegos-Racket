;Happy Glass


(require (lib "graphics.ss" "graphics"))
(open-graphics)

;abriendo ventana

(define uno (open-viewport "Happy Glass" 800 600))
;cuadricula
;((draw-pixmap uno) "cuadricula.bmp"(make-posn 0 0) "blue")



;--------------------------------------
; sacar la Y mayor de entre todos las Y de los puntos del dibujo

(define (mayor a b)
  (if (> a b)
      a
      b
      )
  )

(define (buscar-mayor-y lista y-max)
  (cond
    ;Si la lista ya esta vacia se tira la Y maxima
    ((null? lista)
     y-max)
    
    (else                    ;sacando el mayor entre la Y "actual mayor" y las Y del ini y fin de la linea 
     (buscar-mayor-y (cdr lista) (mayor y-max (mayor (posn-y (linea-segmento-inicio (car lista)))
                                                     (posn-y (linea-segmento-fin (car lista))))))
     )
    )
  )
;------------------------------------------
;sacando la y menor
(define (menor a b)
  (if (< a b)
      a
      b)
  )

(define (buscar-menor-y lista y-min)
  (cond
    ((null? lista)
     y-min)
    (else
     (buscar-menor-y (cdr lista) (menor y-min (menor (posn-y (linea-segmento-inicio (car lista)))
                                                     (posn-y (linea-segmento-fin (car lista))))))
     )
    ))

;-------------------------------------------
;sacando los limites en x del dibujo izq, dere

; Encuentra la X que está más a la izquierda

(define (buscar-menor-x lista x-min)
  (cond
    ((null? lista)
     x-min)
    (else
     (buscar-menor-x (cdr lista) (menor x-min (menor (posn-x (linea-segmento-inicio (car lista)))
                                                     (posn-x (linea-segmento-fin (car lista)))))
                     )
     )
    ))

; Encuentra la X que está más a la derecha

(define (buscar-mayor-x lista x-max)
  (cond
    ((null? lista) x-max)
    (else
     (buscar-mayor-x (cdr lista) (mayor x-max (mayor (posn-x (linea-segmento-inicio (car lista)))
                                                     (posn-x (linea-segmento-fin (car lista))))))
     )
    ))
;--------------------------------------

;dibujar interfaz
(define (dibujar-interfaz)
  ; Linea separadora de la barra de herramientas
  ((draw-line uno) (make-posn 0 60) (make-posn 800 60) "black")
  
  ; Boton REINICIAR (Posición: X de 20 a 140, Y de 15 a 45)
  ((draw-solid-rectangle uno) (make-posn 20 15) 120 30 "lightgray")
  ((draw-rectangle uno) (make-posn 20 15) 120 30 "black")
  ((draw-string uno)  (make-posn 45 35) "REINICIAR")
  
  ; Boton SALIR (Posición: X de 660 a 780, Y de 15 a 45)
  ((draw-solid-rectangle uno) (make-posn 660 15) 120 30 "salmon")
  ((draw-rectangle uno) (make-posn 660 15) 120 30 "black")
  ((draw-string uno) (make-posn 700 35) "SALIR" )
  )

;-------------------------------------------

; Función que dibuja todos las lineas de la lista movidos hacia abajo

(define (dibujar-animacion lista desp-y)
  (cond 
    ((not (null? lista))
     ((draw-line uno) (make-posn (posn-x (linea-segmento-inicio (car lista))) (+ (posn-y (linea-segmento-inicio (car lista))) desp-y))
                      (make-posn (posn-x (linea-segmento-fin (car lista))) (+ (posn-y (linea-segmento-fin (car lista))) desp-y))
                      "black")
   
;      (let* ((segmento (car lista))
;             ; Extraemos los puntos originales
;             (ini (linea-segmento-inicio segmento))
;             (fin (linea-segmento-fin segmento))
;             ; Creamos los nuevos puntos desplazados en Y
;             (nuevo-ini (make-posn (posn-x ini) (+ (posn-y ini) desp-y)))
;             (nuevo-fin (make-posn (posn-x fin) (+ (posn-y fin) desp-y)))
;             )
;        
;        ; Dibujamos el pedacito de línea desplazado
;        ((draw-line uno) nuevo-ini nuevo-fin "black")
;        


       ;recorta la lista para analizar y crear la siguiente linea
       (dibujar-animacion (cdr lista) desp-y)
       )
     )
    )

;-----------------------------------------------
;funcion auxiliar para analizar si el dibujo toco algun bloque del nivel

; Estructura base para definir plataformas en tus niveles
(define-struct bloque (x-inicio x-fin y-altura))

; Estructura para El vaso se define por su esquina izquierda X, su ancho, su Y base, su alto y si está lleno
(define-struct vaso (x-izq ancho y-base alto lleno?))

; Estructura para el tubo/grifo de donde cae el agua
(define-struct tubo (x y))

; Estructura para el escenario contiene los bloques, el tubo y el vaso de cada nivel en un solo objeto
(define-struct escenario (lista-bloques datos-vaso datos-tubo))


(define (toco-bloque? x-izq x-der y-actual lista-bloques)
  (cond
    ; si analizo todos los bloques y no colicionaron
    ((null? lista-bloques)
     #f)
            ;analiza si el dibujo toco el bloque
    ((and (>= y-actual (bloque-y-altura (car lista-bloques))) 
          (>= x-der (bloque-x-inicio (car lista-bloques)))
          (<= x-izq (bloque-x-fin (car lista-bloques))))
     
     #t)   
    ; mira los demas bloques de la lista
    (else
     (toco-bloque? x-izq x-der y-actual (cdr lista-bloques)))
    )
  )
;-----------------------------------------------
; mira si el vaso choca con el dibujo del usuario

(define (vaso-toca-linea? v-x-izq v-ancho v-y-actual lista-lineas desp-y-linea)
  (cond
    ((null? lista-lineas)
     #f) ; Si revisó todo el dibujo y no tocó nada
    (else
     (let* ((segmento (car lista-lineas))
            ; Puntos de la línea real en la pantalla (sumándoles lo que cayó la línea)
            (x1 (posn-x (linea-segmento-inicio segmento)))
            (y1 (+ (posn-y (linea-segmento-inicio segmento)) desp-y-linea))
            (x2 (posn-x (linea-segmento-fin segmento)))
            (y2 (+ (posn-y (linea-segmento-fin segmento)) desp-y-linea))
            
            ; Límites de X de ese pequeño segmento
            (seg-x-izq (menor x1 x2))
            (seg-x-der (mayor x1 x2))
            ; Altura promedio de este mini segmento
            (seg-y-promedio (/ (+ y1 y2) 2))
            
            ; Datos del vaso
            (v-x-der (+ v-x-izq v-ancho)))
       
       (if (and (>= v-y-actual (- seg-y-promedio 4)) ; el vaso llego a la altura de esa linea?
                (<= v-y-actual (+ seg-y-promedio 8)) ;para que no se pase de largo
                (>= v-x-der seg-x-izq)              ; El vaso covers horizontalmente el pedazo de linea?
                (<= v-x-izq seg-x-der))
           
           #t
           (vaso-toca-linea? v-x-izq v-ancho v-y-actual (cdr lista-lineas) desp-y-linea)
           )
       )
     )
    )
  )
;------------------------------------------------
;funciones auxiliares para las gotas

(define (dibujar-agua lista-gotas)
  (cond
    ((not (null? lista-gotas))
     ((draw-solid-rectangle uno) (car lista-gotas) 4 4 "blue") ; Gotas de 4x4 píxeles azules
     (dibujar-agua (cdr lista-gotas))
     )
    )
  )
;-------------------------------------
;mirar si la gota toca la linea dibujada

(define (gota-toca-lineas? gota lista-lineas desp-y-linea)
  (cond
    ((null? lista-lineas)
     #f)
    (else
     (let* ((segmento (car lista-lineas))
            (x1 (posn-x (linea-segmento-inicio segmento)))
            (y1 (+ (posn-y (linea-segmento-inicio segmento)) desp-y-linea))
            (x2 (posn-x (linea-segmento-fin segmento)))
            (y2 (+ (posn-y (linea-segmento-fin segmento)) desp-y-linea))
            (seg-x-izq (menor x1 x2))
            (seg-x-der (mayor x1 x2))
            (seg-y-promedio (/ (+ y1 y2) 2)))
       
       (if (and (>= (posn-y gota) (- seg-y-promedio 4))
                (<= (posn-y gota) (+ seg-y-promedio 4))
                (>= (posn-x gota) seg-x-izq)
                (<= (posn-x gota) seg-x-der))
           #t
           (gota-toca-lineas? gota (cdr lista-lineas) desp-y-linea)
           )
       )
     )
    )
  )
;---------------------------------------
;mover y y aplicar gravedad a las gotas y mira si hay alguna colisicon

; Función que encuentra qué lado del segmento está más bajo y devuelve la dirección (-4 o 4)
(define (obtener-direccion-pendiente segmento)
  (let* ((ini (linea-segmento-inicio segmento))
         (fin (linea-segmento-fin segmento)))
    (cond
      ; Si el punto final está más abajo en Y que el inicial, la gota va hacia el fin (derecha o izquierda)
      ((> (posn-y fin) (posn-y ini))
       (if (> (posn-x fin) (posn-x ini)) 4 -4))
      ; Si el punto inicial está más abajo en Y, la gota se mueve hacia el inicio
      ((< (posn-y fin) (posn-y ini))
       (if (> (posn-x ini) (posn-x fin)) 4 -4))
      ; Si es perfectamente horizontal, al azar
      (else (if (= (random 2) 0) -4 4)))
    )
  )

;----------------------------------------------
; Función que busca cuál línea exacta tocó la gota para calcular su dirección
(define (obtener-linea-tocada gota lista-lineas desp-y-linea)
  (cond
    ((null? lista-lineas)
     #f)
    (else
     (let* ((segmento (car lista-lineas))
            (x1 (posn-x (linea-segmento-inicio segmento)))
            (y1 (+ (posn-y (linea-segmento-inicio segmento)) desp-y-linea))
            (x2 (posn-x (linea-segmento-fin segmento)))
            (y2 (+ (posn-y (linea-segmento-fin segmento)) desp-y-linea))
            (seg-x-izq (menor x1 x2))
            (seg-x-der (mayor x1 x2))
            (seg-y-promedio (/ (+ y1 y2) 2))
            )
       (if (and (>= (posn-y gota) (- seg-y-promedio 4))
                (<= (posn-y gota) (+ seg-y-promedio 4))
                (>= (posn-x gota) seg-x-izq)
                (<= (posn-x gota) seg-x-der))
           segmento ; Devolvemos el segmento tocado
           (obtener-linea-tocada gota (cdr lista-lineas) desp-y-linea))
       )
     )
    )
  )
;-------------------------

; Mueve las gotas según bloques o inclinación de las líneas
(define (actualizar-posicion-gotas lista-gotas lista-lineas desp-y-linea datos-escenario v-actual)
  (cond
    ((null? lista-gotas)
     '())
    (else
     (let* ((gota (car lista-gotas))
            (gx (posn-x gota))
            (gy (posn-y gota))
            ; Colisión con bloques estáticos
            (toca-bloque? (toco-bloque? gx (+ gx 4) (+ gy 6) (escenario-lista-bloques datos-escenario)))
            ; Colisión con líneas (obtenemos el segmento si hay choque)
            (linea-tocada (obtener-linea-tocada (make-posn gx (+ gy 6)) lista-lineas desp-y-linea))
            )
       
       (cond
         ; Toca línea del usuario, Se desliza por la pendiente
         (linea-tocada
          (let ((dir-x (obtener-direccion-pendiente linea-tocada)))
            (cons (make-posn (+ gx dir-x) (+ gy 2)) 
                  (actualizar-posicion-gotas (cdr lista-gotas) lista-lineas desp-y-linea datos-escenario v-actual))))
         
         ; Toca bloque, Se escurre aleatoriamente
         (toca-bloque?
          (let ((despliegue-x (if (= (random 2) 0) -4 4)))
            (cons (make-posn (+ gx despliegue-x) (+ gy 2)) 
                  (actualizar-posicion-gotas (cdr lista-gotas) lista-lineas desp-y-linea datos-escenario v-actual))
            ))
         
         ; Fuera de pantalla
         ((> gy 600) 
          (actualizar-posicion-gotas (cdr lista-gotas) lista-lineas desp-y-linea datos-escenario v-actual))
         
         ; Caída libre normal
         (else
          (cons (make-posn gx (+ gy 6))
                (actualizar-posicion-gotas (cdr lista-gotas) lista-lineas desp-y-linea datos-escenario v-actual))
          )
         )
       )
     )
    )
  )
;---------------------------------------------
;fisicas del agua en si, controla el agua corriendo
(define (fisicas-agua lista-lineas desp-y-linea datos-escenario v-actual lista-gotas gotas-restantes gotas-adentro n) "cambio pasamos el nivel n para que al perder o ganar no de error"
  (cond
    ;SI EL VASO SE LLENO: Limpiamos pantalla de gotas y ganamos
    ((>= gotas-adentro 30)
     ((clear-viewport uno))
     (dibujar-bloques (escenario-lista-bloques datos-escenario))
     (dibujar-tubo (escenario-datos-tubo datos-escenario))
     (dibujar-animacion lista-lineas desp-y-linea)
     (dibujar-vaso v-actual)
     
     ; Pintamos el vaso lleno al 100%
     ((draw-solid-rectangle uno) 
      (make-posn (+ (vaso-x-izq v-actual) 2) (- (vaso-y-base v-actual) (vaso-alto v-actual)))
      (- (vaso-ancho v-actual) 4) (vaso-alto v-actual) "blue")
     (display "¡NIVEL COMPLETADO! El vaso está lleno")
     (esperar)) ; Esperamos clic para continuar

    ; Si no hay más gotas y el vaso no se llenó (perdió)
    ((and (null? lista-gotas) (<= gotas-restantes 0))
     (display "Nivel fallido, el agua no fue suficiente. Reiniciar.")
     
     (esperar))

    (else
     ((clear-viewport uno))
     (dibujar-bloques (escenario-lista-bloques datos-escenario))
     (dibujar-tubo (escenario-datos-tubo datos-escenario))
     (dibujar-animacion lista-lineas desp-y-linea)
     (dibujar-vaso v-actual)
     
     ; Dibujo del agua acumulada dentro del vaso
     (cond
       ((> gotas-adentro 0)
        ((draw-solid-rectangle uno) 
         (make-posn (+ (vaso-x-izq v-actual) 2) (- (vaso-y-base v-actual) (menor (vaso-alto v-actual) (* gotas-adentro 1.8))))
         (- (vaso-ancho v-actual) 4) 
         (menor (vaso-alto v-actual) (* gotas-adentro 1.8)) 
         "blue")
        )
       )

     ; Generar una nueva gota desde el tubo si aún quedan disponibles
     (let* ((tubito (escenario-datos-tubo datos-escenario))
            (nuevas-gotas (if (> gotas-restantes 0)
                              (cons (make-posn (tubo-x tubito) (+ (tubo-y tubito) 20)) lista-gotas)
                              lista-gotas))
            (nuevas-gotas-restantes (if (> gotas-restantes 0) (- gotas-restantes 1) 0))
            
            ; Actualizar posiciones con la física de pendientes
            (gotas-movidas (actualizar-posicion-gotas nuevas-gotas lista-lineas desp-y-linea datos-escenario v-actual))
            
            ; Analizar cuáles gotas entraron al vaso
            (v-izq (vaso-x-izq v-actual))
            (v-der (+ v-izq (vaso-ancho v-actual)))
            (v-base (vaso-y-base v-actual))
            (v-tope (- v-base (vaso-alto v-actual)))

            (gotas-captured (length (filter (lambda (g) 
                                                 (and (>= (posn-x g) v-izq) 
                                                      (<= (posn-x g) v-der)
                                                      (>= (posn-y g) v-tope)
                                                      (<= (posn-y g) v-base))) 
                                             gotas-movidas)))
            
            (gotas-filtradas (filter (lambda (g)
                                       (not (and (>= (posn-x g) v-izq) 
                                                 (<= (posn-x g) v-der)
                                                 (>= (posn-y g) v-tope)
                                                 (<= (posn-y g) v-base)))) 
                                     gotas-movidas)))

       (dibujar-agua gotas-filtradas)
       (sleep 0.01)
       
       (fisicas-agua lista-lineas 
                     desp-y-linea 
                     datos-escenario 
                     v-actual 
                     gotas-filtradas 
                     nuevas-gotas-restantes 
                     (+ gotas-adentro gotas-captured)
                     n); "cambio añadimos n al final para mantener el nivel en el ciclo"
       )
     )
    )
  )
;----------------------------------------------

(define (dibujar-tubo datos-tubo)
  (let ((x (tubo-x datos-tubo))
        (y (tubo-y datos-tubo)))
    ; se dibuja el "tubo"
    ((draw-solid-rectangle uno) (make-posn (- x 15) y) 30 20 "gray")
    ((draw-rectangle uno) (make-posn (- x 15) y) 30 20 "black"))
  )

;------------------------------------------------

; Función que recorre la lista de bloques y dibuja cada uno en pantalla
(define (dibujar-bloques lista-bloques)
  (cond
    ((not (null? lista-bloques))
     
     ; Punto inicial: (x-inicio, y-altura)
     ; Ancho: (x-fin menos x-inicio)
     ; Alto: 50 píxeles por defecto
     ((draw-solid-rectangle uno) (make-posn (bloque-x-inicio (car lista-bloques)) (bloque-y-altura (car lista-bloques)))
                                 (- (bloque-x-fin (car lista-bloques)) (bloque-x-inicio (car lista-bloques))) 50 "brown")

     (dibujar-bloques (cdr lista-bloques))
     )
    )
  )
;-------------------------------------------

(define (dibujar-vaso datos-vaso)
  ((draw-rectangle uno) 
       (make-posn (vaso-x-izq datos-vaso) (- (vaso-y-base datos-vaso) (vaso-alto datos-vaso)))
                (vaso-ancho datos-vaso)
                          (vaso-alto datos-vaso)
                                               "black"))

;--------------------------------------------------

; Función que crea la caida

; y-maxima: el punto Y más bajo del dibujo (para saber cuándo choca)

(define (fisicas-lineas lista desp-y y-maxima x-izq x-der y-min datos-escenario n); "cambio añadimos n para saber que nivel procesar"
  (cond
    ; Extraemos la lista de bloques del escenario para mirar la colicion
   ((toco-bloque? x-izq x-der (+ y-maxima desp-y) (escenario-lista-bloques datos-escenario))
    
     ; llamamos a la funcion del fisicas vaso para iniciar su recurción
     (fisicas-vaso lista desp-y y-maxima x-izq x-der datos-escenario 0 n)); "cambio pasamos n al vaso"
   
    ((> (+ y-min desp-y) 620)
     (display "dibujo fuera de la pantalla, reiniciar nivel")
     (esperar))
    
    (else
     ((clear-viewport uno))
     ;Dibujamos los bloques del escenario actual
     (dibujar-bloques (escenario-lista-bloques datos-escenario))

     ;tuboo
     (dibujar-tubo (escenario-datos-tubo datos-escenario)); "cambio usamos datos-escenario en vez de escenario-inicial para que cambie en nivel 2"
     
     ;Dibujamos el vaso del escenario actual en su posición quieta (por ahora)
     (dibujar-vaso (escenario-datos-vaso datos-escenario))

     ; 3. Dibujamos la línea cayendo
     (dibujar-animacion lista desp-y)
     (sleep 0.05)
     
     ; Pasamos el datos-escenario sin tocar a la recursion
     (fisicas-lineas lista (+ desp-y 10) y-maxima x-izq x-der y-min datos-escenario n): "cambio pasamos n al ciclo"
     )
    )
  )

;-----------------------------------------
; funcion para sacar el movimiento del vaso
(define (fisicas-vaso lista-lineas desp-y-final-linea y-max-linea x-izq x-der datos-escenario desp-y-vaso n); "cambio añadimos n al final"
  
  (let* ((v-original (escenario-datos-vaso datos-escenario))
         ; Posición Y actual de la base del vaso
         (v-y-actual (+ (vaso-y-base v-original) desp-y-vaso))
         (v-actualizado (make-vaso (vaso-x-izq v-original) 
                                   (vaso-ancho v-original) 
                                   v-y-actual 
                                   (vaso-alto v-original) 
                                   (vaso-lleno? v-original))))
    
    (cond
      ; El vaso choca con algún bloque?
      ((toco-bloque? (vaso-x-izq v-actualizado) 
                     (+ (vaso-x-izq v-actualizado) (vaso-ancho v-actualizado)) 
                     v-y-actual 
                     (escenario-lista-bloques datos-escenario))
       
       (let* ((bloque-tocado (car (escenario-lista-bloques datos-escenario)))
              (y-exacta-bloque (bloque-y-altura bloque-tocado))
              (v-perfecto (make-vaso (vaso-x-izq v-original) (vaso-ancho v-original) y-exacta-bloque (vaso-alto v-original) #f)))
         (display "¡El vaso toco piso, Soltando agua")
         
         ; Llama a las físicas del agua pasándole: 120 gotas totales en el tanque, 0 gotas adentro al iniciar
         (fisicas-agua lista-lineas desp-y-final-linea datos-escenario v-perfecto '() 120 0 n))); "cambio pasamos n al agua"

      ;  EL vaso cae en el dibujo del usuario?
      ((vaso-toca-linea? (vaso-x-izq v-actualizado) 
                         (vaso-ancho v-actualizado) 
                         v-y-actual 
                         lista-lineas          
                         desp-y-final-linea)
       
       (let* ((v-perfecto (make-vaso (vaso-x-izq v-original) (vaso-ancho v-original) v-y-actual (vaso-alto v-original) #f)))
         (display "¡El vaso se sostuvo en el dibujo, Soltando agua")
         ; Llama a las físicas del agua
         (fisicas-agua lista-lineas desp-y-final-linea datos-escenario v-perfecto '() 120 0 n))) ;"cambio pasamos n al agua"

      ; Si el vaso se cae al vacío
      ((> v-y-actual 620)
       (display "El vaso cayó al vacío, reiniciar nivel")
       (esperar))

      ; Sigue cayendo en el aire
      (else
       ((clear-viewport uno))
       (dibujar-tubo (escenario-datos-tubo datos-escenario)); "cambio usamos datos-escenario"
       (dibujar-bloques (escenario-lista-bloques datos-escenario))
       (dibujar-animacion lista-lineas desp-y-final-linea)
       (dibujar-vaso v-actualizado)
        
       (sleep 0.02)
       (fisicas-vaso lista-lineas desp-y-final-linea y-max-linea x-izq x-der datos-escenario (+ desp-y-vaso 8) n) ;"cambio pasamos n"
       )
      )
    )
  )
;-------------------------------------------------------------------------------------------------------
;Funcion Para Guardar los niveles (aca creo la estructura de los bloques de cada nivel)
(define (cargar-nivel n)
  (cond
    ; Nivel 1: Bloques, vaso en Y=200, y Tubo en X=380, Y=50 
    ((= n 1)
     (make-escenario 
      (list (make-bloque 300 380 400) (make-bloque 470 500 400)) ; Bloques
      (make-vaso 400 60 225 50 #f)                               ; Vaso
      (make-tubo 420 150)))                                       ; Tubo
    
    ; Nivel 2: Bloques, vaso en Y=150, y Tubo en X=580, Y=40
    ((= n 2)
     (make-escenario 
      (list (make-bloque 100 300 250) (make-bloque 500 700 350)) ; Bloques
      (make-vaso 550 60 150 50 #f)                               ; Vaso
      (make-tubo 580 40)))                                       ; tubo
    
    (else
     (make-escenario '() (make-vaso 0 0 0 0 #f) (make-tubo 0 0)))
    )
  )
  

;----------------------------------------
      
;lapiz

; Estructura que guarda cada linea del dibujo del usuario
(define-struct linea-segmento (inicio fin))

;se necesita el let porque si no se generan huecos en el dibujo del usuario debido a que
;la funcion query-mouse-posn se toma un tiempo para analizar la posicion del mouse,
;no es inmediato lo que genera espacios entre cada linea del dibujo, el let guarda temporalmente
;el dato de la posicion en la que termino la linea solucionando el pobrema de los huecos

(define (lapiz posn-anterior lista-lineas datos-escenario n) ; <-- Añadido n
  (cond
    ((not (ready-mouse-release uno))
     (let ((posn-actual (query-mouse-posn uno)))
       (cond 
         ((and (<= (posn-x posn-actual) 800) (>= (posn-x posn-actual) 0)
               (<= (posn-y posn-actual) 600) (>= (posn-y posn-actual) 0))
          
          ((draw-line uno) posn-anterior posn-actual "black")
          
          ; Pasamos n en la recursión
          (lapiz posn-actual (cons (make-linea-segmento posn-anterior posn-actual) lista-lineas) datos-escenario n))
         
         (else
          (lapiz posn-anterior lista-lineas datos-escenario n)))))
    (else
     ; Al soltar el clic, pasamos el nivel n a las físicas de las líneas
     (fisicas-lineas-nivel lista-lineas 0
                           (buscar-mayor-y lista-lineas (posn-y (linea-segmento-inicio (car lista-lineas))))
                           (buscar-menor-x lista-lineas (posn-x (linea-segmento-inicio (car lista-lineas))))
                           (buscar-mayor-x lista-lineas (posn-x (linea-segmento-inicio (car lista-lineas))))
                           (buscar-menor-y lista-lineas (posn-y (linea-segmento-inicio (car lista-lineas))))
                           datos-escenario n)
     )
    )
  )

;--------------------------------------------
;reiniciar juego

(define (reiniciar-juego n)
  (let ((escenario-limpio (cargar-nivel n)))
    ((clear-viewport uno))
    (dibujar-bloques (escenario-lista-bloques escenario-limpio))
    (dibujar-vaso (escenario-datos-vaso escenario-limpio))
    (dibujar-tubo (escenario-datos-tubo escenario-limpio))
    (dibujar-interfaz)
    ; Volvemos a esperar el clic para iniciar el dibujo en un escenari
    (esperar-clic-nivel n))
  )
;-------------------------------------------
;validar el clic
(define (validar-clic c n)
  (let* ((pos (mouse-click-posn c))
         (cx (posn-x pos))
         (cy (posn-y pos)))
    (cond
      ; toco reiniciar? (X de 20 a 140, Y de 15 a 45)
      ((and (>= cx 20) (<= cx 140) (>= cy 15) (<= cy 45))
       (displayln "Reiniciando nivel")
       (reiniciar-juego n))
      
      ; toco salir? (X de 660 a 780, Y de 15 a 45)
      ((and (>= cx 660) (<= cx 780) (>= cy 15) (<= cy 45))
       (displayln "Cerrando el juego")
       (close-viewport uno))
      
      ; 3. en la barra de herramientas pero en ningun boton?
      ((< cy 60)
       (esperar-clic-nivel n))
      
      ; 4. en la pantalla del juego? dibujar
      (else
       (lapiz (query-mouse-posn uno) '() (cargar-nivel n) n))
      )
    )
  )
;_-------------------------------

; esperar (tambien resive el nievel actual
(define (esperar-clic-nivel n)
  (let ((c (get-mouse-click uno)))
    (validar-clic c n)))
;--------------------------------------

(define (fisicas-lineas-nivel lista desp-y y-max x-izq x-der y-min datos n)
  (fisicas-lineas lista desp-y y-max x-izq x-der y-min datos n)); "cambio agregamos n al final para enviarlo a fisicas-lineas"

(define (esperar)
  (get-mouse-click uno))
;--------------------------------------

; inicio del juego (Nivel 1)
(define escenario-inicial (cargar-nivel 1))

((clear-viewport uno))

(dibujar-bloques (escenario-lista-bloques escenario-inicial))
(dibujar-vaso (escenario-datos-vaso escenario-inicial))
(dibujar-tubo (escenario-datos-tubo escenario-inicial))
(dibujar-interfaz)



(esperar-clic-nivel 1)