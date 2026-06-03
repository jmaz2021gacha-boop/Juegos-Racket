(require (lib "graphics.ss" "graphics"))

(open-graphics)

(define juego (open-viewport "Ludo" 1200 700))
;-------------------------------------------------------------------------------
; VECTOR DE ESTADO: 
; Índice 0 = Amarillo (Jugador 1) -> Pasos de 0 a 56
; Índice 1 = Azul     (Jugador 2) -> Pasos de 0 a 56
; Índice 2 = Rojo     (Jugador 3) -> Pasos de 0 a 56
; Índice 3 = Verde    (Jugador 4) -> Pasos de 0 a 56
(define Actual (make-vector 4 0))
;-------------------------------------------------------------------------------
;MAPA
((draw-pixmap juego) "mesa.bmp" (make-posn 300 50))
((draw-solid-ellipse juego) (make-posn 300 50) 200 200 "Gold")
((draw-solid-ellipse juego) (make-posn 370 120) 60 60 "Goldenrod")
((draw-solid-ellipse juego) (make-posn 700 50) 200 200 "CornflowerBlue")
((draw-solid-ellipse juego) (make-posn 770 120) 60 60 "Navy")
((draw-solid-ellipse juego) (make-posn 700 450) 200 200 "Orange Red")
((draw-solid-ellipse juego) (make-posn 770 520) 60 60 "Firebrick")
((draw-solid-ellipse juego) (make-posn 300 450) 200 200 "Yellow Green")
((draw-solid-ellipse juego) (make-posn 370 520) 60 60 "Dark Olive Green")
;-------------------------------------------------------------------------------
;FICHA
(define (ficha color x y)
  ((draw-solid-ellipse juego) (make-posn (- x 4) (- y 10)) 15 15 color) ;cabeza
  ((draw-solid-rectangle juego) (make-posn x y) 8 22 color) ;tronco
  ((draw-solid-rectangle juego) (make-posn (- x 6) (+ y 20)) 19 7 color) ;base pqñ
  ((draw-solid-rectangle juego) (make-posn (- x 9) (+ y 22)) 25 8 color) ;base grande
  )

;-------------------------------------------------------------------------------
; LÓGICA DE PASOS EN CASILLAS REALES (Actualizado con el nuevo orden de jugadores)
(define (obtener-casilla-real jug pasos)
  (cond
    ((or (= pasos 0) (> pasos 50)) 0) ; Base o zona ganadora
    ((= jug 1) (if (<= pasos 27) (+ 25 pasos) (- pasos 27))) ; Amarillo (Sale en 26)
    ((= jug 2) (if (<= pasos 39) (+ 13 pasos) (- pasos 39))) ; Azul     (Sale en 14)
    ((= jug 3) pasos)                                        ; Rojo     (Sale en 1)
    ((= jug 4) (if (<= pasos 14) (+ 38 pasos) (- pasos 14))) ; Verde    (Sale en 39)
    (else 0)))

; MECÁNICA DE COLISIONES (Sin usar LET, mediante recursión pura interna)
(define (verificar-captura jug-actual)
  (define pasos-actual (vector-ref Actual (- jug-actual 1)))
  (define cas-actual (obtener-casilla-real jug-actual pasos-actual))
  (define seguros '(1 11 14 23 26 36 39 50))
  
  (define (recorrer-enemigos i)
    (cond
      ((<= i 4)
       [begin
         (if (and (not (= i jug-actual)) 
                  (= cas-actual (obtener-casilla-real i (vector-ref Actual (- i 1)))))
             (if (member cas-actual seguros)
                 (display "¡Casilla segura! No puedes comer aquí.\n")
                 [begin
                   (vector-set! Actual (- i 1) 0) ; Regresa a la base mandando sus pasos a 0
                   (display "¡PUM! Ficha comida enviada al inicio.\n")])
             'nothing)
         (recorrer-enemigos (+ i 1))]))
    )

  (if (not (= cas-actual 0))
      (recorrer-enemigos 1)
      'nothing))

;-------------------------------------------------------------------------------
;DADO Y VASO
(define (dado n jugador) ;dado según el número sacado
  ((draw-solid-rectangle juego) (make-posn 1000 600) 50 50 "wheat")
  
  ; Control exacto de pasos hacia la meta (56)
  (cond
    ((> (+ (vector-ref Actual (- jugador 1)) n) 56)
     (display "¡Te pasaste! Tiro anulado.\n"))
    (else
     [begin
       (vector-set! Actual (- jugador 1) (+ (vector-ref Actual (- jugador 1)) n))
       (verificar-captura jugador) ; Evalúa capturas inmediatamente después de mover
       ]))
  
  (cond
    ((= n 1) [begin ((draw-solid-ellipse juego) (make-posn 1020 620) 10 10 "black")])
    ((= n 2) [begin ((draw-solid-ellipse juego) (make-posn 1005 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 635) 10 10 "black")])
    ((= n 3) [begin ((draw-solid-ellipse juego) (make-posn 1005 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1020 620) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 635) 10 10 "black")])
    ((= n 4) [begin ((draw-solid-ellipse juego) (make-posn 1005 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1005 635) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 635) 10 10 "black")])
    ((= n 5) [begin ((draw-solid-ellipse juego) (make-posn 1005 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1020 620) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1005 635) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 635) 10 10 "black")])
    ((= n 6) [begin ((draw-solid-ellipse juego) (make-posn 1005 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 605) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1005 620) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 620) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1005 635) 10 10 "black")
                    ((draw-solid-ellipse juego) (make-posn 1035 635) 10 10 "black")])))

;---------
(define (menos_vaso x y) ;elimina el rastro del vaso
  ((draw-solid-rectangle juego) (make-posn x y) 450 200 "white"))
;---------
(define (vaso jug cont x y)
  (cond
    ((< cont 1) [begin
                  ((draw-pixmap juego) "vaso.bmp" (make-posn 950 400))
                  (sleep 1)
                  (menos_vaso x y)
                  (vaso jug (+ cont 1) x (+ y 200))])
    ((< cont 2) [begin
                  ((draw-pixmap juego) "vaso.bmp" (make-posn 950 530))
                  (sleep 1)
                  (menos_vaso 950 530)
                  (vaso jug (+ cont 1) x y)])
    ((< cont 11) [begin
                   ((draw-pixmap juego) "vaso.bmp" (make-posn 950 530))
                   (sleep 0.09)
                   ((draw-pixmap juego) "vaso.bmp" (make-posn 900 530))
                   (sleep 0.09)
                   (menos_vaso 900 530)
                   (vaso jug (+ cont 1) x y)])
    (else [begin
            (menos_vaso 900 530)
            ((draw-pixmap juego) "vaso.bmp" (make-posn 950 400))
            (dado (+ 1 (random 6)) jug) 
            (sleep 1)
            
            ; Cambio secuencial de turnos 
            (cond
              ((= jug 1) (principal 2))
              ((= jug 2) (principal 3))
              ((= jug 3) (principal 4))
              ((= jug 4) (principal 1)))])
    ))

;-------------------------------------------------------------------------------
;TURNOS DE CADA JUGADOR (BUCLE PRINCIPAL)
(define (principal jug)
  (actualizar-pantalla) ; Redibuja el tablero limpio y las posiciones de todas las fichas
  
  ((draw-solid-rectangle juego) (make-posn 950 30) 250 350 "white")
  ((draw-string juego) (make-posn 950 70) "6 para salir")

  (cond 
    ; Condiciones de victoria globales con el nuevo orden de índices
    ((= (vector-ref Actual 0) 56) ((draw-string juego) (make-posn 950 200) "¡GANÓ EL AMARILLO! 🎉🟡" "Goldenrod"))
    ((= (vector-ref Actual 1) 56) ((draw-string juego) (make-posn 950 200) "¡GANÓ EL AZUL! 🎉🔵" "Navy"))
    ((= (vector-ref Actual 2) 56) ((draw-string juego) (make-posn 950 200) "¡GANÓ EL ROJO! 🎉🔴" "red"))
    ((= (vector-ref Actual 3) 56) ((draw-string juego) (make-posn 950 200) "¡GANÓ EL VERDE! 🎉🟢" "Dark Olive Green"))
    
    ; Mensajes de turno con el orden modificado
    ((or (= jug 1) (= jug 2) (= jug 3) (= jug 4))
     [begin
       (cond
         ((= jug 1) ((draw-string juego) (make-posn 950 150) "Turno: Ficha Amarilla 🟡" "Goldenrod"))
         ((= jug 2) ((draw-string juego) (make-posn 950 150) "Turno: Ficha Azul 🔵" "Navy"))
         ((= jug 3) ((draw-string juego) (make-posn 950 150) "Turno: Ficha Roja 🔴" "Firebrick"))
         ((= jug 4) ((draw-string juego) (make-posn 950 150) "Turno: Ficha Verde 🟢" "Dark Olive Green")))
       
       ((draw-string juego) (make-posn 950 300) "Tu turno we, puchale al enter⬇️" "blue")

       (define tecla (key-value (get-key-press juego)))
       (cond
         ((equal? tecla #\return) [begin (vaso jug 0 950 400)])
         (else (principal jug)))])
    (else (principal 1))))

;----------------------------------
;MOVIMIENTO DEL JUGADOR (Mapea el renderizado usando el nuevo orden de índices)
(define (mover-ficha-amarilla pasos)
  (cond
    ((= pasos 0) (ficha "yellow" 396 138))
    ((<= pasos 27) (casillas (+ 26 pasos) "yellow"))
    ((<= pasos 50) (casillas (- pasos 27) "yellow"))
    ((<= pasos 56) (casillasW (- pasos 38) "yellow"))))

(define (mover-ficha-azul pasos)
  (cond
    ((= pasos 0) (ficha "blue" 796 138))
    ((<= pasos 39) (casillas (+ 13 pasos) "blue"))
    ((<= pasos 50) (casillas (- pasos 39) "blue"))
    ((<= pasos 56) (casillasW (- pasos 44) "blue"))))

(define (mover-ficha-roja pasos)
  (cond
    ((= pasos 0) (ficha "red" 796 540))
    ((<= pasos 50) (casillas pasos "red"))
    ((<= pasos 56) (casillasW (- pasos 32) "red"))))

(define (mover-ficha-verde pasos)
  (cond
    ((= pasos 0) (ficha "green" 396 540))
    ((<= pasos 14) (casillas (+ 39 pasos) "green"))
    ((<= pasos 50) (casillas (- pasos 14) "green"))
    ((<= pasos 56) (casillasW (- pasos 50) "green"))))

; INTERFAZ DE REDIBUJO AUTOMÁTICO
(define (actualizar-pantalla)
  ((draw-pixmap juego) "mesa.bmp" (make-posn 300 50))
  ((draw-solid-ellipse juego) (make-posn 300 50) 200 200 "Gold")
  ((draw-solid-ellipse juego) (make-posn 370 120) 60 60 "Goldenrod")
  ((draw-solid-ellipse juego) (make-posn 700 50) 200 200 "CornflowerBlue")
  ((draw-solid-ellipse juego) (make-posn 770 120) 60 60 "Navy")
  ((draw-solid-ellipse juego) (make-posn 700 450) 200 200 "Orange Red")
  ((draw-solid-ellipse juego) (make-posn 770 520) 60 60 "Firebrick")
  ((draw-solid-ellipse juego) (make-posn 300 450) 200 200 "Yellow Green")
  ((draw-solid-ellipse juego) (make-posn 370 520) 60 60 "Dark Olive Green")
  
  (mover-ficha-amarilla (vector-ref Actual 0)) ; Índice 0
  (mover-ficha-azul     (vector-ref Actual 1)) ; Índice 1
  (mover-ficha-roja     (vector-ref Actual 2)) ; Índice 2
  (mover-ficha-verde    (vector-ref Actual 3))) ; Índice 3

;-------------------------------------------------------------------------------
; DEFINICIÓN DE MATRIZ DE CASILLAS (Sin duplicados y corregida)
(define (casillas n color)
  (if (= n 1)  (ficha color 637 542))
  (if (= n 2)  (ficha color 637 502))
  (if (= n 3)  (ficha color 637 462))
  (if (= n 4)  (ficha color 637 422))
  (if (= n 5)  (ficha color 677 382)) 
  (if (= n 6)  (ficha color 717 382)) 
  (if (= n 7)  (ficha color 757 382)) 
  (if (= n 8)  (ficha color 797 382)) 
  (if (= n 9)  (ficha color 837 382)) 
  (if (= n 10) (ficha color 877 382)) 
  (if (= n 11) (ficha color 877 342)) 
  (if (= n 12) (ficha color 877 302)) 
  (if (= n 13) (ficha color 837 302)) 
  (if (= n 14) (ficha color 797 302)) 
  (if (= n 15) (ficha color 757 302))
  (if (= n 16) (ficha color 717 302))
  (if (= n 17) (ficha color 677 302))
  (if (= n 18) (ficha color 637 262))
  (if (= n 19) (ficha color 637 220))
  (if (= n 20) (ficha color 637 180))
  (if (= n 21) (ficha color 637 140))
  (if (= n 22) (ficha color 637 98))
  (if (= n 23) (ficha color 637 58))
  (if (= n 24) (ficha color 597 58))
  (if (= n 25) (ficha color 557 58))
  (if (= n 26) (ficha color 557 98))
  (if (= n 27) (ficha color 557 140))
  (if (= n 28) (ficha color 557 180))
  (if (= n 29) (ficha color 557 220))
  (if (= n 30) (ficha color 557 260))
  (if (= n 31) (ficha color 517 300))
  (if (= n 32) (ficha color 477 300))
  (if (= n 33) (ficha color 437 300))
  (if (= n 34) (ficha color 397 300))
  (if (= n 35) (ficha color 357 300))
  (if (= n 36) (ficha color 317 300))
  (if (= n 37) (ficha color 317 340))
  (if (= n 38) (ficha color 317 380))
  (if (= n 39) (ficha color 357 380))
  (if (= n 40) (ficha color 397 380))
  (if (= n 41) (ficha color 437 380))
  (if (= n 42) (ficha color 477 380))
  (if (= n 43) (ficha color 517 380)) 
  (if (= n 44) (ficha color 557 420))
  (if (= n 45) (ficha color 557 460))
  (if (= n 46) (ficha color 557 500))
  (if (= n 47) (ficha color 557 540))
  (if (= n 48) (ficha color 557 580))
  (if (= n 49) (ficha color 557 620))
  (if (= n 50) (ficha color 597 620))
  (if (= n 51) (ficha color 637 620))
  (if (= n 52) (ficha color 637 580)))

(define (casillasW n color)
  ; Casillas ganadoras del Verde (1 al 6)
  (if (= n 1)  (ficha color 357 340))
  (if (= n 2)  (ficha color 397 340))
  (if (= n 3)  (ficha color 437 340))
  (if (= n 4)  (ficha color 477 340))
  (if (= n 5)  (ficha color 517 340))
  (if (= n 6)  (ficha color 557 340))
  
  ; Casillas ganadoras del Azul (7 al 12)
  (if (= n 7)  (ficha color 837 340))
  (if (= n 8)  (ficha color 797 340))
  (if (= n 9)  (ficha color 757 340))
  (if (= n 10) (ficha color 717 340))
  (if (= n 11) (ficha color 677 340))
  (if (= n 12) (ficha color 637 340))
  
  ; Casillas ganadoras del Amarillo (13 al 18)
  (if (= n 13) (ficha color 597 98))
  (if (= n 14) (ficha color 597 138))
  (if (= n 15) (ficha color 597 178))
  (if (= n 16) (ficha color 597 218))
  (if (= n 17) (ficha color 597 258))
  (if (= n 18) (ficha color 597 298))
  
  ; Casillas ganadoras del Rojo (19 al 24)
  (if (= n 19) (ficha color 597 582))
  (if (= n 20) (ficha color 597 542))
  (if (= n 21) (ficha color 597 502))
  (if (= n 22) (ficha color 597 462))
  (if (= n 23) (ficha color 597 422))
  (if (= n 24) (ficha color 597 382)))

;--------------------------------
;POSICIONES INICIALES
(actualizar-pantalla)

;---------------------------------

(principal 1) ; Arranca el Jugador 1 (Amarillo)