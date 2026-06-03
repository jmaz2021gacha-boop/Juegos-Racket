
(require 2htdp/image)
(require 2htdp/universe)
(require racket/system)

;; ============================================================
;;  CONSTANTES
;; ============================================================
(define ANCHO 1000)
(define ALTO 700)
(define TAMANO-CELDA 70)
(define VELOCIDAD 10)
(define RADIO 25)
(define PUERTA1-F 4)
(define PUERTA1-C 3)
(define PUERTA2-F 4)
(define PUERTA2-C 9)

;; ============================================================
;;  IMAGENES
;; ============================================================
(define IMAGEN-INICIO
  (bitmap/file "INICIO.png"))

(define IMAGEN-INICIO2
  (bitmap/file "INICIO2.png"))

(define TEXTURA-SUELO
  (bitmap/file "B.GRIS-CLARO.png"))

(define TEXTURA-PARED
  (bitmap/file "B.GRIS-OSCU.png"))

(define TILE-ROJO
  (bitmap/file "COLOR-ROJO.png"))

(define TILE-ESTRELLA
  (bitmap/file "FIG-ESTRELLA.png"))

(define TILE-TRIANGULO
  (bitmap/file "FIG-TRIANGULO.png"))

(define PUERTA-CERRADA-TR
  (bitmap/file "CUADRO-TR.png"))

(define PUERTA-CERRADA-CV
  (bitmap/file "CUADRO-CV.png"))

(define PUERTA-ABIERTA
  (bitmap/file "CUADRO-PA.png"))

(define IMAGEN-OPCIONES
  (bitmap/file "PANEL.png"))

(define IMAGEN-OPCIONES2
  (bitmap/file "PANEL2.png"))

(define IMAGEN-TEXTO-F
  (bitmap/file "TEXTO-F.png"))

(define TILE-CUADRADO
  (bitmap/file "FIG-CUADRADO.png"))

(define TILE-CIRCULO
  (bitmap/file "FIG-CIRCULO.png"))

(define TILE-VERDE
  (bitmap/file "COLOR-VERDE.png"))

(define TILE-AZUL
  (bitmap/file "COLOR-AZUL.png"))

(define PUERTA-COLOR-ROJO
  (bitmap/file "CUADRO-CR.png"))

(define PUERTA-CIRCULO-V
  (bitmap/file "CUADRO-CIR V.png"))

(define PUERTA-CIRCULO-H
  (bitmap/file "CUADRO-CIR H.png"))

(define PUERTA-COLOR-VERDE
  (bitmap/file "CUADRO-COLORV.png"))

(define PUERTA-CUADRADO
  (bitmap/file "CUADRO-C.png"))

(define PUERTA-COLOR-AZUL
  (bitmap/file "CUADRO-CA.png"))

(define PUERTA-ABIERTA-H
  (bitmap/file "CUADRO-PA H.png"))


;; ============================================================
;;  MAPA NIVEL 1
;; ============================================================

;0 = suelo libre
;1 = pared
;2 = tile rojo
;3 = tile triángulo
;4 = estrella
;5 = puerta circulo verde
;6 = puerta triangulo rojo

(define NIVEL1
  '((1 1 1 1 1 1 1 1 1 1 1)
    (1 0 1 0 0 0 0 0 0 0 1)
    (1 0 1 1 0 1 2 1 0 0 1)
    (1 0 0 1 0 1 1 1 0 1 1)
    (1 0 0 5 0 0 0 0 0 6 4)
    (1 0 0 1 0 1 1 1 0 1 1)
    (1 0 1 1 0 1 3 1 0 0 1)
    (1 0 1 0 0 0 0 0 0 0 1)
    (1 1 1 1 1 1 1 1 1 1 1)))

;; ============================================================
;;  MAPA NIVEL 2
;; ============================================================

;0 = suelo libre
;1 = pared
;2 = tile rojo
;3 = tile triángulo
;4 = estrella
;5 = puerta circulo verde
;6 = puerta triangulo rojo
;7 = tile cuadrado
;8 = puerta color rojo
;9 = puerta circulo vertical
;10 = puerta circulo horizontal
;11 = puerta color verde
;12 = tile circulo
;13 = tile verde
;14 = puerta cuadrado
;15 = puerta color azul
;16 = tile azul

(define NIVEL2
  '((0 0 1 1 1 1 1 0 0 0 4)
    (0 1 1 0 7 1 0 0 1 1 1)
    (1 1 0 0 1 1 8 1 0 0 2)
    (1 0 0 1 0 0 0 9 0 1 1)
    (0 0 0 11 0 0 0 1 1 12 1)
    (1 0 0 1 0 0 0 1 1 0 1)
    (1 1 14 1 1 12 1 13 1 0 1)
    (1 0 0 0 1 1 1 10 1 15 1)
    (16 0 0 0 0 7 0 0 0 0 0)))

;; ============================================================
;;  ESTRUCTURA DEL ESTADO
;; ============================================================

(define-struct estado
  (pantalla bola-x bola-y color forma puerta1 puerta2 nivel-completo tiempo nivel))

(define ESTADO-INICIAL
  (make-estado "inicio" 105 315 "green" "circulo" #false #false #false 0 1))

;; ============================================================
;;  DIBUJAR BOLA
;; ============================================================
(define (dibujar-bola color forma)
  (cond
    [(equal? forma "circulo")
     (circle RADIO "solid" color)]
    [(equal? forma "cuadrado")
     (rectangle (* 2 RADIO) (* 2 RADIO) "solid" color)]
    [(equal? forma "triangulo")
     (triangle (* 2 RADIO) "solid" color)]))

;; ============================================================
;;  DIBUJAR MAPA
;; ============================================================

(define (puerta-abierta? tipo-puerta color forma)
  (cond
    [(= tipo-puerta 8)  (equal? color "red")]
    [(= tipo-puerta 9)  (equal? forma "circulo")]
    [(= tipo-puerta 10) (equal? forma "circulo")]
    [(= tipo-puerta 11) (equal? color "green")]
    [(= tipo-puerta 14) (equal? forma "cuadrado")]
    [(= tipo-puerta 15) (equal? color "blue")]
    [else #false]))

(define (dibujar-fila fila estado)
  (cond
    [(empty? fila) (empty-scene 0 TAMANO-CELDA)]
    [(= (first fila) 1)
     (beside TEXTURA-PARED (dibujar-fila (rest fila) estado))]
    [(= (first fila) 2)
     (beside TILE-ROJO (dibujar-fila (rest fila) estado))]
    [(= (first fila) 3)
     (beside TILE-TRIANGULO (dibujar-fila (rest fila) estado))]
    [(= (first fila) 4)
     (beside TILE-ESTRELLA (dibujar-fila (rest fila) estado))]
    [(= (first fila) 5)
     (beside (if (estado-puerta1 estado) PUERTA-ABIERTA PUERTA-CERRADA-CV)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 6)
     (beside (if (estado-puerta2 estado) PUERTA-ABIERTA PUERTA-CERRADA-TR)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 7)
     (beside TILE-CUADRADO (dibujar-fila (rest fila) estado))]
    [(= (first fila) 8)
     (beside (if (and (puerta-abierta? 8 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 2 6) 70))
                 PUERTA-ABIERTA-H PUERTA-COLOR-ROJO)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 9)
     (beside (if (and (puerta-abierta? 9 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 3 7) 70))
                 PUERTA-ABIERTA PUERTA-CIRCULO-V)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 10)
     (beside (if (and (puerta-abierta? 10 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 7 7) 70))
                 PUERTA-ABIERTA-H PUERTA-CIRCULO-H)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 11)
     (beside (if (and (puerta-abierta? 11 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 4 3) 70))
                 PUERTA-ABIERTA PUERTA-COLOR-VERDE)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 12)
     (beside TILE-CIRCULO (dibujar-fila (rest fila) estado))]
    [(= (first fila) 13)
     (beside TILE-VERDE (dibujar-fila (rest fila) estado))]
    [(= (first fila) 14)
     (beside (if (and (puerta-abierta? 14 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 6 2) 70))
                 PUERTA-ABIERTA-H PUERTA-CUADRADO)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 15)
     (beside (if (and (puerta-abierta? 15 (estado-color estado) (estado-forma estado))
                      (< (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) 7 9) 70))
                 PUERTA-ABIERTA-H PUERTA-COLOR-AZUL)
             (dibujar-fila (rest fila) estado))]
    [(= (first fila) 16)
     (beside TILE-AZUL (dibujar-fila (rest fila) estado))]
    [else
     (beside TEXTURA-SUELO (dibujar-fila (rest fila) estado))]))

(define (dibujar-mapa mapa estado)
  (cond
    [(empty? mapa) (empty-scene (* 11 TAMANO-CELDA) 0)]
    [else
     (above (dibujar-fila (first mapa) estado)
            (dibujar-mapa (rest mapa) estado))]))

;; ============================================================
;;  DIBUJAR NIVEL 1
;; ============================================================
(define (dibujar-nivel1 estado)
  (let* ([panel (if (estado-nivel-completo estado) IMAGEN-OPCIONES2 IMAGEN-OPCIONES)]
         [mapa-con-bola (place-image (dibujar-bola (estado-color estado)
                                                   (estado-forma estado))
                                     (estado-bola-x estado)
                                     (estado-bola-y estado)
                                     (dibujar-mapa NIVEL1 estado))]
         [mapa-con-panel (beside mapa-con-bola panel)]
         [escena (place-image mapa-con-panel
                              (/ ANCHO 2)
                              (/ ALTO 2)
                              (rectangle ANCHO ALTO "solid" (color 121 121 142 255)))])
    (if (estado-nivel-completo estado)
        (place-image
         (text "PRESS SPACE TO CONTINUE" 22 "black")
         400 480
         (place-image
          (overlay
           (above
            (text (string-append "Time: " (number->string (estado-tiempo estado)) "  Par: 20") 25 "black")
            (text "Level Complete: +1000" 25 "black")
            (text "Time Bonus: +0" 25 "black"))
           (rectangle 380 120 "solid" (color 255 255 255 180)))
          400 390
          (place-image
           IMAGEN-TEXTO-F
           400 280
           escena)))
        escena)))

;; ============================================================
;;  DIBUJAR NIVEL 2
;; ============================================================

(define (dibujar-nivel2 estado)
  (let* ([panel (if (estado-nivel-completo estado) IMAGEN-OPCIONES2 IMAGEN-OPCIONES2)]
         [mapa-con-bola (place-image (dibujar-bola (estado-color estado)
                                                   (estado-forma estado))
                                     (estado-bola-x estado)
                                     (estado-bola-y estado)
                                     (dibujar-mapa NIVEL2 estado))]
         [mapa-con-panel (beside mapa-con-bola panel)]
         [escena (place-image mapa-con-panel
                              (/ ANCHO 2)
                              (/ ALTO 2)
                              (rectangle ANCHO ALTO "solid" (color 121 121 142 255)))])
    (if (estado-nivel-completo estado)
        (place-image
         (text "PRESS SPACE TO CONTINUE" 22 "black")
         400 480
         (place-image
          (overlay
           (above
            (text (string-append "Time: " (number->string (estado-tiempo estado)) "  Par: 20") 25 "black")
            (text "Level Complete: +1000" 25 "black")
            (text "Time Bonus: +0" 25 "black"))
           (rectangle 380 120 "solid" (color 255 255 255 180)))
          400 390
          (place-image
           IMAGEN-TEXTO-F
           400 280
           escena)))
        escena)))

;; ============================================================
;;  AUDIO
;; ============================================================

(define AUDIO-NIVEL1
  (path->string
   (build-path (current-directory)
               "AUDIO NIVEL 1.mp3")))

(define AUDIO-NIVEL2
  (path->string
   (build-path (current-directory)
               "AUDIO NIVEL 2.mp3")))

(define (reproducir-nivel1)
  (set! proceso-audio
        (thread
         (lambda ()
           (system
            (string-append
             "powershell -c \"Add-Type -AssemblyName presentationCore; "
             "$player = New-Object System.Windows.Media.MediaPlayer; "
             "$player.Open([uri]'"
             AUDIO-NIVEL1
             "'); "
             "$player.Play(); "
             "while($true){ if($player.Position -ge $player.NaturalDuration.TimeSpan){ $player.Position = [TimeSpan]::Zero; $player.Play() }; Start-Sleep -m 500 }\""))))))

(define (reproducir-nivel2)
  (set! proceso-audio
        (thread
         (lambda ()
           (system
            (string-append
             "powershell -c \"Add-Type -AssemblyName presentationCore; "
             "$player = New-Object System.Windows.Media.MediaPlayer; "
             "$player.Open([uri]'"
             AUDIO-NIVEL2
             "'); "
             "$player.Play(); "
             "while($true){ if($player.Position -ge $player.NaturalDuration.TimeSpan){ $player.Position = [TimeSpan]::Zero; $player.Play() }; Start-Sleep -m 500 }\""))))))

(define proceso-audio #f)


(define (detener-audio)
  (when proceso-audio
    (kill-thread proceso-audio)
    (system "taskkill /F /IM powershell.exe /T")
    (set! proceso-audio #f)))

(plumber-add-flush! (current-plumber)
                    (lambda (handle)
                      (detener-audio)))

;; ============================================================
;;  LOGICA DEL JUEGO
;; ============================================================
(define (celda-en-mapa mapa fila col)
  (list-ref (list-ref mapa fila) col))

(define (puede-moverse? x y estado)
  (let* ([col  (inexact->exact (floor (/ x TAMANO-CELDA)))]
         [fila (inexact->exact (floor (/ y TAMANO-CELDA)))]
         [mapa-actual (if (= (estado-nivel estado) 1) NIVEL1 NIVEL2)]
         [celda (celda-en-mapa mapa-actual fila col)]
         [color (estado-color estado)]
         [forma (estado-forma estado)])
    (not (or (= celda 1)
             (and (= celda 5) (not (estado-puerta1 estado)))
             (and (= celda 6) (not (estado-puerta2 estado)))
             (and (= celda 8)  (not (puerta-abierta? 8  color forma)))
             (and (= celda 9)  (not (puerta-abierta? 9  color forma)))
             (and (= celda 10) (not (puerta-abierta? 10 color forma)))
             (and (= celda 11) (not (puerta-abierta? 11 color forma)))
             (and (= celda 14) (not (puerta-abierta? 14 color forma)))
             (and (= celda 15) (not (puerta-abierta? 15 color forma)))))))

(define (distancia-puerta bola-x bola-y puerta-f puerta-c)
  (sqrt (+ (expt (- bola-x (+ (* puerta-c TAMANO-CELDA) (/ TAMANO-CELDA 2))) 2)
           (expt (- bola-y (+ (* puerta-f TAMANO-CELDA) (/ TAMANO-CELDA 2))) 2))))

(define (verificar-tiles estado)
  (let* ([col  (inexact->exact (floor (/ (estado-bola-x estado) TAMANO-CELDA)))]
         [fila (inexact->exact (floor (/ (estado-bola-y estado) TAMANO-CELDA)))]
         [mapa-actual (if (= (estado-nivel estado) 1) NIVEL1 NIVEL2)]
         [celda (celda-en-mapa mapa-actual fila col)]
         [nuevo-color (cond
                        [(= celda 2)  "red"]
                        [(= celda 13) "green"]
                        [(= celda 16) "blue"]
                        [else (estado-color estado)])]
         [nueva-forma (cond
                        [(= celda 3)  "triangulo"]
                        [(= celda 7)  "cuadrado"]
                        [(= celda 12) "circulo"]
                        [else (estado-forma estado)])]
         [dist-p1 (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) PUERTA1-F PUERTA1-C)]
         [dist-p2 (distancia-puerta (estado-bola-x estado) (estado-bola-y estado) PUERTA2-F PUERTA2-C)]
         [puerta1-abierta (and (< dist-p1 70)
                               (equal? nuevo-color "green")
                               (equal? nueva-forma "circulo"))]
         [puerta2-abierta (and (< dist-p2 70)
                               (equal? nuevo-color "red")
                               (equal? nueva-forma "triangulo"))]
         [nivel-completo (= celda 4)])
    (make-estado (estado-pantalla estado)
                 (estado-bola-x estado)
                 (estado-bola-y estado)
                 nuevo-color
                 nueva-forma
                 puerta1-abierta
                 puerta2-abierta
                 nivel-completo
                 (estado-tiempo estado)
                 (estado-nivel estado))))

;; ============================================================
;;  MOVER BOLA
;; ============================================================
(define (mover-bola estado tecla)
  (cond
    [(and (equal? tecla " ")
          (estado-nivel-completo estado))
     (detener-audio)
     (reproducir-nivel2)  
     (make-estado "juego" 105 315 "red" "triangulo" #false #false #false 0 2)]
    [(or (not (equal? (estado-pantalla estado) "juego"))
         (estado-nivel-completo estado))
     estado]
    [else
     (let* ([x (estado-bola-x estado)]
            [y (estado-bola-y estado)]
            [nuevo-x (cond
                       [(equal? tecla "right") (+ x VELOCIDAD)]
                       [(equal? tecla "left")  (- x VELOCIDAD)]
                       [else x])]
            [nuevo-y (cond
                       [(equal? tecla "up")   (- y VELOCIDAD)]
                       [(equal? tecla "down") (+ y VELOCIDAD)]
                       [else y])]
            [nuevo-estado (if (puede-moverse? nuevo-x nuevo-y estado)
                              (make-estado (estado-pantalla estado)
                                           nuevo-x nuevo-y
                                           (estado-color estado)
                                           (estado-forma estado)
                                           (estado-puerta1 estado)
                                           (estado-puerta2 estado)
                                           #false
                                           (estado-tiempo estado)
                                           (estado-nivel estado))
                              estado)])
       (verificar-tiles nuevo-estado))]))

;; ============================================================
;;  DIBUJAR GENERAL
;; ============================================================
(define (dibujar estado)
  (cond
    [(equal? (estado-pantalla estado) "inicio") IMAGEN-INICIO]
    [(equal? (estado-pantalla estado) "inicio2") IMAGEN-INICIO2]
    [(= (estado-nivel estado) 1) (dibujar-nivel1 estado)]
    [(= (estado-nivel estado) 2) (dibujar-nivel2 estado)]
    [else (dibujar-nivel1 estado)]))

;; ============================================================
;;  MANEJO DEL MOUSE
;; ============================================================
(define (mover-mouse estado x y evento)
  (cond
    [(equal? (estado-pantalla estado) "juego") estado]
    [(and (equal? evento "button-down")
          (>= x 54) (<= x 319)
          (>= y 603) (<= y 667))
     (begin
       (reproducir-nivel1)
       (make-estado "juego" 105 315 "green" "circulo" #false #false #false 0 1))]
    [(and (>= x 54) (<= x 319)
          (>= y 603) (<= y 667))
     (make-estado "inicio2" 105 315 "green" "circulo" #false #false #false 0 1)]
    [else
     (make-estado "inicio" 105 315 "green" "circulo" #false #false #false 0 1)]))

;; ============================================================
;;  TIEMPO
;; ============================================================

(define (actualizar-tiempo estado)
  (if (and (equal? (estado-pantalla estado) "juego")
           (not (estado-nivel-completo estado)))
      (make-estado (estado-pantalla estado)
                   (estado-bola-x estado)
                   (estado-bola-y estado)
                   (estado-color estado)
                   (estado-forma estado)
                   (estado-puerta1 estado)
                   (estado-puerta2 estado)
                   (estado-nivel-completo estado)
                   (+ (estado-tiempo estado) 1)
                   (estado-nivel estado))
      estado))

;; ============================================================
;;  ARRANCAR EL JUEGO
;; ============================================================
(dynamic-wind
  void
  (lambda ()
    (big-bang ESTADO-INICIAL
      (to-draw   dibujar)
      (on-key    mover-bola)
      (on-mouse  mover-mouse)
      (on-tick   actualizar-tiempo 1)))
  (lambda ()
    (detener-audio)))