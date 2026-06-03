#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require "big head football.rkt")
(require "the-right-mix.rkt")

;; ================================================
;; GRÁFICOS DEL MENÚ
;; ================================================
(define (dibujar-menu estado)
  (place-image (text "SELECCIONA UN JUEGO" 36 "white") 400 100
     (place-image (text "[1] Big Head Football" 24 "yellow") 400 220
        (place-image (text "[2] The Right Mix" 24 "cyan") 400 300
           (place-image (text "[ESC] Salir del Arcade" 18 "red") 400 450
              (rectangle 800 500 "solid" (make-color 20 30 40)))))))

;; ================================================
;; CONTROLES DEL MENÚ
;; ================================================
(define (control-menu estado tecla)
  (cond
    [(string=? tecla "1") "futbol"]
    [(string=? tecla "2") "mix"]
    [(string=? tecla "escape") "salir"]
    [else estado]))

;; Detiene el menú cuando el estado cambia a un juego o a salir
(define (terminar? estado)
  (or (string=? estado "futbol") 
      (string=? estado "mix")
      (string=? estado "salir")))

;; ================================================
;; EL BUCLE PRINCIPAL (LA MAGIA RECURSIVA)
;; ================================================
(define (bucle-principal)
  ;; 1. Arrancamos el menú y guardamos lo que el jugador elija
  (define eleccion
    (big-bang "inicio"
      (to-draw dibujar-menu)
      (on-key control-menu)
      (stop-when terminar?)
      (name "Arcade Principal")))

  ;; 2. Evaluamos la elección
  (cond
    [(string=? eleccion "futbol") 
     (iniciar-futbol)    ;; Abre el fútbol y el programa se "pausa" aquí hasta que lo cierres
     (bucle-principal)]  ;; Cuando cierras el fútbol, se vuelve a llamar al menú
     
    [(string=? eleccion "mix") 
     (iniciar-mix)       ;; Abre The Right Mix
     (bucle-principal)]  ;; Cuando lo cierras, se vuelve a llamar al menú
     
    [(string=? eleccion "salir") 
     (displayln "¡Gracias por jugar!")] ;; Termina el programa por completo
    
    [else 
     (displayln "Ventana del menú cerrada.")]))

;; ================================================
;; ARRANCAR EL PROGRAMA
;; ================================================
(bucle-principal)
