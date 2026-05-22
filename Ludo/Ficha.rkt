(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define juego(open-viewport "Ludo" 1200 700))

(define (ficha color)
  ((draw-solid-rectangle juego)(make-posn 200 100)8 22 color)
  ((draw-solid-rectangle juego)(make-posn 191 122)25 8 color)
  ((draw-solid-rectangle juego)(make-posn 194 120)19 7 color)
  ((draw-solid-ellipse juego)(make-posn 196 90)15 15 color)
  )

(ficha "blue")









































