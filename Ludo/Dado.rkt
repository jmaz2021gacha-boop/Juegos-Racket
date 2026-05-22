(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define juego(open-viewport "dice" 1000 600))

(define (dado n)
  ((draw-solid-rectangle juego)(make-posn 100 100)50 50 "wheat")
  (cond
    ((= n 1)[begin
              ((draw-solid-ellipse juego)(make-posn 120 120)10 10 "black")
              ])
    ((= n 2)[begin
              ((draw-solid-ellipse juego)(make-posn 105 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 135)10 10 "black")
              ])
    ((= n 3)[begin
              ((draw-solid-ellipse juego)(make-posn 105 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 120 120)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 135)10 10 "black")
              ])
    ((= n 4)[begin
              ((draw-solid-ellipse juego)(make-posn 105 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 105 135)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 135)10 10 "black")
              ])
    ((= n 5)[begin
              ((draw-solid-ellipse juego)(make-posn 105 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 120 120)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 105 135)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 135)10 10 "black")
              ])
    ((= n 6)[begin
              ((draw-solid-ellipse juego)(make-posn 105 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 105)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 105 120)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 120)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 105 135)10 10 "black")
              ((draw-solid-ellipse juego)(make-posn 135 135)10 10 "black")
              ])
    
    )
  )

(dado 1)
(sleep 1)
(dado 2)
(sleep 1)
(dado 3)
(sleep 1)
(dado 4)
(sleep 1)
(dado 5)
(sleep 1)
(dado 6)
(sleep 1)











