(define (imprimir a)
  (if (= a 0)
      (void)
      (begin
        (display "*")
        (imprimir (- a 1)))))

(define s (string #\c #\b #\c #\a #\b #\c #\d #\e #\e #\a #\b #\b #\b #\d #\c #\a #\c #\c #\b #\d #\a))

(define (azucar t s a b c d e)
  (if (= t (- (string-length s) 1))
      (begin
        (display "a: ") (imprimir a) (newline)
        (display "b: ") (imprimir b) (newline)
        (display "c: ") (imprimir c) (newline)
        (display "d: ") (imprimir d) (newline)
        (display "e: ") (imprimir e) (newline))
      (if (char=? (string-ref s t) #\a)
          (begin 
            (azucar (+ t 1) s (+ a 1) b c d e))
          (if (char=? (string-ref s t) #\b)
              (azucar (+ t 1) s a (+ b 1) c d e)
              (if (char=? (string-ref s t) #\c)
                  (azucar (+ t 1) s a b (+ c 1) d e)
                  (if (char=? (string-ref s t) #\d)
                      (azucar (+ t 1) s a b c (+ d 1) e)
                      (if (char=? (string-ref s t) #\e)
                          (azucar (+ t 1) s a b c d (+ e 1))
                          (azucar (+ t 1) s a b c d e))))))))
(azucar 0 s 0 0 0 0 0)



(define (arr cadena i j temporal)
  (begin
    (string-set! cadena i (string-ref cadena j))
    (string-set! cadena j temporal)))

(define (inver cadena i j)
  (cond ((< i j)
         (begin
           (arr cadena i j (string-ref cadena i))
           (inver cadena (+ i 1) (- j 1))))))

(define (principal)
  (define dos (string #\a #\r #\r #\o #\z))
  (inver dos 0 (- (string-length dos) 1))
  (displayln dos))

(define (maelopa t s a b c d e)
  (if (not (char=? (string-ref s t) #\space)
           (maelopa (+ t 1) s (+ a 1) b c d e)
           (begin
             (define 

(principal)



(define (arigato)
(if (and (char=? (string-ref s (- t 1 )) #\space )(or (char=? (string-ref s t) #\a)
                                                      (char=? (string-ref s t) #\e)
                                                      (char=? (string-ref s t) #\i)
                                                      (char=? (string-ref s t) #\o)
                                                      (char=? (string-ref s t) #\u)))
    
                                                   
  

