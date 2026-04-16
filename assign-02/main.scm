;;defining a 2d space
(define (space x y)
    (list x y 1))

(define (make-line x1 y1 x2 y2)
    (list (space x1 y1) (space x2 y2)))

(display (make-line 0 0 1 1))
(newline)