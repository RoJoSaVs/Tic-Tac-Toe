#lang racket
(require "matrix.rkt")
(require "validations.rkt")

 
(define (greedyAlgorithm matrix)
    (cond
        ((not (null? (wins? 0 0 matrix 1)));Gana PC
            (greedyAlgorithmAux matrix (wins? 0 0 matrix 1))
        )
        ((not (null? (wins? 0 0 matrix 2)));Gana User
            (greedyAlgorithmAux matrix (wins? 0 0 matrix 2))
        )
        (else
            (greedyAlgorithmAux matrix (cdr (bestPos (probabilityList matrix 0 0 '()))))
        )
    )
)
(define (greedyAlgorithmAux matrix pos)
    (setValueMatrix matrix (car pos) (cadr pos) 1)
)

(define (wins? x y matrix num)
    (cond
        ((equal? x (lenghtMatrix (car matrix)))
            (wins? 0 (+ y 1) matrix num)
        )
        ((equal? y (lenghtMatrix matrix))
            '()
        )
        ((not (or (equal? (getValue matrix y x) 2) (equal? (getValue matrix y x) 1)))
            (cond
                ((win (setValueMatrix matrix x y num) num)
                    (list x y)
                )
                (else
                    (wins? (+ x 1) y matrix num)
                )
            )
        )
        (else
            (wins? (+ x 1) y matrix num)
        )
    )
)

(define (bestPos mainList)
    (cond
        ((null? mainList)
            #f
        )
        (else
            (bestPosAux (car mainList) (cdr mainList))
        )
    )
)

(define (bestPosAux value subList)
    (cond
        ((null? subList)
            value
        )
        ((< (caar subList) (car value))
            (bestPosAux (car subList) (cdr subList))
        )
        (else
            (bestPosAux value (cdr subList))
        )
    )
)

(define (probabilityList matrix x y subList)
    (cond
        ((equal? x (lenghtMatrix (car matrix)))
            (probabilityList matrix 0 (+ y 1) subList)
        )
        ((equal? y (lenghtMatrix matrix))
            (invertMatrix subList)
        )
        ((not (or (equal? (getValue matrix y x) 2) (equal? (getValue matrix y x) 1)))
            (probabilityList matrix (+ x 1) y (cons (list (- (possibleWins (setValueMatrix matrix x y 1) 1) (possibleWins (setValueMatrix matrix x y 2) 2)) x y) subList))
        )
        (else
            (probabilityList matrix (+ x 1) y subList)
        )
    )
)