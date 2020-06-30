#lang racket
(require "matrix.rkt")
(require "validations.rkt")

#|En caso de tener la PC una opcion de gane toma la posicion en la cual puede ganar y la cambia por 1
Encuentra un posible gane del jugador y lo bloquea
Encuentra una posicion optima basado en la informacion del tablero que tiene en el momento 
buscando un posible gane
|#
;(greedyAlgorithm (matriz de juego))
(define (greedyAlgorithm matrix)
    (cond
        ((not (null? (wins? 0 0 matrix 2)));Gana PC
            (greedyAlgorithmAux matrix (wins? 0 0 matrix 2))
        )
        ((not (null? (wins? 0 0 matrix 1)));Gana User
            (greedyAlgorithmAux matrix (wins? 0 0 matrix 1))
        )
        (else
            (greedyAlgorithmAux matrix (cdr (bestPos (probabilityList matrix))))
        )
    )
)
#|Sustituye 0 por 1 en el tablero en la posicion que entra en la lista|#
;(greedyAlgorithmAux (matriz de juego) (lista con valores x, y))
(define (greedyAlgorithmAux matrix pos)
    (setValueMatrix matrix (cadr pos) (car pos) 2)
)

#|Verifica en que posicion se puede dar un gane y retorna la posicion en caso de darse un posible gane|#
;(wins? (posicion x) (posicion x) (matriz de juego) (valor de juego))
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
                ((win (setValueMatrix matrix y x num) num)
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
#|Evalua cual es la opcion mas optima para jugar|#
;(bestPos (lista de opciones a jugar))
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
#|Recorre una lista con posibles opciones y determina cual es la mejor|#
;(bestPosAux (valor a probar) (lista de opciones))
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

(define (probabilityList matrix)
    (probabilityListAux matrix 0 0 '())
)
#|Sustituye temporalmente una posición vacía y evalua si tiene posibilidad de gane a futuro
basandose en la informacion del momento y retorna las posiciones|#
;(probabilityListAux (matriz de juego) (valor de x) (valor de y) (lista vacia))
(define (probabilityListAux matrix x y subList)
    (cond
        ((equal? x (lenghtMatrix (car matrix)))
            (probabilityListAux matrix 0 (+ y 1) subList)
        )
        ((equal? y (lenghtMatrix matrix))
            (invertMatrix subList)
        )
        ((not (or (equal? (getValue matrix y x) 2) (equal? (getValue matrix y x) 1)))
            (probabilityListAux matrix (+ x 1) y (cons (list 
            (- (possibleWins (setValueMatrix matrix y x 2) 1) (possibleWins (setValueMatrix matrix y x 2) 2)) x y) subList))
        )
        (else
            (probabilityListAux matrix (+ x 1) y subList)
        )
    )
)
(provide (all-defined-out))