#lang racket

;Retorna una matriz con valores que no tienen importancia para el desarrollo del juego
(define (createMatrix x y)
    (crateMatrixAux x y '())
)

;Crea la matriz de "y" filas por "x" columnas estableciendo cada valor en 0
(define (crateMatrixAux x y matrix)
    (cond (
        (zero? y) 
            matrix)
        (else (crateMatrixAux x (- y 1) (append matrix (list (make-list x 0))) )
        )
    )
)
;Recorre la matriz en busca de una posicion deseada "y" , "x" y cambia el valor de esa posicion 
(define (setValueMatrix matrix x y value)
    (cond
        ((zero? y) 
            (cons (replaceSublist (car matrix) x value) (cdr matrix))
        )
        (else 
            (cons (car matrix) (setValueMatrix (cdr matrix) x (- y 1) value))
        )
    )
)
;Remplaza el valor que se ubica en la posicion de "x"
(define (replaceSublist subList x value)
    (cond
        ((zero? x)
            (cons value (cdr subList))
        )
        (else 
            (cons (car subList) (replaceSublist (cdr subList) (- x 1) value))
        )    
    )
)

;Obtiene el elemento que se encuentre en la posicion "y", "x"
(define (getValue matrix y x)
    (cond 
        ((zero? y)
            (getValueAux (car matrix) x)
        )
        (else
            (getValue (cdr matrix) (- y 1) x)
        )
    )
)
;Recorre una fila hasta encontrar el elemento en la posicion "x"
(define (getValueAux subList x)
    (cond
        ((zero? x)
            (car subList)
        )
        (else
            (getValueAux (cdr subList) (- x 1))
        )
    )
)
;Retorna la cantidad de movimientos posibles
(define (possibleWins matrix num)
    (+ (horizontal matrix num 0) (vertical matrix num 0) (diagonal matrix num 0) (diagonalUpward matrix num 0))
)
;Verifica si un hay una fila llena de num o 0
(define (horizontal matrix num possibility)
    (cond
        ((null? matrix) 
            possibility
        )
        ((verifyLine (car matrix) num) 
            (horizontal (cdr matrix) num (+ possibility 1)))
        (else
            (horizontal (cdr matrix) num possibility)
        )
    )
)
;Revisa que una linea este llena con num o 0
(define (verifyLine subList num)
    (cond
        ((null? subList) 
            #t
        )
        ((or (zero? (car subList)) (equal? (car subList) num)) 
            (verifyLine (cdr subList) num)
        )
        (else 
            #f
        )
    )
)
;Verifica si un hay una columna llena de un numero
(define (vertical matrix num possibility)
    (horizontal (transposed matrix) num possibility)
)
;Transpone la matriz
(define (transposed matrix)
    (cond
        ((null? matrix)
            '()
        )
        ((null? (car matrix))
            '()
        )
        (else
            (cons (getColumn matrix) (transposed (deleteColumn matrix)))
        )
    )
)
;Obtiene la primera columna de una matriz dada
(define (getColumn matrix)
    (cond
        ((null? matrix)
            '()
        )
        (else
            (cons (caar matrix) (getColumn (cdr matrix)))
        )
    )
)
;Elimina la primera columna de una matriz dada
(define (deleteColumn matrix)
    (cond
        ((or (null? matrix) (null? (car matrix)))
            '()
        )
        (else
            (cons (cdar matrix) (deleteColumn (cdr matrix)))
        )
    )
)
;Obtinen todas las diagonales validas descendentes de la matriz 
(define (diagonal matrix num possibility)
    (cond
        ((null? matrix) 
            possibility
        )
        (else
            (+ (diagonalUp matrix num possibility) (diagonalDown (cdr matrix) num possibility))
        )

    )
)
;Obtiene las diagonales validas que se encuentran por debajo de la diagonal principal
(define (diagonalDown matrix num possibility)
    (cond
        ((null? matrix)
            possibility
        )
        ((and (diagonalChecker matrix num) (> (lenghtMatrix matrix) 2) (> (lenghtMatrix (transposed matrix)) 2))
            (diagonalDown (cdr matrix) num (+ possibility 1))
        )
        (else
            (diagonalDown (cdr matrix) num possibility)
        )
    )
)
;Obtiene la diagonal principal de la matriz y las que se encuentran sobre la principal 
(define (diagonalUp matrix num possibility)
    (cond
        ((null? matrix)
            possibility
        )
        ((and (diagonalChecker matrix num) (> (lenghtMatrix matrix) 2) (> (lenghtMatrix (transposed matrix)) 2))
            (diagonalUp (deleteColumn matrix) num (+ possibility 1))
        )
        (else
            (diagonalUp (deleteColumn matrix) num possibility)
        )
    )
)

;Obtiene la longitud de la matriz 
(define (lenghtMatrix matrix)
    (cond
        ((null? matrix)
            0
        )
        (else
            (+ 1 (lenghtMatrix (cdr matrix)))
        )
    )
)
;Valida que una diagonal contenga a num o 0
(define (diagonalChecker matrix num)
    (cond
        ((or (null? matrix) (null? (car matrix)))
            #t
        )
        ((or (equal? (caar matrix) num) (zero? (caar matrix)))
            (diagonalChecker (deleteColumn (cdr matrix)) num)
        )
        (else
            #f
        )
    )
)
;Revisa la diagonal ascendente
(define (diagonalUpward matrix num possibility)
    (diagonal (invertMatrix matrix) num possibility)
)


;Invierte la matriz de abajo hacia arriba
(define (invertMatrix matrix)
    (cond
        ((null? matrix) 
            '()
        )
        (else
            (append (invertMatrix (cdr matrix)) (list (car matrix)))
        )
    )
)
(provide (all-defined-out))