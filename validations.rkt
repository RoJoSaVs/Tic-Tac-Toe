#lang racket

;Retorna #t si se encuentra una secuencia ganadora en la matriz para un valor dado, en caso contrario retorna #f
(define (win matrix num)
    (or (horizontalValid matrix num) (verticalValid matrix num) (diagonalUpwardValid matrix num) (diagonalValid matrix num))
)

;Verifica si un hay una fila llena de num
(define (horizontalValid matrix num)
    (cond
        ((null? matrix) 
            #f
        )
        ((verifyLineValid (car matrix) num) 
            #t
        )
        (else
            (horizontalValid (cdr matrix) num)
        )
    )
)
;Revisa que una linea este llena con num o 0
(define (verifyLineValid subList num)
    (cond
        ((null? subList) 
            #t
        )
        ((equal? (car subList) num) 
            (verifyLineValid (cdr subList) num)
        )
        (else 
            #f
        )
    )
)
;Verifica si un hay una columna llena de un numero
(define (verticalValid matrix num)
    (horizontalValid (transposedValid matrix) num)
)
;Transpone la matriz
(define (transposedValid matrix)
    (cond
        ((null? matrix)
            '()
        )
        ((null? (car matrix))
            '()
        )
        (else
            (cons (getColumnValid matrix) (transposedValid (deleteColumnValid matrix)))
        )
    )
)
;Obtiene la primera columna de una matriz dada
(define (getColumnValid matrix)
    (cond
        ((null? matrix)
            '()
        )
        (else
            (cons (caar matrix) (getColumnValid (cdr matrix)))
        )
    )
)
;Elimina la primera columna de una matriz dada
(define (deleteColumnValid matrix)
    (cond
        ((or (null? matrix) (null? (car matrix)))
            '()
        )
        (else
            (cons (cdar matrix) (deleteColumnValid (cdr matrix)))
        )
    )
)
;Obtinen todas las diagonales validas descendentes de la matriz 
(define (diagonalValid matrix num)
    (cond
        ((null? matrix) 
            #f
        )
        ((or (diagonalUpValid matrix num) (diagonalDownValid (cdr matrix) num))
            #t
        )
        (else
            #f
        )
    )
)
;Obtiene las diagonales validas que se encuentran por debajo de la diagonal principal
(define (diagonalDownValid matrix num)
    (cond
        ((null? matrix)
            #f
        )
        ((and (diagonalCheckerValid matrix num) (> (lenghtMatrixValid matrix) 2) (> (lenghtMatrixValid (transposedValid matrix)) 2))
            #t
        )
        (else
            (diagonalDownValid (cdr matrix) num)
        )
    )
)
;Obtiene la diagonal principal de la matriz y las que se encuentran sobre la principal 
(define (diagonalUpValid matrix num)
    (cond
        ((null? matrix)
            #f
        )
        ((and (diagonalCheckerValid matrix num) (> (lenghtMatrixValid matrix) 2) (> (lenghtMatrixValid (transposedValid matrix)) 2))
            #t
        )
        (else
            (diagonalUpValid (deleteColumnValid matrix) num)
        )
    )
)

;Obtiene la longitud de la matriz 
(define (lenghtMatrixValid matrix)
    (cond
        ((null? matrix)
            0
        )
        (else
            (+ 1 (lenghtMatrixValid (cdr matrix)))
        )
    )
)
;Valida que una diagonal contenga a num o 0
(define (diagonalCheckerValid matrix num)
    (cond
        ((or (null? matrix) (null? (car matrix)))
            #t
        )
        ((equal? (caar matrix) num)
            (diagonalCheckerValid (deleteColumnValid (cdr matrix)) num)
        )
        (else
            #f
        )
    )
)
;Revisa la diagonal ascendente
(define (diagonalUpwardValid matrix num)
    (diagonalValid (invertMatrixValid matrix) num)
)


;Invierte la matriz de abajo hacia arriba
(define (invertMatrixValid matrix)
    (cond
        ((null? matrix) 
            '()
        )
        (else
            (append (invertMatrixValid (cdr matrix)) (list (car matrix)))
        )
    )
)
(provide (all-defined-out))
;(win '((1 2 1 1) (2 2 1 1) (1 1 1 2) (2 2 2 2)) 2)