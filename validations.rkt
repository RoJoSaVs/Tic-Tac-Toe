#lang racket

(define (win matrix num)
    (or (horizontal matrix num) (vertical matrix num) (diagonalUpward matrix num) (diagonal matrix num))
)

;Verifica si un hay una fila llena de num
(define (horizontal matrix num)
    (cond
        ((null? matrix) 
            #f
        )
        ((verifyLine (car matrix) num) 
            #t
        )
        (else
            (horizontal (cdr matrix) num)
        )
    )
)
;Revisa que una linea este llena con num o 0
(define (verifyLine subList num)
    (cond
        ((null? subList) 
            #t
        )
        ((equal? (car subList) num) 
            (verifyLine (cdr subList) num)
        )
        (else 
            #f
        )
    )
)
;Verifica si un hay una columna llena de un numero
(define (vertical matrix num)
    (horizontal (transposed matrix) num)
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
(define (diagonal matrix num)
    (cond
        ((null? matrix) 
            #f
        )
        ((or (diagonalUp matrix num) (diagonalDown (cdr matrix) num))
            #t
        )
        (else
            #f
        )
    )
)
;Obtiene las diagonales validas que se encuentran por debajo de la diagonal principal
(define (diagonalDown matrix num)
    (cond
        ((null? matrix)
            #f
        )
        ((and (diagonalChecker matrix num) (> (lenghtMatrix matrix) 2) (> (lenghtMatrix (transposed matrix)) 2))
            #t
        )
        (else
            (diagonalDown (cdr matrix) num)
        )
    )
)
;Obtiene la diagonal principal de la matriz y las que se encuentran sobre la principal 
(define (diagonalUp matrix num)
    (cond
        ((null? matrix)
            #f
        )
        ((and (diagonalChecker matrix num) (> (lenghtMatrix matrix) 2) (> (lenghtMatrix (transposed matrix)) 2))
            #t
        )
        (else
            (diagonalUp (deleteColumn matrix) num)
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
        ((equal? (caar matrix) num)
            (diagonalChecker (deleteColumn (cdr matrix)) num)
        )
        (else
            #f
        )
    )
)
;Revisa la diagonal ascendente
(define (diagonalUpward matrix num)
    (diagonal (invertMatrix matrix) num)
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