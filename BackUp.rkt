#lang racket

;Archivos y librerias necesarios para ejecutar el programa
(require "matrix.rkt")
(require "validations.rkt")
(require "greedyAlgorithmBack.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;Definicion de medidas para la ventana de juego
(define WIDTH 600)
(define HEIGHT 600)
(define MSG_POS (make-posn (- (/ WIDTH 2) 300) (/ HEIGHT 2)))
(define CANVAS (empty-scene WIDTH HEIGHT))

;Definicion de variables
(define ROW 1);Cantidad de filas
(define COLUMN 1);Cantidad de columnas
(define TURN 0);Numero de turno
(define RT_STOP (list 0));Ayuda a determinar cuando se acaba el juego

;Calcula el tamaño que debe tener cada una de las casillas del tablero
(define square-size-x (/ WIDTH COLUMN))
(define square-size-y (/ HEIGHT ROW))

;Calcula cual es la posicion que elije el jugador y obtiene la posicion en la matriz
(define (getPosX x m) (quotient x (floor (/ WIDTH m))))
(define (getPosY y n) (quotient y (floor (/ HEIGHT n))))

;Crea un cuadro donde estaran contenidas las figuras "O" y "X" con el tamaño que entre en los parametros
(define (box width height)
  (rectangle width height "outline" "gray")
)

;Crea un cuadro donde estaran contenida la figura "O" con el tamaño que entre en los parametros
(define (draw_O width height)
  (overlay (rectangle width height "outline" "gray") (circle 28 "outline" "blue"))
)

;Crea un cuadro donde estaran contenida la figura "X" con el tamaño que entre en los parametros
(define (draw_X width height)
  (overlay (rectangle width height "outline" "gray") (overlay (line -35 -45 "green") (line -35 45 "green")))
)

#|Muestra un mensaje dependiendo de la cantidad de turnos, si son mayores al tamaño de la matriz es empate, 
sino verifica en que turno se encuentra, turnos pares gana la PC, turnos impares gana el jugador|#
(define (messageWinner player)
  (cond    
    ((equal? player (* ROW COLUMN))
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Empate :|" 40 "red")        
    )
    ((equal? (remainder player 2) 1)
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Ganaste :)" 40 "red")
    )
    (else
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Perdiste :(" 40 "red")
    )
  )
)

;Muestra una lista con figuras dependiendo de la lista que entra por parametro
(define (listImage subList)
  (listImageAux subList '())
)  

#|Si el valor en la lista es un 0 agrega a la lista un cuadro en blanco, si es 
un 1 agrega un "O" y si es un 2 agrega una "X"|#
(define (listImageAux subList result)
  (cond
    ((empty? subList)
     result
    )
    (else
     (cond
       ((zero? (car subList))
        (listImageAux (cdr subList) (append result (list (box square-size-x square-size-y))))
       )
       ((equal? (car subList) 2)
        (listImageAux (cdr subList) (append result (list (draw_O square-size-x square-size-y))))
       )
       ((equal? (car subList) 1)
        (listImageAux (cdr subList) (append result (list (draw_X square-size-x square-size-y))))
       )
       (else
        (listImageAux (cdr subList) result)
       ) 
     )  
    ) 
  )  
)

;Regresa un lista con las posiciones para dibujar los cuadros
(define (imagePos matrix)
  (imagePosAux matrix 0 0 '())
)

;Agrega las posiciones en las que hay elementos en la matriz
(define (imagePosAux matrix x y result)
  (cond
    ((empty? matrix)
     result
    )
    ((empty? (car matrix))
     (imagePosAux (cdr matrix) (+ x square-size-y) 0 result)
    )
    (else
     (imagePosAux (append (list (cdar matrix)) (cdr matrix)) x (+ square-size-x y)
           (append result (list (make-posn y x))))
    ) 
  ) 
)  

;Realiza los dibujos en la interfaz
(define (draw n)
  (cond
    ((zero? (car RT_STOP))     
     (place-images/align (listImage (matrixToList n)) (imagePos n) "left" "top" CANVAS)
    )
    (else     
     (place-images/align (append (listImage (matrixToList n)) (list (messageWinner TURN)))
                         (append (imagePos n) (list MSG_POS)) "left" "top" CANVAS)
    )
  )  
)

;Registra la posicion del puntero y coloca un 1 en la posicion de la matriz que selecciona
(define (playerSelection matrix x y player)
  (cond
    ((mouse=? player "button-down")
     (cond
       ((zero? (getValue matrix (getPosY y ROW) (getPosX x COLUMN)))
        (set! TURN (+ TURN 1))
        (setValueMatrix matrix (getPosY y ROW) (getPosX x COLUMN) 1)
       )
       (else
        matrix
       ) 
     )  
    )
    (else
     matrix
    )
  )  
)

;Establece todos los valores de la matriz de juego otra vez a 0
(define (restart matrix key)
    (cond
        ((key=? key "up")
            (createMatrix ROW COLUMN)
        )
        (else
            matrix
        )
    )  
)

;Verifica si logra ganar el jugador o la PC
(define (alreadyWinner matrix)
    (cond
        ((or (win matrix 1) (win matrix 2) (equal? TURN (* ROW COLUMN)))
            (set! RT_STOP (list 1))
            #t
        )
        (else
            #f
        )
    )
)

;Se encarga de ejecutar el greedyAlgorithm para hacer el movimiento de la PC
(define (movePC matrix)
    (cond
        ((equal? (remainder TURN 2) 1)
            (set! TURN (+ TURN 1))
            (greedyAlgorithm matrix)
        )
        (else
            matrix
        )
    )
)

#|Es la funcion encargada de ejecutar el juego, cambia los valores de las filas y las columnas,
realiza los cambios sobre la matriz de juego y ejecuta las funciones necesarias para que "juege" la PC|#
(define (TTT x y)
    (cond 
        ((or (< x 3) (< y 3) (> x 10) (> y 10))
            (display "No se puede jugar con las dimensiones elegidas")
        )
        (else
            (set! ROW x)
            (set! COLUMN y)
            (set! square-size-x (/ WIDTH COLUMN))
            (set! square-size-y (/ HEIGHT ROW))
            
            (big-bang
                (createMatrix ROW COLUMN)
                (name "TIC-TAC-TOE")      
                (on-mouse playerSelection)      
                (on-key restart)
                (on-tick movePC)
                (to-draw draw)
                (stop-when alreadyWinner draw)      
            )
        )
    )    
)

(TTT 4 4)