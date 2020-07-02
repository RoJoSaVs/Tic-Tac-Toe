#lang racket/gui


(define M 5)
(define N 3)

(define cont_veces 0)
(define cont_actual 0)

(define dimension_x 600)
(define dimension_y 600)

(define matrix '())

(require "validations.rkt")
(require "matrix.rkt")
(require "greedyAlgorithm.rkt")



;__________________________________________________________________________________________________________________________________________________________________________________



(define frame
  (new frame%
       [label "Tic Tac Toe"]
       [width dimension_x]
       [height (+ dimension_y 59)])); le suma 59 para considerar el espacio de arriba de la ventana

 ; Se crea la primer venta que se meustra la cual pedira al usuario que ingrese la cantidad de columnas y filas 
( define ventana ( new frame% [ label " Nuevo Juego "]) )

( define datos ( new message%
                      [ label " Escoja el tamaño de la matriz "]
                      [ parent ventana ]
                      [ auto-resize #t ]
                      ) )

; define ventana q pide al usuario las dimensiones, ademas de funcion que limita a que la matriz sea de maxio 10x10 y minimo 3x3
( define lanzar
    ( new button%
          [ parent ventana ]
          [ label " escoger "]
          [ callback
            ( lambda ( b c )
               ( send ventana-de-diálogo show #t )

               

               (set! M (string->number (send txt-F get-value)))
               (set! N (string->number (send txt-C get-value) ))
               (set! cont_veces (* N M))
               (set! cont_actual 0)
               (print M)
               (print N )

               ;Verifica si el N o el M contiene algun valor mayor a 10 o menor a 3
               (cond ((or (> (string->number (send txt-C get-value)) 3) (<  (string->number (send txt-C get-value)) 10) (>  (string->number (send txt-F get-value)) 3) (<  (string->number (send txt-F get-value)) 10)
                      ( send ventana show #t )))) 
               ;Verifica que todos los valores estan entre 3 y 10, incluyendo a ambos
               (and (>= (string->number (send txt-C get-value)) 3) (<=  (string->number (send txt-C get-value)) 10) (>=  (string->number (send txt-F get-value)) 3) (<=  (string->number (send txt-F get-value)) 10)
                       (send frame show #t) (send ventana show #f))
              ; grafica las lineas  
               (sleep/yield 1)
               (draw-vertical-lines dc (/ dimension_y N));dibuja lineas verticales
               (draw-horizontal-lines dc (/ dimension_x M));dibuja lineas horizontales
               (create_matrix M N '());crea la matriz
               
            
               ) ]         
          ) )

 ; crea la ventana en la cual el jugador ingresa el valor de columnas y filas 
( define ventana-de-diálogo ( new dialog% [ label " Escoja las dimensiones del tablero "]) )
;permite ingresar las columnas
( define txt-C ( new text-field%
                            [ label " columnas :"]
                            [ parent ventana-de-diálogo ]
                           ) )
;permite ingresar las filas
( define txt-F ( new text-field%
                              [ label " filas :"]
                              [ parent ventana-de-diálogo ]
                              ) )
( new button%
      [ parent ventana-de-diálogo ]
      [ label " Aceptar "]
      [ callback ( lambda ( b c ) ( send ventana-de-diálogo show #f ) ) ]
      )

    


; Funcion que permite el uso del click izquierdo sobre un canvas
(define click-canvas%
  (class canvas%
      (init-field [character #\Space]
                  [color (make-object color% "red")])
    
     (define/override (on-event e)
      (when (equal? (send e get-event-type) 'left-down)
        ;se encarga de dibujar las "X" o las "O" en la posicion que corresponda
        (draw_X (send e get-x) (send e get-y) )
        ;verifica si exite un ganador en cada jugada
        (bloquear-matrix matrix 1)
        (bloquear-matrix matrix 2)
        ))
      
        (super-new)))

; define un canvas sobre el frame para poder utilizar las funciones

(define canvas(new click-canvas%
               [parent frame]))
           
(define dc (send canvas get-dc))
;dibuja encima del canvas

;crea matriz que almacena valores, ESTA FUNCION SOLO DEBE SER LLAMADA UNA VEZ AL PRINCIPIO DEL JUEGO
(define (create_matrix M N new_matrix)
(cond ((= M 0) (displayln new_matrix)(set! matrix new_matrix))
      (else(create_matrix (- M 1) N (append new_matrix (list (fill_columns N '())))))))

(define (fill_columns N new_list)
(cond ((= N 0) new_list)
      (else (fill_columns (- N 1) (append new_list '(0))))))


;modifica la matriz
(define (change-at l x y to);argumentos: lista posicion_x posicion_y nuevo valor
  (for/list ([row l] [i (length l)])
    (for/list ([e row] [j (length row)])
      (if (and (= x i) (= y j))
          to
          e))))

;lineas verticales
(define (draw-vertical-lines dc position)
(cond((< dimension_x position) #f)
     
     (else (send dc draw-line position 0 position dimension_x); primero las coordenadas iniciales, luego las finaless
           (draw-vertical-lines dc (+ position (/ dimension_x N)) )

      )))
  
;lineas horizontales
(define (draw-horizontal-lines dc position)
(cond((< dimension_y position) #f)
     
     (else (send dc draw-line 0 position dimension_y position); primero las coordenadas iniciales, luego las finaless
           (draw-horizontal-lines dc (+ position (/ dimension_y M)) )

      )))

;dibuja X's
(define (draw_X position_x position_y)
  (set! cont_actual (add1 cont_actual))
  (cond ( (< cont_actual (- cont_veces 0))
          (print cont_actual)
         (print (< cont_actual cont_veces) )
      (cond ((= (list-ref (list-ref matrix (quotient (exact-round position_y) (quotient dimension_y M))) (quotient (exact-round position_x) (quotient dimension_x N)) )0 )

               
               (draw_X_aux (+ (/ dimension_x (* 2 N))(* (quotient (exact-round position_x) (quotient dimension_x N))(/ dimension_x N))) (+ (/ dimension_y (* 2 M))(* (quotient (exact-round position_y) (quotient dimension_y M))(/ dimension_y M)))  )

               (set! matrix (change-at matrix (quotient (exact-round position_y) (quotient dimension_y M)) (quotient (exact-round position_x) (quotient dimension_x N)) 1  ))

               (draw_O (+ (/ dimension_x (* 2 N))(* (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)1) (/ dimension_x N))) (+ (/ dimension_y (* 2 M))(* (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)0) (/ dimension_y M)))  )

               (set! matrix (caadr (greedyAlgorithm matrix)))

               (displayln matrix)

               
               
               
               )
            (else #f)))

        (else
         (draw_X_aux (+ (/ dimension_x (* 2 N))(* (quotient (exact-round position_x) (quotient dimension_x N))(/ dimension_x N))) (+ (/ dimension_y (* 2 M))(* (quotient (exact-round position_y) (quotient dimension_y M))(/ dimension_y M)))  )
         (message-box/custom "The End"  "¡ha sido un empate!" #f #f #f) )))
  

(define (draw_X_aux x_center y_center)
(send dc draw-line (- x_center 25) (- y_center 25) (+ x_center 25) (+ y_center 25))
(send dc draw-line (+ x_center 25) (- y_center 25) (- x_center 25) (+ y_center 25)))


;O's
(define (draw_O position_x position_y)
  (draw_O_aux (+ (/ dimension_x (* 2 N))(* (quotient (exact-round position_x) (quotient dimension_x N))(/ dimension_x N))) (+ (/ dimension_y (* 2 M))(* (quotient (exact-round position_y) (quotient dimension_y M))(/ dimension_y M)))  )
  (set! cont_actual (add1 cont_actual))
  (cond ((< cont_actual cont_veces)
        #f)
        (else
         (message-box/custom "The End"  "¡ha sido un empate!" #f #f #f)))

  )

(define (draw_O_aux x_center y_center)
  (displayln x_center)
  (displayln y_center)
  (displayln (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)0))
  (displayln (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)1))
  
  (send dc draw-ellipse (- x_center 25) (- y_center 25) 50 50) 
)



;bloquea la ventana y crea ventana emergente cuando el jugador gana, pierde o empata
(define(bloquear-matrix matrix num)
  (cond ((equal? (win matrix num) #f)
          num)
        (else
          (equal? 1 (message-box/custom "The End"  (get-end-game-message num) #f #f #f))
          (send frame show #t)
          (sleep/yield 1)
          (draw-vertical-lines dc (/ dimension_x M))
          (draw-horizontal-lines dc (/ dimension_y N)))))
;Crea el mensaje que se imprime sobre la ventana creada anteriormente segun corresponda(gane,derrota o empate)
(define (get-end-game-message num)
    (cond ((= num 2) "Perdiste intenta de nuevo")
     (else (cond ((= num 1) "Ganaste, no lo puedo creer")
           (else "empate"))
            )
     
  ))

;Creacion de GUI de juego 

(define menu-bar
  (new menu-bar%
       [parent frame]))
 
(define file-menu
  (new menu%
       [label "Juego"]
       [parent menu-bar]))
;crea funcion para inicir nuevamente el juego desde la ventana 
(define new-menu
  (new menu-item%
       [label "nuevo juego"]
       [parent file-menu]
       [callback (lambda (i e) ( send ventana show #t ))]
       [shortcut #\n]))
;crea funcion para finalizar nuevamente el juego desde la ventana  
(define quit-menu
  (new menu-item%
       [label "Quitar"]
       [parent file-menu]
       [callback (lambda (i e) (send frame show #f))]
       [shortcut #\q]))

;Incializa el juego en la pimer ventana
( send ventana show #t )