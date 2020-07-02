#lang racket/gui


;(define M 5)
;(define N 3)

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

 ; Ventana principal
( define ventana ( new frame% [ label " Nuevo Juego "]) )

( define datos ( new message%
                      [ label " Escoja el tamaño de la matriz "]
                      [ parent ventana ]
                      [ auto-resize #t ]
                      ) )

; define ventana q pide al usuario las dimensiones
( define lanzar
    ( new button%
          [ parent ventana ]
          [ label " escoger "]
          [ callback
            ( lambda ( b c )
               ( send ventana-de-diálogo show #t )

              
               (define M (string->number (send txt-C get-value)))
               (define N (string->number (send txt-F get-value) ))
               

               (cond ((or (> (string->number (send txt-C get-value)) 3) (<  (string->number (send txt-C get-value)) 10) (>  (string->number (send txt-F get-value)) 3) (<  (string->number (send txt-F get-value)) 10)
                      ( send ventana show #t )))) 

               (and (>= (string->number (send txt-C get-value)) 3) (<=  (string->number (send txt-C get-value)) 10) (>=  (string->number (send txt-F get-value)) 3) (<=  (string->number (send txt-F get-value)) 10)
                       (send frame show #t) (send ventana show #f))
              
               (sleep/yield 1)
               (draw-vertical-lines dc (/ dimension_y (string->number (send txt-C get-value))));dibuja lineas verticales
               (draw-horizontal-lines dc (/ dimension_x (string->number (send txt-F get-value) )));dibuja lineas horizontales
               (create_matrix (string->number (send txt-C get-value)) (string->number (send txt-F get-value)) '());crea la matriz
               
            
               ) ]         
          ) )

 ; La otra ventana , de diálogo
( define ventana-de-diálogo ( new dialog% [ label " Escoja las dimensiones del tablero "]) )

( define txt-C ( new text-field%
                            [ label " columnas :"]
                            [ parent ventana-de-diálogo ]
                            ) )
( define txt-F ( new text-field%
                              [ label " filas :"]
                              [ parent ventana-de-diálogo ]
                              ) )
( new button%
      [ parent ventana-de-diálogo ]
      [ label " Aceptar "]
      [ callback ( lambda ( b c ) ( send ventana-de-diálogo show #f ) ) ]
      )
( send ventana show #t )
    
(define M (string->number (send txt-C get-value)))
(define N (string->number (send txt-F get-value) ))



;define el click

(define click-canvas%
  (class canvas%
      (init-field [character #\Space]
                  [color (make-object color% "red")])
    
     (define/override (on-event e)
      (when (equal? (send e get-event-type) 'left-down)
        (draw_X (send e get-x) (send e get-y) )
        (bloquear-matrix matrix 1)
        (bloquear-matrix matrix 2)
        ))
      
        (super-new)))

; define un canvas sobre el frame

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
           (draw-vertical-lines dc (+ position (/ dimension_x (string->number (send txt-F get-value)))) )

      )))
  
;lineas horizontales
(define (draw-horizontal-lines dc position)
(cond((< dimension_y position) #f)
     
     (else (send dc draw-line 0 position dimension_y position); primero las coordenadas iniciales, luego las finaless
           (draw-horizontal-lines dc (+ position (/ dimension_y (string->number (send txt-C get-value)))) )

      )))

;dibuja X's
(define (draw_X position_x position_y)
  (cond (      (= (list-ref (list-ref matrix (quotient position_y (quotient dimension_y  (string->number (send txt-C get-value) )))) (quotient position_x (quotient dimension_x  (string->number (send txt-F get-value) ))) )0 )

               
  (draw_X_aux (+ (/ dimension_x (* 2 (string->number (send txt-F get-value))))(* (quotient position_x (quotient dimension_x (string->number (send txt-F get-value))))(/ dimension_x (string->number (send txt-F get-value))))) (+ (/ dimension_y (* 2 (string->number (send txt-C get-value))))(* (quotient position_y (quotient dimension_y (string->number (send txt-C get-value))))(/ dimension_y (string->number (send txt-C get-value)))))  )

  (set! matrix (change-at matrix (quotient position_y (quotient dimension_y (string->number (send txt-C get-value)))) (quotient position_x (quotient dimension_x (string->number (send txt-F get-value)))) 1  ))

  (draw_O (+ (/ dimension_x (* 2 (string->number (send txt-F get-value))))(* (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)1) (/ dimension_x (string->number (send txt-F get-value))))) (+ (/ dimension_y (* 2 (string->number (send txt-C get-value))))(* (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)0) (/ dimension_y (string->number (send txt-C get-value)))))  )

  (set! matrix (caadr (greedyAlgorithm matrix)))

  (displayln matrix)
  )
        (else #f)

        ))
  

(define (draw_X_aux x_center y_center)
(send dc draw-line (- x_center 25) (- y_center 25) (+ x_center 25) (+ y_center 25))
(send dc draw-line (+ x_center 25) (- y_center 25) (- x_center 25) (+ y_center 25)))


;O's
(define (draw_O position_x position_y)
  (draw_O_aux (+ (/ dimension_x (* 2 (string->number (send txt-F get-value))))(* (quotient position_x (quotient dimension_x (string->number (send txt-F get-value))))(/ dimension_x (string->number (send txt-F get-value))))) (+ (/ dimension_y (* 2 (string->number (send txt-C get-value))))(* (quotient position_y (quotient dimension_y (string->number (send txt-C get-value))))(/ dimension_y (string->number (send txt-C get-value)))))  )
  )

(define (draw_O_aux x_center y_center)
  (displayln x_center)
  (displayln y_center)
  (displayln (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)0))
  (displayln (list-ref(list-ref (list-ref (greedyAlgorithm matrix)0)0)1))
  
  (send dc draw-ellipse (- x_center 25) (- y_center 25) 50 50) 
)

;bloquea la ventana y crea ventana emergente
(define(bloquear-matrix matrix num)
  (cond ((equal? (win matrix num) #f)
          num)
        (else
          (equal? 1 (message-box/custom "The End"  (get-end-game-message num) #f #f #f))
          (send frame show #t)
          (sleep/yield 1)
          (draw-vertical-lines dc (/ dimension_x (/ dimension_x (send txt-C get-value))))
          (draw-horizontal-lines dc (/ dimension_y (/ dimension_x (send txt-C get-value)))))))

(define (get-end-game-message num)
    (cond
      (print "   Exelente juego  ")
      ;(equal? 2 2) (print "Perdiste intenta de nuevo")
      ;(equal? num 1) (print "Ganaste, no lo puedo creer")
     
  ))

(define menu-bar
  (new menu-bar%
       [parent frame]))
 
(define file-menu
  (new menu%
       [label "Juego"]
       [parent menu-bar]))
 
(define new-menu
  (new menu-item%
       [label "nuevo juego"]
       [parent file-menu]
       [callback (lambda (i e) (print "comienza"))]
       [shortcut #\n]))
  
(define quit-menu
  (new menu-item%
       [label "Quitar"]
       [parent file-menu]
       [callback (lambda (i e) (send frame show #f))]
       [shortcut #\q]))

;(send frame show #t)
; (sleep/yield 1)
;(draw-vertical-lines dc (/ dimension_y N));dibuja lineas verticales
;(draw-horizontal-lines dc (/ dimension_x M));dibuja lineas horizontales
;(create_matrix M N '());crea la matriz
