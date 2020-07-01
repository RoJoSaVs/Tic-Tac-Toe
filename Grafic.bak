#lang racket/gui


(define M 7)
(define N 5)

(define dimension_x 600)
(define dimension_y 600)


;__________________________________________________________________________________________________________________________________________________________________________________



(define frame
  (new frame%
       [label "Tic Tac Toe"]
       [width dimension_x]
       [height (+ dimension_y 59)])); le suma 59 para considerar el espacio de arriba de la ventana

;define el click

(define click-canvas%
  (class canvas%
      (init-field [character #\Space]
                  [color (make-object color% "red")])
    
     (define/override (on-event e)
      (when (equal? (send e get-event-type) 'left-down)
        (print "getx")
        (print (send e get-x))
       (print "gety")
        
        (print (send e get-y))
        (draw_O (send e get-x) (send e get-y) )
        ))
      
        (super-new)))

; define un canvas sobre el frame

(define canvas(new click-canvas%
               [parent frame]))
           
(define dc (send canvas get-dc))
;dibuja encima del canvas 
;lineas verticales
(define (draw-vertical-lines dc position)
(cond((< dimension_x position) #f)
     
     (else (send dc draw-line position 0 position dimension_x); primero las coordenadas iniciales, luego las finaless
           (draw-vertical-lines dc (+ position (/ dimension_x M)) )

      )))
  
;lineas horizontales
(define (draw-horizontal-lines dc position)
(cond((< dimension_y position) #f)
     
     (else (send dc draw-line 0 position dimension_y position); primero las coordenadas iniciales, luego las finaless
           (draw-horizontal-lines dc (+ position (/ dimension_y N)) )

      )))

;X's
(define (draw_X position_x position_y)
  (draw_X_aux (+ (/ dimension_x (* 2 M))(* (quotient position_x (quotient dimension_x M))(/ dimension_x M))) (+ (/ dimension_y (* 2 N))(* (quotient position_y (quotient dimension_x N))(/ dimension_y N)))  )
  )

(define (draw_X_aux x_center y_center)
(send dc draw-line (- x_center 25) (- y_center 25) (+ x_center 25) (+ y_center 25))
(send dc draw-line (+ x_center 25) (- y_center 25) (- x_center 25) (+ y_center 25)))




;O's
(define (draw_O position_x position_y)
  (draw_O_aux (+ (/ dimension_x (* 2 M))(* (quotient position_x (quotient dimension_x M))(/ dimension_x M))) (+ (/ dimension_y (* 2 N))(* (quotient position_y (quotient dimension_x N))(/ dimension_y N)))  )
  )

(define (draw_O_aux x_center y_center)
  (send dc draw-ellipse (- x_center 25) (- y_center 25) 50 50) 
)


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

(send frame show #t)
 (sleep/yield 1)
(draw-vertical-lines dc (/ dimension_x M));dibuja lineas verticales
(draw-horizontal-lines dc (/ dimension_y N));dibuja lineas horizontales