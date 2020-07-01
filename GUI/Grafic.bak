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

; define un canvas sobre el frame
(define canvas (new canvas% [parent frame]))
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