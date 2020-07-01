#lang racket/gui
(define frame
  (new frame%
       [label "Tic Tac Toe"]
       [width 600]
       [height 600]))
 
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