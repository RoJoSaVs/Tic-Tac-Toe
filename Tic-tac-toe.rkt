#lang racket/gui

(define new-game #t)
(define game-finished #f)
 
(define user-move #\X)
(define computer-move #\O)
(define move-count 0)
 
(define start-move user-move)
(define current-move start-move)
 
(define main-board (make-vector 12 #\space))

(define win-paths '((0 1 2) (0 3 6) (0 4 8) (3 4 5) (1 4 7) (2 4 6) (6 7 8) (2 5 8)))
(define win-move '(0 0 0))

 
;_____________________________________________________________________________________________
;Configuaracion de juego
;_____________________________________________________________________________________________

(define (prepare-new-game)
  ;; inica el ijuego    
    ;; inicializa todos los valores 
    (set! main-board (make-vector 12 #\Space))
    (set! current-move start-move)
    (set! game-finished #f)
    (set! move-count 0)
    (set! new-game #f)
   
    ;; comienza con el tablero en blanco
    (for [(canvas  canvases)]
      (send canvas set-character #\Space)
      (send canvas set-color (make-object color% "gray"))
      (send canvas refresh))
    (send frame refresh))
   
   
;verifica que la ósicion se vacia
(define (empty-position? board pos)
  (char=? (vector-ref board pos) #\Space))

;permite o no el movimineto verificando si esta o no vacia la posicion 
(define (make-move-at pos)
  (if (empty-position? main-board pos)
      (begin
        (send (list-ref canvases pos) set-character current-move)
        (send (list-ref canvases pos) refresh)
        (vector-set! main-board pos current-move)
        (set! move-count (+ 1 move-count))
        (if (has-won? main-board current-move)
            (end-game)
            (toggle-current-move)))
      #f))


; alterna en uso de X y O, comenznado por X
(define (toggle-current-move)
  (set! current-move (other-move current-move)))


;verifica si el movimiento gano o no 
(define (has-won? board move)
  (let-syntax ((has-won-syn
                (syntax-rules ()
                  ((has-won-syn p1 p2 p3)
                   (if (char=? (vector-ref board p1); venir a cambiar
                               (vector-ref board p2)
                               (vector-ref board p3) move)
                       (set! win-move (list p1 p2 p3))
                       #f)))))                      
    (for/or ((pos win-paths))
      (has-won-syn (list-ref pos 0) (list-ref pos 1) (list-ref pos 2)))))

;Bloquea cualquier movimineto despues de que existe un ganador
(define (end-game)
  (for ([i win-move])
    (send (list-ref canvases i) refresh))
  (set! game-finished #t)
  (set! new-game #t))


; verifica si el tablero esta lleno
(define (full-board? board)
  (not (for/or ((v board))
         (char=? v #\Space))))


;_________________________________________________________________________________________________________
;movimientos y cambios entre el jugador y la compuadora
;_________________________________________________________________________________________________________


;Verifica el moviento anterior para saber que simbolo utilizo
(define (other-move move)
  (if (char=? move #\X) #\O #\X))

;permite alternar el movimiento del jugador y la computadora dependiendo de la ficha usada (si se usa X juega el jugador pero si sigue una O continua computadora)
(define (toggle-user-move)
  (set! user-move (other-move user-move)))

;permite alternar el movimiento del jugador y la computadora dependiendo de la ficha usada (si se usa X juega el jugador pero si sigue una O continua computadora)
(define (toggle-computer-move)
  (set! computer-move (other-move computer-move)))

; se definen los moviminetos de la computadora
(define (computer-moves)
  (unless (full-board? main-board)
    (make-move-at (1))));en lugar del 1 debe ir la funcion que selecciona donde la computadora pondra su ficha


;_________________________________________________________________________________________________________
;interfaz grafica
;_________________________________________________________________________________________________________

;interfaz
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
       [callback (lambda (i e) (prepare-new-game))]
       [shortcut #\n]))
 
(define quit-menu
  (new menu-item%
       [label "Quitar"]
       [parent file-menu]
       [callback (lambda (i e) (send frame show #f))]
       [shortcut #\q]))
 
;_______________________________________________________________________________________
;define las dimensiones del tablero 
(define main-pane
  (new vertical-pane%
       [parent frame]
       [vert-margin 5]
       [horiz-margin 5]
       [spacing 5]))

(define upper-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))
 
(define middle-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))
 
(define lower-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))

 (define ras-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))

;____________________________________________________________________________-

; verifica las espacios en la lista para poder colocar X o O
(define canvas-box%
  (class canvas%
    (init-field [character #\Space]
                [position 0]
                [color (make-object color% "red")])

       (inherit get-dc)
  ; define los eventos del click y va verificando si es o no un movimineto ganador
    (define/override (on-event e)
      (when (equal? (send e get-event-type) 'left-down)
        (unless (or (full-board? main-board) game-finished)
          (when new-game
            (prepare-new-game))
          (when (make-move-at position)
            (unless game-finished
              (computer-moves))))
        (let ((dc (get-dc)))
          (send dc clear))
        (send this refresh)))
   ;Dibuja las X o las  O segun corresponda 
    (define/override  (on-paint)
      (let ((dc (get-dc)))        
        (let-values (((x y) (send this get-size)))
          (send dc set-text-foreground color)
          (send dc set-font (make-object font% 50 'default))
          (send dc draw-text (string character) (/ (- x 50) 2) (/ (- y 75) 2)))))
   
    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

; se define cada uno de los espacios en la lista 
 (define canvases
  (let ((canvas-one (new canvas-box%
                         [position 0]
                         [parent upper-pane]))
        (canvas-two (new canvas-box%
                         [position 1]
                         [parent upper-pane]))
        (canvas-three (new canvas-box%
                           [position 2]  
                           [parent upper-pane]))
        (canvas-four (new canvas-box%
                          [position 3]  
                          [parent middle-pane]))
        (canvas-five (new canvas-box%
                          [position 4]  
                          [parent middle-pane]))
        (canvas-six (new canvas-box%
                         [position 5]
                         [parent middle-pane]))
        (canvas-seven (new canvas-box%
                           [position 6]  
                           [parent lower-pane]))
        (canvas-eight (new canvas-box%
                           [position 7]      
                           [parent lower-pane]))
        (canvas-nine (new canvas-box%
                          [position 8]
                          [parent lower-pane]))
        (canvas-ten (new canvas-box%
                          [position 9]
                          [parent ras-pane]))
        (canvas-eleven (new canvas-box%
                          [position 10]
                          [parent ras-pane]))
    (canvas-twelve (new canvas-box%
                          [position 11]
                          [parent ras-pane]))) 
    (list canvas-one canvas-two canvas-three
          canvas-four canvas-five canvas-six
          canvas-seven canvas-eight canvas-nine
          canvas-ten canvas-eleven canvas-twelve)))

; centra la pantalla y coloca los lienzos
(send frame center 'both)
; permite visualisar los lienzos 
(send frame show #t)