#lang racket
;G36
;220201014-220201023
(require racket/mpair)

(define v
  (list (list 1 0 0 0 0)
        (list 0 0 0 0 0)
        (list 0 0 0 0 0)
        (list 0 0 0 0 0)
        (list 0 0 0 0 0)))

(define show_table
  (lambda (table)
    (for-each (lambda (row) 
                (writeln row))
              table)))   

(define parse_input
  (lambda (input)
    (let ([a (regexp-split "," input)])
      (list (-(string->number (car a)) 1) (- (string->number (cadr a)) 1)))))
  
(define endGame?
  (lambda (v counter bool)
    (cond ((= counter (length v)) bool)
          ((equal? (list-ref v counter) (make-list (length (list-ref v 1)) 1)) (endGame? v (+ 1 counter) #t))
          (else #f))))
                       
(define update
  (lambda (list x y temp)
    (cond ((= x (length list)) list)
          ((= temp (length (list-ref list 1)))(update list (+ x 1) y y))
          (else (update (list-set list x (list-set (list-ref list x) temp 1)) x  y (+ temp 1)  ))))) 

(define valid_input
  (lambda (list x y)
    (cond ((>= x (length list)) #f)
          ((>= y (length (list-ref list 1))) #f)
          ((= 1 (list-ref (list-ref list x) y)) #f)
          ((and (= 0 x) ( = 0 y))#f) 
          (else #t))))


(define computerPlay
  (lambda (list playerMoves) 
    (cond ((= (length list) (length (list-ref list 0)))
           (cond ((member playerMoves '(1 1)) (if (equal? 1 (list-ref (list-ref list (cadar playerMoves)) (caar playerMoves)))
                                                  (update list (- (caar playerMoves) 1) (- (cadar playerMoves) 1) (- (cadar playerMoves) 1))
                                                  (update list (cadar playerMoves) (caar playerMoves) (caar playerMoves)))
                                              (and (and (not(equal? 0 (caar playerMoves))) (not(equal? 0 (cadar playerMoves)))) (not (equal?  (car playerMoves) '(1 1))))  
                                              (update list 1 1 1))
                 (else (update list (cadar playerMoves) (caar playerMoves) (caar playerMoves)))))
          (else (cond ((firstColRow list 1 1) (update list 1 1 1))
                       (else (update list (cadar playerMoves) (caar playerMoves) (caar playerMoves))))))))
                                             
                                             
                                                                    


(define firstColRow
  (lambda (list countx county)
    (cond ((or (= countx (length list)) (= county (length (list-ref list 0)))) #t)
          (else (if (and (zero? (list-ref (list-ref list countx) 0)) (zero? (list-ref (list-ref list 0) county)))
                    (firstColRow list (+ 1 countx) (+ 1 county))
                    #f)))))

(define game 
  (lambda (table playerMoves turn)
    (cond ((and (endGame? table 0 #t) (= turn 1)) (writeln "You WIN!"))
          ((and (endGame? table 0 #t) (= turn 0)) (show_table table)
                                                   (writeln "You LOST!"))                                           
          ((= 0 turn) (show_table table)
                       (displayln "-------------")
                       (displayln "Your input (ex. 2,1) : ")
                       (let ([input (read-line)])
                         (let* ([x (car (parse_input input))]
                                [y (cadr (parse_input input))])
                           (cond ((valid_input table x y)(game (update table x y y) (cons (list x y) playerMoves ) 1))
                                 (else (displayln "Invalid input.Please give a valid coordinate")
                                       (displayln "-------------")
                                       (game table playerMoves 0))))))
          ((= 1 turn) (show_table table)
                       (displayln "Computer made choice")
                       (game (computerPlay table playerMoves) playerMoves 0)))))   

(game v '() 0)











                       