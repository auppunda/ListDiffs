
(define (null-ld? obj)
	(if (null? obj) #f (if (not (pair? obj)) #f (eq? (car obj) (cdr obj)))) 
)


(define (ld? obj) 
	(if (null-ld? obj) #t
            (if (null? obj) #f (if (not (pair? obj)) #f
                                   (if (not (pair? (car obj))) #f
                                       (ld? (cons (cdr (car obj)) (cdr obj)))))
            ))
)


(define (cons-ld obj listdiff)
  (cons (cons obj (car listdiff)) (cdr listdiff))
)

(define (car-ld listdiff)
  (car (car listdiff))

)

(define (cdr-ld listdiff)
  (if (null-ld? listdiff) (cons '() '()) (cons (cdr (car listdiff)) (cdr listdiff)))

)

(define (ld . args)
  (if (null? args) (cons '() '()) (cons args '()))
)

;; uses null-ld to check listdiff, assumes that it is a valid listdiff
(define (length-ld listdiff)
  (if (not (ld? listdiff)) 0 (if (null-ld? listdiff) 0 (+ 1 (length-ld (cons (cdr (car listdiff)) (cdr listdiff))))))
)

(define (get-l lis l)
  (if (= l 0) '() (append (list (car lis)) (get-l (cdr lis) (- l 1))))
)

(define (append-ld . args)
  ;(apply append-helper args)
  (if (null? args) (cons '() '())
      (if (= (length args) 1) (car args)
         (cons (append (apply append-helper args) (car (car (last args)))) (cdr (car (last args))))))
    ;  (apply append-ld
     ;        (cons (append (get-l (car listdiff) (length-ld listdiff)) (car (car args)))
     ;              (cdr (car args))) (cdr args)))
)

(define (last list)
  (if (= (length list) 1)
      list
      (last (cdr list)))
)

(define (append-helper . args)
  (if (null? (cdr args)) '() (apply append (get-l (car (car args)) (length-ld (car args))) (append-helper (cdr args))))
)

(define (get-last l)
  (if (null? l) '() (if (null? (cdr l)) (car l) (get-last (cdr l))))
)

(define (ld-tail listdiff k)
  (if (= k 0) listdiff (ld-tail (cons (cdr (car listdiff)) (cdr listdiff)) (- k 1))) 
)

(define (list->ld list)
  (cons list '())    
)

(define (ld->list listdiff)
  (if (null-ld? listdiff) '() (append (list (car (car listdiff))) (ld->list (cons (cdr (car listdiff)) (cdr listdiff)))))
)

(define (map-ld proc . args)
  ;(map-ldwithl proc 1 (length-ld (car args)) args)
  (if (null? args) (cons '() '()) (cons (apply map-ldwithl proc args) '()))
)

(define (map-ldwithl proc . args)
  (if (null-ld? (car args))
      '()
      (cons (eval proc (get-list args 1))
            (apply map-ldwithl proc (apply-tail args))))
  
)

(define (apply-tail args)
  (if (null? (cdr args)) (cdr-ld (car args)) (list (cdr-ld (car args)) (apply-tail (cdr args))))
 )
         
(define (getAt l listdiff)
  (if (= l 1) (car-ld listdiff) (getAt (- l 1) (cdr-ld listdiff)))
)

(define (get-list args l)
   (if (null? (cdr args)) (list (getAt l (car args))) (append (list (getAt l (car args))) (get-list (cdr args) l)))
)
  
(define (eval proc l)
   (apply proc l)
)


(define (expr2ld expr)
  (if (null? expr) expr
     (if (pair? expr)
         (cons (expr2ld (car expr)) (expr2ld (cdr expr)))
         (switch expr)))
)

(define (switch expr)
  (if (equal? expr 'list) 'ld
      (if (equal? expr 'map) 'map-ld
       (if (equal? expr 'list-tail) 'ld-tail
        (if (equal? expr 'length) 'length-ld
           (if (equal? expr 'cons) 'cons-ld
              (if (equal? expr 'car) 'car-ld
               (if (equal? expr 'cdr) 'cdr-ld
                (if (equal? expr 'append) 'append-ld
                    (if (equal? expr 'null?) 'null-ld
                        (if (equal? expr 'list?) 'ld? expr))))))))))
                  
)