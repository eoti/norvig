(define pi 3)

(define alsopi pi)

(define mk3
	(lambda ()
		(3)))

(define mkpi
	(lambda ()
		(pi)))

(define mkpiargs
	(lambda (a b c)
		(pi)))

(define foo
	(lambda (a b) 
		(.plus. a b)))

(define foo2
	(lambda (a b c) 
		(+ (+ a c) (+ b (+ c c)))))

;(define baz
;	(lambda (a)
;		(lambda (b)
;			(+ a b))))

;(define bar
;	(lambda (a)
;		(lambda (b)
;			(lambda (c)
;				(+ a b c)))))


;(define bazlet
;	(lambda (a b)
; 		(let ((c (+ a a)))
;      			(+ b c))))


;(define foolet
;	(lambda (a b)
;  		(let ((c (+ a a)))
;    		(let ((d (+ c c)))
;      			(+ d d)))))


;(define here 0)
;(foo pi pi)

;(define here 1)
;(foo (bar 1 (bar 4 5)) (bar 2 3))

;(define here 2)
;(+ (+ 1 2) (+ 3 4))

;(define here 3)
;(lambda (a b) 
;	(+ a b))

;(define here 4)
;((lambda (a b)(+ a b)) 1 2)

;(define here 5)
;(lambda (a b) 
;	(+ a ((lambda (a b)(+ a b)) 1 2)))

