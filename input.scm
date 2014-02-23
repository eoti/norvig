(define pi 3)

(define alsopi pi)

(define mk3
	(lambda ()
		(3)))

(define mka
	(lambda (a)
		(a)))

(define mk6
	(lambda ()
		(+ pi pi)))

(define mk7
	(lambda ()
		(+ 3 4)))

(define mkpi
	(lambda ()
		(pi)))

(define mkpiargs
	(lambda (a b c)
		(pi)))

(define foo
	(lambda (a b) 
		(+ a b)))

(define recfoo
	(lambda (a b) 
		(+ a (recfoo a b))))

(define foo2
	(lambda (a b c) 
		(+ (+ a c) (+ b (+ c c)))))

(define foofun
	(lambda (a b) 
		(+ a ((lambda (c d) (+ c d a)) a b))))


(define foofunpi
	(lambda (a b) 
		(+ a ((lambda () (pi))))))

(define baz
	(lambda (a)
		(lambda (b)
			(+ a b))))

(define bar
	(lambda (a)
		(lambda (b)
			(lambda (c)
				(+ a b c)))))


(define bazlet
	(lambda (a b)
 		(let ((c (+ a a)))
      			(+ b c))))


(define foolet
	(lambda (a b)
  		(let ((c (+ a a)))
    		(let ((d (+ c c)))
      			(+ d d)))))


(define messy
	(lambda (a b)
  		(let ((c (+ a (((lambda () (pi))) ())  )))
    		(let ((d (+ c c)))
      			(+ d d)))))

