(define pi 3)

(define alsopi pi)

(define mk3
	(lambda ()
		(3)))

(define mk6
	(lambda ()
		(.PLUS pi pi)))

(define mk7
	(lambda ()
		(.PLUS 3 4)))

(define mkpi
	(lambda ()
		(pi)))

(define mkpiargs
	(lambda (a b c)
		(pi)))

(define foo
	(lambda (a b) 
		(.PLUS a b)))

(define foo2
	(lambda (a b c) 
		(.PLUS (.PLUS a c) (.PLUS b (.PLUS c c)))))

(define baz
	(lambda (a)
		(lambda (b)
			(.PLUS a b))))

(define bar
	(lambda (a)
		(lambda (b)
			(lambda (c)
				(.PLUS a b c)))))


(define bazlet
	(lambda (a b)
 		(let ((c (.PLUS a a)))
      			(.PLUS b c))))


(define foolet
	(lambda (a b)
  		(let ((c (.PLUS a a)))
    		(let ((d (.PLUS c c)))
      			(.PLUS d d)))))


(define here 0)
(foo pi pi)

(define here 1)
(foo (bar 1 (bar 4 5)) (bar 2 3))

(define here 2)
(.PLUS (.PLUS 1 2) (.PLUS 3 4))

(define here 3)
(lambda (a b) 
	(.PLUS a b))

(define here 4)
((lambda (a b)(.PLUS a b)) 1 2)

(define here 5)
(lambda (a b) 
	(.PLUS a ((lambda (a b)(.PLUS a b)) 1 2)))

(define here 6)
((foo2 a b) c)
