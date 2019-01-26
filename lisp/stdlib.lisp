(define id (lambda (x) x))
(define compose (lambda (f g) (lambda (x) (f (g x)))))
(define curry (lambda (f x) (lambda (y) (f x y))))
(define flip (lambda (f) (lambda (x y) (f y x))))
