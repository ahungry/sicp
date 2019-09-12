;; Load at REPL via ,use (oop goops
(use-modules (oop goops))

;; Main conversion types...
;; string->number
;; number->string

(define (foo n) n)

(define (fn i o f)
  `((i . ,i)
    (o . ,o)
    (f . ,f)))

(define add-one (fn 'number 'number (lambda (n) (+ 1 n))))
(define stringer (fn 'number 'string (lambda (n) (number->string n))))

(define mode 'type-check)
(define (type-check) (set! mode 'type-check))
(define (runtime) (set! mode 'runtime))
(define (invoke f x) ((cdr (assoc 'f f)) x))
(define (in-type f) (cdr (assoc 'i f)))
(define (out-type f) (cdr (assoc 'o f)))
(define (assert-types f g)
  (if (equal? (in-type f) (out-type g))
      #t
      (error "Incompatible types composed.")))
(define (_comp f g) (lambda (x) (invoke f (invoke g x))))
(define (comp f g)
  (if (equal? 'type-check mode)
      (begin (assert-types f g) (_comp f g))
      (_comp f g)))

;; Will compose just fine.
(define add-one->string (comp stringer add-one))

;;  Will trigger an error.
(define add-one-to-string (comp add-one stringer))

(in-type add-one)
(out-type add-one)
(invoke add-one 3)

((cdr (assoc 'f add-one)) 3)
