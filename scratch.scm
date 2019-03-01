;; SICP: 1-1-7

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(average 10 5)

(define (improve guess x)
  (average guess (/ x guess)))

(improve 10 20)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
