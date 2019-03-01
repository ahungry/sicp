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

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

;; 1-2-1
;; A linear recursive process
;; This is a recursive procedure that is describing a linear process (TCO)
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Compute with a linear iterative process
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (lfactorial n)
  (fact-iter 1 1 n))

;; Or we could define it with block structure to hide iter
(define (bfactorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
