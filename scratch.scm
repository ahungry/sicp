(use-modules (statprof))

;; Always complains nothing recorded, lame
(define (profile)
  (statprof-reset 0 50000 #t)
  (statprof-start)
  (my-expt-tco 10 10)
  (statprof-stop)
  (statprof-display))

(statprof
 (lambda ()
   ;; (my-expt-tco 10 10)
   ;; iota = get a range/series of 1..n
   (map 1+ (iota 100))
   #f))

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
;; This is different than recursive procedure, because the return value
;; is itself the function (the function is in the final position, therefore
;; we do not have to save the function as a return value of another computation.
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

;; 1-2-4 Exponentiation
;; Computes in O(n) steps and O(1) space
(define (my-expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (my-expt-tco b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))

;; 1-3-1
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; 2-1-3 - WOW! Defining a data representation as a procedure, neat!
(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (my-map proc items)
  (if (null? items)
      #nil
      (cons (proc (car items))
            (my-map proc (cdr items)))))

(define (my-accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (my-accumulate op initial (cdr sequence)))))

(define (my-accumulate-cs op initial sequence)
  (define (my-iter acc seq)
    (if (null? seq)
        acc
        (my-iter (op acc (car seq))
                 (cdr seq))))
  (my-iter 0 sequence))

;; Equivalent
(define (add-one n) (+ 1 n))
(define add-one-l (lambda (n) (+ 1 n)))

;; Leaving off at 2.3.3 (Sets)
