; 1.3 Formulating Abstractions with Higher-Order Procedures

; Exercise 1.29
; Simpson's Rule: h/3[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-1 + 4yn-1 + yn]
; h = (b - a)/n
; yk = f(a + kh)
(define (round-to-even x) 
   (+ x (remainder x 2))) 
(define (simp-integral f a b n)
  (define even-n (round-to-even n))
  (define h (/ (- b a) even-n))
  (define (y k) (f (+ a (* k h))))
  (define (a-term k)
    (cond ((or (= k 0) (= k even-n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (/ h 3.0) (sum a-term 0 inc even-n)))

; Exercise 1.30
; Fill in the blanks to change sum to an iterative recursive procedure
; linear
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; iterative
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
; write a product function similar to sum
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1)))

; define factorial using product
(define (prod-fact n)
  (product identity 1 inc n))

; Use the Wallis Product to approximate pi
(define (pi-approx n)
  (define (pi-term a)
    (if (even? a)
      (/ (+ a 2) (+ a 1.0))
      (/ (+ a 1) (+ a 2))))
  (* (product pi-term 0 inc n) 2))

; Exercise 1.32
; sum and product are part of a more general notation called accumulate
; write this procedure
; iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; linear
(define (accumulate-linear combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; Exercise 1.33

; Write a filtered-accumulate abstraction takes the same arguments
; as accumulate, together with an additional predicate of one argument that
; specifies the filter.
(define (filtered-accumulate combiner predicate null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
      ((predicate a) (iter (next a) (combiner result (term a))))
      (else (iter (next a) result))))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (relative-prime? i n)
  (= (gcd i n) 1))
(define (prod-relative-primes n)
  (define (predicate i) (relative-prime? i n))
  (filtered-accumulate * predicate 1 identity 1 inc (- n 1)))