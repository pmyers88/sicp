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

; Exercise 1.34
; Suppose we define the procedure
(define (f g)
  (g 2))
; Then we have
(f square)
; 4
(f (lambda (z) (* z (+ z 1))))
; 6
; What happens if we (perversely) ask the interpreter to evaluate the
; combination (f f)? Explain.
; (f f)
; (f (f 2))
; (f (2 2))

; Find roots using the midpoint formula
(define (average x y)
  (/ (+ x y) 2.0))

(define (search f a b)
  (let ((possible-zero (average a b)))
    (cond ((< (abs (f possible-zero)) .001) possible-zero)
      ((< (f possible-zero) 0) (search f possible-zero b))
      (else (search f a possible-zero)))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; Exercise 1.35
; Show that the goldon ration is a fixed point of the transformation 
; x -> 1 + 1/x and use this fact to compute it by the fixed-point procedure.
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 

; Exercise 1.36

; Modify fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in exercise 1.22.
; Then find a solution to x^x = 1000 by finding a fixed point of x
; log(1000)/log(x). (Use Scheme's primitive log procedure, which computes
; natural logarithms.) Compare the number of steps this takes with and without
; average damping. (Note that you cannot start fixed-point with a guess of 1, as
; this would cause division by log(1) = 0.)

; damped
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try (average guess next)))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; took 10 guesses with an initial guess of 2

; non-damped
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; took many more guesses due to oscillations

; Exercise 1.37 
; Write a k-term finite continued fraction function
(define (cont-frac n d k)
  (define (cont-frac-inner i result)
    (newline)
    (display result)
    (let ((current-result (/ (n i) (+ (d i) result))))
      (if (= i 1)
        current-result
      (cont-frac-inner (- i 1) current-result))))
  (cont-frac-inner k 0))
    