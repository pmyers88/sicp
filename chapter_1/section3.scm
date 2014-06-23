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
  (iter a 1))

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
  (define tolerance 0.00001)
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
    (newline)
    (display (n i))
    (newline)
    (display (d i))
    (let ((current-result (/ (n i) (+ (d i) result))))
      (if (= i 1)
        current-result
      (cont-frac-inner (- i 1) current-result))))
  (cont-frac-inner k 0))

; Exercise 1.38
; continued fraction for e - 2; n(i) are all 1; D(i) are successively
; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10 ...
(define (euler-d i)
  (if (= (modulo (+ i 1) 3) 0)
    (* 2 (+ 1 (floor (/ i 3))))
    1))
(cont-frac (lambda (i) 1) euler-d 10)
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* -1.0 (square x))))
             (lambda (i) (- (* i 2) 1))
             k))

; Section 1.3.4 Procedures as returned values
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Newton's Method
; f(x) = x - g(x)/g'(x) when g(x) is differentiable
; g'(x) = (g(x + dx) - g(x))/dx as dx -> 0
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; Exercise 1.40
; Define a procedure cubic that can be used together with the newtons-method 
; procedure in expressions of the form
; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x3 + ax2 + bx + c.
(define (cube x)
  (* x x x))
(define (cubic a b c)
  (lambda (x) 
    (+ (cube x) (* a (square x)) (* b x) c))) 
(newtons-method (cubic 2 3 6) 1.0)
; Value: -2

; Exercise 1.41
; Define a procedure double that takes a procedure of one argument as argument 
; and returns a procedure that applies the original procedure twice. For 
; example, if inc is a procedure that adds 1 to its argument, then (double inc)
; should be a procedure that adds 2. What value is returned by
; (((double (double double)) inc) 5)
(define (inc n) (+ n 1))
(define (double f)
  (lambda (x) (f (f x))))
(((double (double double)) inc) 5)
; 21 - inc is expanded and applied 16 times

; Exercise 1.42  
; Let f and g be two one-argument functions. The composition f after g is 
; defined to be the function x  f(g(x)). Define a procedure compose that  
; implements composition. For example, if inc is a procedure that adds 1 to its
; argument,
((compose square inc) 6)
; 49
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43  
; Write a procedure that takes as inputs a procedure that computes f and a 
; positive integer n and returns the procedure that computes the nth repeated 
; application of f. Your procedure should be able to be used as follows:
((repeated square 2) 5)
625
(define (repeated f n)
  (define (repeated-inner g n)
    (if (= n 1) 
      g
      (repeated-inner (compose g f) (- n 1))))
  (repeated-inner f n))

; Exercise 1.44
; The idea of smoothing a function is an important concept in signal processing.
; If f is a function and dx is some small number, then the smoothed version of f
; is the function whose value at a point x is the average of f(x - dx), f(x), 
; and f(x + dx). Write a procedure smooth that takes as input a procedure that 
; computes f and returns a procedure that computes the smoothed f. It is 
; sometimes valuable to repeatedly smooth a function (that is, smooth the 
; smoothed function, and so on) to obtained the n-fold smoothed function. Show 
; how to generate the n-fold smoothed function of any given function using 
; smooth and repeated from exercise 1.43.
(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Exercise 1.45 
; Do some experiments to determine how many average damps are required to 
; compute nth roots as a fixed-point search based upon repeated average damping
; of y  x/yn-1. Use this to implement a simple procedure for computing nth 
; roots using fixed-point, average-damp, and the repeated procedure of exercise
; 1.43. Assume that any arithmetic operations you need are available as 
; primitives.
(define (exp b n)
  (define (succ-sqr a b n)
    (cond ((= n 0) a)
          ((even? n) (succ-sqr a (square b) (/ n 2)))
          (else (succ-sqr (* a b) b (- n 1)))))
  (succ-sqr 1 b n))
(define (log2 x) (/ (log x) (log 2)))
(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y) (/ x (exp y (- n 1)))))
                1.0))

; Exercise 1.46
; Write a procedure iterative-improve that takes two procedures as arguments: a
; method for telling whether a guess is good enough and a method for improving a
; guess. Iterative-improve should return as its value a procedure that takes a
; guess as argument and keeps improving the guess until it is good enough. 
; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of 
; section 1.3.3 in terms of iterative-improve.
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (inner-improve x)
      (let ((improved-x (improve x)))
        (if (good-enough? x improved-x)
          improved-x
          (inner-improve improved-x))))
    (inner-improve x)))
(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))
(define (sqrt x)
  ((iterative-improve close-enough? 
                      (lambda (y) (/ (+ (/ x y) y) 2))) 1.0))
(define (fixed-point f guess)
  ((iterative-improve close-enough? f) guess))
