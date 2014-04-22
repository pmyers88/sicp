; Exercise 1.1
10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 4
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
;  a-> 3
(define b (+ a 1))
; b -> 4
(+ a b (* a b))
; 19
(= a b)
; false
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a) b a))
; 4
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16

; Exercise 1.2
(/ 
  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
  (* 3 (- 6 2) (- 2 7))
  )

; Exercise 1.3 Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers.
(define (square x) (* x x))
(define (sum-squares x y)
  (+ (square x) (square y)))
(define (sum-square-largest x y z)
  (cond ((and (<= x y) (<= x z)) (sum-squares y z))
        ((and (<= y x) (<= y z)) (sum-squares x z))
        (else (sum-squares x y))))

; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; if b > 0, use the + operator (+ a b)
; else use the - operator (- a (-b))
; result is a + abs(b)

; Exercise 1.5
; Ben Bitdiddle has invented a test to determine whether the interpreter he is 
; faced with is using applicative-order evaluation or normal-order evaluation. 
; He defines the following two procedures:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-
; order evaluation? What behavior will he observe with an interpreter that 
; uses normal-order evaluation? Explain your answer. (Assume that the 
; evaluation rule for the special form if is the same whether the interpreter 
; is using normal or applicative order: The predicate expression is evaluated 
; first, and the result determines whether to evaluate the consequent or the 
; alternative expression.)

; With normal-order he will observe 0 because the if statement in test is
; true, so y is never evaluated.
; With applicative-order he will observe an infinte loop because (p) will be 
; evaluated infinitely.

; Exercise 1.6 
; Alyssa P. Hacker doesn't see why if needs to be provided as a
; special form. ``Why can't I just define it as an ordinary procedure in terms
; of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be
; done, and she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
; 5

(new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; What happens when Alyssa attempts to use this to compute square roots?
; Explain. 
; Same as in 1.5 - new-if uses applicative-order evaluation and never
; terminates.

; Exercise 1.7
; Newton's method with better good-enough?
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
    (* guess .001)))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; rewritten with block structure
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess))
    (* guess .001)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Exercise 1.8
; Cube root procedure
(define (good-enough-cbrt? guess x)
  (< (abs (- (improve-cbrt guess x) guess))
    (* guess .001)))
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cbrt-iter guess x)
  (if (good-enough-cbrt? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x)
                 x)))
(define (cbrt x)
  (cbrt-iter 1.0 x))

; 1.2  Procedures and the Processes They Generate
(define (factorial n)
  (define (fact-iter x accum)
    (if (= x 1)
      accum
      (fact-iter (- x 1) (* x accum))))
  (fact-iter n 1))

; Linear Recursive Process - the stack grows linearly with n
; Linear Iterative Process - the number of steps grows linearly with n 
; (aka tail-recursion)

; Exercise 1.9
; commenting these out since inc and dec aren't in the namespace
; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))
; This is recursive. The recursive call is an argument to inc.

; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))
; This is iterative. The recursive call is a tail call.

;; Exercise 1.10.  
;; The following procedure computes a mathematical function called Ackermann's
;; function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; What are the values of the following expressions?
(A 1 10)
; 1024
(A 2 4)
; 65536
(A 3 3)
; 65536

; Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; Give concise mathematical definitions for the functions computed by the
; procedures f, g, and h for positive integer values of n. For example, (k n)
; computes 5n2.

; (define (f n) (A 0 n)) => 2n
; (define (g n) (A 1 n)) => 2^n
; (define (g n) (A 1 n)) => 2^2^2...n times

; Exercise 1.11.  

; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1)
; + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means
; of a recursive process. Write a procedure that computes f by means of an
; iterative process.

; f(n) = n, n < 3
;      = f(n - 1) + 2f(n - 2) + 3f(n - 3), n => 3
(define (e11-rec n)
  (if (< n 3)
    n
    (+ (e11-rec (- n 1)) (* 2 (e11-rec (- n 2))) (* 3 (e11-rec (- n 3))))
    ))  

(define (e11-iter n)
  (define (e11-inner a b c count)
    (if (= 2 count)
      a
      (e11-inner (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3)
    n
    (e11-inner 2 1 0 n)))

; Exercise 1.12

; Write a procedure that computes elements of Pascal's triangle by means of a
; recursive process.

(define (pascal x y)
  (if (or (= x 0) (= x y)) 
    1
    (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1)))))


(define (pascal x y)
  (cond ((= x 0) 1)
    ((< x 0) 0)
    ((> x y) 0)
    (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))

; Exercise 1.15
; The sine of an angle (specified in radians) can be computed by making use of
; the approximation sin x x if x is sufficiently small, and the trigonometric
; identity sin(r) = 3*sin(r/3) - 4*sin^3(r/3) to reduce the size of the
; argument of sin. (For purposes of this exercise an angle is considered
; ``sufficiently small'' if its magnitude is not greater than 0.1 radians.)
; These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a. How many times is the procedure p applied when (sine 12.15) is evaluated?
;    .1 * 3^x = 12.5
;    3^x = 125
;    x = ceil(logb3(125)) = 5
; b. f(a) = ceil(logb3(a / .1)) -> O(log n) in time and space

; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps, as does fast-expt.

(define (exp b n)
  (define (succ-sqr a b n)
    (cond ((= n 0) a)
          ((even? n) (succ-sqr a (square b) (/ n 2)))
          (else (succ-sqr (* a b) b (- n 1)))))
  (succ-sqr 1 b n))
; (exp 5 7)
; (succ-sqr 1 5 7)
; (succ-sqr 5 5 6)
; (succ-sqr 5 25 3)
; (succ-sqr 125 25 2)
; (succ-sqr 125 625 1)
; 78125

; Exercise 1.17
; Write a multiplication procedure like fast-exp that uses a logarithmic number
; of steps
(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))
(define (negate n) (* n -1))
(define (fast-mult x y)
  (cond ((= y 0) 0)
        ((even? y) (double (fast-mult x (halve y))))
        (else (+ x (fast-mult x (- y 1))))))
; does not handle negative y values

; Exercise 1.18
; Write an iterative multiplication procedure like fast-exp that uses a 
; logarithmic number of steps
(define (fast-mult-iter x y)
  (define (inner a x y)
    (cond ((= y 0) a)
          ((even? y) (inner a (double x) (halve y)))
          (else (inner (+ a x) x (- y 1)))))
  (if (< y 0)
    ())
  (inner 0 x y))



