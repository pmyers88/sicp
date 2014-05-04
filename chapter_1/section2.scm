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

; 1.2.5: Greatest Common Divisors
; GCD algorithm - if r is the remainder when a is divided by b, then the common
; divisors of a and b are precisely the same as the common divisors of b and r.
; GCD(a, b) = GCD(b, r)

; Example: 
; GCD(206, 40) = GCD(40, 6)
;              = GCD(6, 4)
;              = GCD(4, 2)
;              = GCD(2, 0)
;              = 2
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; Exercise 1.20
; How many remainder operations are actually performed in the normal-order
; evaluation of (gcd 206 40)? In the applicative-order evaluation?
; Normal-order: expand then reduce
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; ... performs many more operations
; applicative-order: evaluate then apply
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; ... performs 4 remainder operations

; Exercise 1.21
; Use the smallest-divisor procedure to find the smallest divisor of each of the
; following numbers: 199, 1999, 19999.
; 199 - 199, 1999 - 1999, 19999 - 7

; Exercise 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

; Using this procedure, write a procedure search-for-primes that checks the
; primality of consecutive odd integers in a specified range. Use your procedure
; to find the three smallest primes larger than 1000larger than 10,000larger
; than 100,000larger than 1,000,000.

(define (search-for-primes min max)
  (define (inner-search cur)
    (if (<= cur max) (timed-prime-test cur))
    (if (<= cur max) (inner-search (+ cur 2))))
  (if (even? min)
    (inner-search (+ min 1))
    (inner-search min)))

; Exercise 1.23
(define (smallest-divisor n)
  (define (next next-num)
    (if (= next-num 2)
      3
      (+ next-num 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder b a) 0))

; Exercise 1.24

; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the
; Fermat method), and test each of the 12 primes you found in that exercise.
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

; Exercise 1.26
; Using 2 calls to expmod in the even? branch generates a tree recursion 
; instead of linear recursion, so it is slower.