; 1.1 The Elements of Programming
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