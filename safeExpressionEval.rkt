#lang racket
(require data/either)

(struct success (value) #:transparent)
(struct failure (message) #:transparent)

;; Helper function to extract the message from a failure
(define (from-failure f)
  (failure-message f))

;; Safe arithmetic operations
(define (safe-real-div x y)
  (if (not (and (number? x) (number? y)))
      (failure "Invalid arguments for division") ; Handle invalid inputs
      (if (= y 0)
          (failure "Division by zero") ; Handle division by zero
          (success (/ x y))))) ; Perform real division

(define (safe-int-div x y)
  (if (not (and (number? x) (number? y)))
      (failure "Invalid arguments for integer division") ; Handle invalid inputs
      (if (= y 0)
          (failure "Division by zero") ; Handle division by zero
          (success (quotient x y))))) ; Perform integer division

(define (safe-add x y)
  (if (and (number? x) (number? y))
      (success (+ x y)) ; Handle addition
      (failure "Invalid arguments for addition"))) ; Handle invalid inputs

(define (safe-sub x y)
  (if (and (number? x) (number? y))
      (success (- x y)) ; Handle subtraction
      (failure "Invalid arguments for subtraction"))) ; Handle invalid inputs

(define (safe-mul x y)
  (if (and (number? x) (number? y)) ; Check if both operands are numbers
      (cond
        [(or (= x 0) (= y 0)) (success 0)] ; Anything multiplied by 0 is 0
        [(= x 1) (success y)] ; Multiplying by 1 returns the other operand
        [(= y 1) (success x)] ; Multiplying by 1 returns the other operand
        [else (success (* x y))]) ; Perform multiplication
      (failure "Invalid arguments for multiplication"))) ; Return failure if operands are invalid

(define (safe-pow base exponent)
  (if (not (and (number? base) (number? exponent))) ; Check if both operands are numbers
      (failure "Invalid arguments for exponentiation") ; Return failure if operands are invalid
      (cond
        [(and (= base 0) (= exponent 0)) (success 0)] ; 0^0 = 0 (as per your requirement)
        [(= base 0) (success 0)] ; 0^n = 0 (for n ≠ 0)
        [(= exponent 0) (success 1)] ; n^0 = 1
        [else (success (expt base exponent))]))) ; Perform exponentiation

;; Expression evaluator
(define (eval-expr expr)
  (match expr
    [(list 'num n) (success n)] ; Handle numeric values
    [(list 'add a b) (eval-binary-op safe-add a b)] ; Handle addition
    [(list 'sub a b) (eval-binary-op safe-sub a b)] ; Handle subtraction
    [(list 'mul a b) (eval-binary-op safe-mul a b)] ; Handle multiplication
    [(list 'div a b) (eval-binary-op safe-real-div a b)] ; Handle real division
    [(list 'num-div a b) (eval-binary-op safe-int-div a b)] ; Handle integer division
    [(list 'pow base exponent) (eval-binary-op safe-pow base exponent)] ; Handle exponentiation
    [_ (failure "Unknown expression")])) ; Handle unknown expressions

;; Binary operation evaluator
(define (eval-binary-op safe-fn a b)
  (let ([a-result (eval-expr a)]
        [b-result (eval-expr b)])
    (if (and (success? a-result) (success? b-result))
        (safe-fn (success-value a-result) (success-value b-result))
        (failure (append (if (success? a-result) '() (list (from-failure a-result)))
                       (if (success? b-result) '() (list (from-failure b-result))))))))

;; Test cases
(eval-expr '(add (mul (num x) (pow (num 2) (num 4)))
           (sub (div (num 10) (num 2))
                (num-div (add (num 5) (num 3))
                         (sub (num 8) (num 6))))))

(eval-expr '(add (num 2) (div (num 3) (num 0))))