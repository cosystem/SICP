#lang sicp
; exercise 2.73 b
; ---- define put and get ----;
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;--------------------- deriv -------------------;
(define (deriv_old exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv_old (addend exp) var)
                   (deriv_old (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv_old (multiplicand exp) var))
           (make-product (deriv_old (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(display "\n---- test deriv old ----\n")
(display (deriv_old '(+ x 3) 'x))
(newline)
(display (deriv_old '(* x y) 'x))
(newline)
(display (deriv_old '(* (* x y) (+ x 3)) 'x))
(display "\n---- test deriv old finish ----")

;; ----- Questions -----
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;a. because we can not tag each number. same-variable involve 1 additional variable  

;b. 
;(define (make-sum a1 a2) (list '+ a1 a2))
;
;(define (make-product m1 m2) (list '* m1 m2))
;
;(define (addend s) (cadr s))
;
;(define (augend s) (caddr s))
;
;(define (multiplier p) (cadr p))
;
;(define (multiplicand p) (caddr p))

(define (install-sum-package)
  ;; internal procedure
  (define (deriv oprand var)
    ;; the previous addend/augend is not valid anymore, as deriv will be applied on operands
    (make-sum (deriv (car oprand) var)
              (deriv (cadr oprand) var))) 
  ;; interface to the rest of the system
  (put 'deriv '(+) deriv))

(define (install-product-package)
  ;; internal procedure
  (define (deriv oprand var)
    (make-sum
     (make-product (car oprand)
                   (deriv (cadr oprand) var))
     (make-product (deriv (car oprand) var)
                   (cadr oprand))))
  ;; interface to the rest of the system
  (put 'deriv '(*) deriv))
  
(display "\n---- test deriv new ----\n")
(install-sum-package)
(install-product-package)
(operator '(* x 3))
(get 'deriv (operator '(* x 3)))
;(display (deriv '(+ x 3) 'x))
;(newline)
;(display (deriv '(* x y) 'x))
;(newline)
;(display (deriv '(* (* x y) (+ x 3)) 'x))
(display "\n---- test deriv new finish ----")
