#lang sicp

;; ---- helper functions ----;
(define (square x)
  (* x x))


;; ---- define put/get-coercion ----;
(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2) coercion-list 
      (set! coercion-list
            (cons (list type1 type2 item)
                  coercion-list))))
(define (get-coercion type1 type2) 
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list) #f
        (let ((top (car list)))
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top))) (get-item top)
                   (get-coercion-iter (cdr list) type1 type2)))))
(get-coercion-iter coercion-list type1 type2))

;; ---- define put and get ----;
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

; ---- define attach tag ----;
(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        ((error "Bad tagged datum -- CONTENTS" datum))))

;(define (magnitude-1 z) (apply-generic 'magnitude-1 z))
;(define (real-part-1 z) (apply-generic 'real-part-1 z))
;(define (imag-part-1 z) (apply-generic 'imag-part-1 z))
;(define (angle-1 z) (apply-generic 'angle-1 z))
;(define (equ? z1 z2) (apply-generic 'equ? z1 z2))

(define (magnitude-1 z) (apply-generic-two 'magnitude-1 z))
(define (real-part-1 z) (apply-generic-two 'real-part-1 z))
(define (imag-part-1 z) (apply-generic-two 'imag-part-1 z))
(define (angle-1 z) (apply-generic-two 'angle-1 z))
(define (equ? z1 z2) (apply-generic-two 'equ? z1 z2))

;; ---- scheme-number package 
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  ; add equal for scheme-number to look-up table
  (put 'equ? '(scheme-number scheme-number) =)
  'done)


;; ---- rational number package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rational x y)
    (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rational)
  'done
  )

;; ---- complex number package

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part-1 z1) (real-part-1 z2))
                         (+ (imag-part-1 z1) (imag-part-1 z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part-1 z1) (real-part-1 z2))
                         (- (imag-part-1 z1) (imag-part-1 z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude-1 z1) (magnitude-1 z2))
                       (+ (angle-1 z1) (angle-1 z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude-1 z1) (magnitude-1 z2))
                       (- (angle-1 z1) (angle-1 z2))))
  (define (equ-complex z1 z2)
    (and (equ? (real-part-1 z1) (real-part-1 z2)) (equ? (imag-part-1 z1) (imag-part-1 z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part-1 '(complex) real-part-1)
  (put 'magnitude-1 '(complex) magnitude-1)
  (put 'equ? '(complex complex) equ-complex) 
  'done)

;; ---- rectangular number package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part-1 z) (car z))
  (define (imag-part-1 z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude-1 z)
    (sqrt (+ (square (real-part-1 z))
             (square (imag-part-1 z)))))
  (define (angle-1 z)
    (atan (imag-part-1 z) (real-part-1 z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part-1 '(rectangular) real-part-1)
  (put 'imag-part-1 '(rectangular) imag-part-1)
  (put 'magnitude-1 '(rectangular) magnitude-1)
  (put 'angle-1 '(rectangular) angle-1)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; ---- polor number package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude-1 z) (car z))
  (define (angle-1 z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part-1 z)
    (* (magnitude-1 z) (cos (angle-1 z))))
  (define (imag-part-1 z)
    (* (magnitude-1 z) (sin (angle-1 z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part-1 '(polar) real-part-1)
  (put 'imag-part-1 '(polar) imag-part-1)
  (put 'magnitude-1 '(polar) magnitude-1)
  (put 'angle-1 '(polar) angle-1)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; apply-generic without coersion;
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;           "No method for these types -- APPLY-GENERIC"
;           (list op type-tags))))))

(define (apply-generic-two op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic-two op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic-two op (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

; test coercion with two types
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(display coercion-list)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)
(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)

; transforming scheme number to complex number
(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define test_scheme_n (make-scheme-number 7))
(display test_scheme_n)
(newline)
(display ((get-coercion 'scheme-number 'complex) test_scheme_n))

; add scheme number and complex numbers
(define z1 (make-complex-from-real-imag 3 4))
(define (add x y) (apply-generic-two 'add x y))
(newline)
(display (add test_scheme_n z1))

;; Q 2.82 Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)

(define (apply-generic-multi op . args)
  (display op))

(newline)
(display (get-coercion 'scheme-number 'complex))
(newline)
(display (not (get-coercion 'scheme-number 'complex)))
(newline)
(display (get-coercion 'scheme-number 'newcomplex))
(newline)
(define proclist (map (lambda (x) (get-coercion x 'complex)) (list 'scheme-number 'new 'somenumber)))

(define (coericiable l)
  (cond ((null? l) nil)
        ((null? (cdr l))
         (if (not (car l)) #f #t))
        ((not (car l)) #f)
        (else (coericiable (cdr l)))))

(define (get-coercion-from-typelist types target)
  (define (coercion type_a type_b)
    (if (eq? type_a type_b)
        (lambda (arg) arg)
        (get-coercion type_a type_b)))

  (map (lambda (intype) (coercion intype target)) types))

  

(display (coericiable proclist))
(newline)
(display (get-coercion-from-typelist (list 'scheme-number 'new 'somenumber) 'complex))
(newline)
(display (get-coercion-from-typelist (list ) 'complex))
(newline)
(display (coericiable (get-coercion-from-typelist (list ) 'complex)))

(define (change-args coercion_fs args)
  (cond ((null? args) nil)
        (else
         (cons ((car coercion_fs) (car args))
              (change-args (cdr coercion_fs) (cdr args))))))



(define (apply-generic-multi op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))



;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (let ((t1->t2 (get-coercion type1 type2))
;                      (t2->t1 (get-coercion type2 type1)))
;                  (cond (t1->t2
;                         (apply-generic-multi op (t1->t2 a1) a2))
;                        (t2->t1
;                         (apply-generic-multi op (t2->t1 a2)))
;                        (else
;                         (error "No method for these types" (list op type-tags))))))
;              (error "No method for these types" (list op type-tags)))))))
