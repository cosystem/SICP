#lang scheme
;(require racket/include)
(include "../packages/arthmetics.scm")

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of ploy
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)(car p))
  (define (term-list p)(cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable v2) (eq? v1 v2)))

  ;; ------ exercise 2.82 =zero? old version ------ 
  ;; (define (all-zero? cs)
  ;;  (cond ((null? cs) #t) ;should be #t, if we consider empty term list to be zero
  ;;        ;((not (pair? cs)) (=zero? cs))
  ;;        ((not (=zero? (car cs))) #f)
  ;;        (else (all-zero? (cdr cs)))))
  ;; (define (=zero_p? p)
  ;;  (all-zero? (map (lambda (x) (coeff x)) (term-list p))))
  ;; ----------------------------------------------

  ;; ------ exercise 2.82 =zero? new version 01 ------
  ;;(define (=zero_c? terms)
  ;;  (cond ((empty-termlist? terms) #t)
  ;;        ((not (=zero? (coeff (first-term terms)))) #f)
  ;;        (else (=zero_c? (rest-terms terms)))))
  ;;(define (=zero_p? p)
  ;;  (=zero_c? (term-list p)))


  ;; ------ exercise 2.82 =zero? new version 02 ------
  ;;(define (=zero_c? terms)
  ;;  (if (empty-termlist? terms)
  ;;      #t
  ;;      (and (=zero? (coeff (first-term terms))) (=zero_c? (rest-terms terms)))))
  ;;(define (=zero_p? p)
  ;;  (=zero_c? (term-list p)))
  
  ;; ------ exercise 2.82 =zero? new version 03 ------
  (define (=zero_c? terms)
    (or (empty-termlist? terms)
        (and (=zero? (coeff (first-term terms))) (=zero_c? (rest-terms terms)))))
  (define (=zero_p? p)
    (=zero_c? (term-list p)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1)
                                            (term-list p2)))
        (error "Ploy not in same var -- ADD-PLOY"
               (list p1 p2))))
  ;(define (mul-ploy p1 p2)
  ;  (if (same-variable? (variable p1) (variable p2))
  ;      (make-poly (variable p1) (mul-terms (term-list p1)
  ;                                          (term-list p2)))
  ;      (error "Ploy not in same var -- MUL-PLOY"
  ;             (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1)(order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  ;;interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  ;(put 'mul '(polynomial polynomial)
  ;     (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero_p? p)))
  (put 'term-list '(polynomial)
       (lambda (p) (term-list p)))
  "polynomial package installed")
(define (=zero? x) (apply-generic '=zero? x))
(install-polynomial-package)
(define (make-polynomial v t) ((get 'make 'polynomial) v t))

(define (term-list p) (apply-generic 'term-list p))
(define r0 (make-rational 0 7))
(define c0 (make-complex-from-real-imag 0 0))
(define c1 (make-complex-from-mag-ang 0 0))
(define p0 (make-polynomial 'x (list (list 3 0) (list r0 0) (list 1 0))))
(define p1 (make-polynomial 'x (list (list 4 c0) (list 2 0) (list 0 0))))
(define p_test (make-polynomial 'y (list (list 9 p0) (list 4 p1) (list 3 c1))))
               

  


; =zero? should be installed in each package for different type of numbers
; so =zero? in all-zero? is a generic method
