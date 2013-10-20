#lang racket

(require eopl)
;(provide the-store empty-store get-store initialize-store! reference? newref deref setref! refs->string list-get list-set)
(provide (all-defined-out))
;; ----------------------------------------------------------------------

(define the-store 'uninitialized)

(define empty-store
  (lambda () '()))

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner (lambda (store1 ref1)
              (cond ((null? store1)
                  ;(report-invalid-reference ref the-store))
                  (eopl:error "report-invalid-reference"))
                ((zero? ref1)
                  (cons val (cdr store1)))
                (else
                  (cons (car store1)
                    (setref-inner (cdr store1) (- ref1 1))))))))
        (setref-inner the-store ref)))))

;; list-of references 들을 string으로 변환하는 procedure
(define refs->string
 (lambda (refs)
  (if (null? refs)
   ""
   (if (null? (cdr refs))
    (string-append (number->string (car refs)))
    (string-append (number->string (car refs)) " " (refs->string (cdr refs)))))))

;;list-exp의 요소를 가져오는 exp
(define list-get
  (lambda (lst ref)
    (list-ref lst ref)))

;;list-exp의 요소를 설정하는 exp
;; call-by-value이기 떄문에 변경된 lst를 리턴한다. 그래야 이 함수를 호출한 쪽에서 변경된 값을 사용할 수 있다.
(define list-set
  (lambda (lst ref val)
    (set! lst
      (letrec ((setref-inner (lambda (store1 ref1)
              (cond ((null? store1)
                  ;(report-invalid-reference ref the-store))
                  (eopl:error 'list-set "REF is out of bound"))
                ((zero? ref1)
                  (cons val (cdr store1)))
                (else
                  (cons (car store1)
                    (setref-inner (cdr store1) (- ref1 1))))))))
        (setref-inner lst ref)))
    lst))



;; ----------------------------------------------------------------------
