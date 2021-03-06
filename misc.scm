#lang racket

(require eopl)
(require "parse.scm" "class.scm"); "store.scm")
(provide readfile trace-run-enter trace-class trace-run-exit trace-run-spaces)

;; ======================================================================

(define readfile
  (lambda (file)
    (let ((port (open-input-file file)))
      (readfile2 '() port))))

(define readfile2
  (lambda (charlist port)
    (let ((c (read-char port)))
      (if (eof-object? c)
          (list->string charlist)
          (readfile2 (append charlist (list c)) port)))))

;; ======================================================================

(define trace-flag #f)
(define trace-no-of-spaces 0)

(define trace-run-enter
  (lambda (exp env)
    (and trace-flag
	 (set! trace-no-of-spaces (+ trace-no-of-spaces 1))
	 (trace-run-spaces trace-no-of-spaces)
	 (display (eopl:printf "+ exp=~a~n" (exp->string exp)))
	 (trace-run-spaces trace-no-of-spaces)
	 ;(display (eopl:printf "| env=~a, sto=~a~n"
;			  (env->string-top env)
;			  (store->string-top the-store)))
         )))

(define trace-class
  (lambda ()
    (and trace-flag
      (display (eopl:printf "~nclass-env=~n~a~n~n"
          (class-env->string-top the-class-env))))))

(define trace-run-exit
  (lambda (val)
    (and trace-flag
	 (trace-run-spaces trace-no-of-spaces)
	 (display (eopl:printf "- val=~a~n" (expval->string val)))
	 (set! trace-no-of-spaces (- trace-no-of-spaces 1)))))

(define trace-run-spaces
  (lambda (n)
    (if (eqv? n 0)
	(display "")
	(begin (display "|")
	       (trace-run-spaces (- n 1))))))

;; ======================================================================
