#lang eopl

(define trace-flag #f)
(define trace-no-of-spaces 0)

(require "parse.scm" "class.scm" "misc.scm" "store.scm" "format.scm" "racket_provide.rkt" "draw-image.rkt")
;; ======================================================================

(define runfile
  (lambda (file)
    (let ((string (readfile file)))
      ;;빈 파일이면 인터프리터를 실행시키지 않는다.
      (if (string=? string "")
          (void)
          (begin
            ;;그래픽으로 결과를 표시하기 위한 준비(결과는 (run (..)에서 표시한다.)
            (init-canvas)

            ;;텍스트로 결과 표시
            ;(display (format "**CODE**~n~a~n~n" string))
            (display (format "**TYPE**~n~a~n~n" (type->string (check string))))
            (display (format "**RESULT**~n~a~n~n" (expval->string (run (readfile file)))))
            ;(display (format "**STORE**~n~a~n~n" (store->string-top the-store)))
            (newline)
            
            ;;그래픽 결과를 임시 파일에 저장 - 이것을 결과창에서 불러와서 화면을 그린다.
            (save-image-canvas)
            )))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (class-decls body)
        (initialize-class-env! class-decls)
        (trace-class);클래스 정보 출력
        (value-of body (init-env))))))

(define value-of
  (lambda (exp env)
    (trace-run-enter exp env)
    (let ((val (value-of-1 exp env)))
      (trace-run-exit val)
      val)))

(define value-of-1
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
        (num-val num))
      (var-exp (var)
        (deref (apply-env env var)))
      (true-exp ()
        (bool-val #t))
      (false-exp ()
        (bool-val #f))
      (string-exp (str)
        (string-val str))
      (zero?-exp (exp1)
        (let ((num1 (expval->num (value-of exp1 env))))
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env))))
      (assign-exp (var exp1)
        (begin
          (setref!
            (apply-env env var)
            (value-of exp1 env))
          (deref (apply-env env var))))
      (begin-exp (exp1 exps)
        (letrec ((value-of-begins
              (lambda (exp2 es)
                (let ((v1 (value-of exp2 env)))
                  (if (null? es)
                    v1
                    (value-of-begins (car es) (cdr es)))))))
          (value-of-begins exp1 exps)))
      (sum-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (+ num1 num2))))
      (diff-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (- num1 num2))))
      (mul-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (* num1 num2))))
      (div-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (/ num1 num2))))
      (mod-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (remainder num1 num2))))
      
      (sleep-exp (exp1)
                 (let ((time (expval->num (value-of exp1 env))))
                   (if (> time 0)
                       (begin (sleep time) (void-val))
                       (eopl:error 'sleep-exp "TIME must be grater than 0"))))

;; 2013. 3. 28 용규 수정
;; list-exp의 interpreter
;; list-exp은 lst의 각각의 value들을 list로 만들어준다..
;; ex) ((3 -3) (5 -5))
      (list-exp (exps) 
        (list-val (value-of-exps exps env)))
	;; 용규 수정;;
      (let-exp (vars exps body)
        (let ((vlist (value-of-exps exps env)))
          (value-of body (extend-env* vars (value-of-refs vlist) env))))
      (call-exp (rator rands)
        (let ((proc (expval->proc (value-of rator env)))
              (args (value-of-exps rands env)))
          (apply-procedure proc args)))
      (proc-exp (vars var-types body)
        (proc-val (procedure vars var-types body env)))
      (letrec-exp (p-result-type p-names b-varss b-var-typess p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec p-result-type p-names b-varss b-var-typess p-bodies env)))
;; new-object-exp는 new Identifier ({Expression}*)을 호출할 때 처리하는 interpreter
;; expressions들의 expvals를 args에, new-object를 통해 새로 만든 객체를 obj에 저장한 뒤
;; apply-method procedure를 호출한다 (class에서 initialize에 해당하는 method와 해당 클래스 객체, 그리고 args를 인자로 받는다)
;; 그리고 해당 obj-val를 return 한다
      (new-object-exp (class-name rands)
	  (let ((args (value-of-exps rands env))
                (obj (new-object class-name)))
            (apply-method (find-method class-name 'initialize)
              obj args)
            (obj-val obj)))
;; method-call-exp는 send Identifier ({Expression}*)을 호출할 때 처리하는 interpreter
;; expression들의 expvals를 args에, obj-exp의 value인 obj-val을 obj에 저장한 뒤
;; apply-method procedure를 호출한다 (해당 object를 만든 class에서 method-name에 해당하는 method와 obj, 그리고 args를 인자로 받는다)
      (method-call-exp (obj-exp method-name rands)
        (let ((args (value-of-exps rands env))
              (obj (value-of obj-exp env)))
          (apply-method
            (find-method (object->class-name (expval->obj obj)) method-name)
            (expval->obj obj) args)))
;; self-exp는 자기 자신의 obj-val를 return 한다
      (self-exp ()
        (obj-val (apply-env env '%self)))

;; super-call-exp는 super identifier ({Expression}*) 을 호출할 때 처리하는 interpreter
;; expressions들의 expvals을 args에, 그리고 self object를 obj에 저장한뒤
;; apply-method procedure를 호출한다. (super class에서 method-name에 해당하는 method와 self, 그리고 args를 인자로 받는다)
      (super-call-exp (method-name rands)
        (let ((args (value-of-exps rands env))
              (obj (apply-env env '%self)))
          (apply-method
            (find-method-with-interface (apply-env env '%super) method-name)
            obj args)))
      (cast-exp (obj-exp c-name)
        (let ((obj (value-of obj-exp env)))
          (if (is-subclass? (object->class-name (expval->obj obj)) c-name)
              obj
              (eopl:error 'cast-exp "cast error ~a ~a" c-name obj))))
      (instanceof-exp (exp c-name)
        (let ((obj (value-of exp env)))
          (if (is-subclass? (object->class-name (expval->obj obj)) c-name)
            (bool-val #t)
            (bool-val #f))))
      
      ;;그리기 함수
      (line-exp (x y i j color)
         (let ((line_x (expval->num (value-of x env)))
              (line_y (expval->num (value-of y env)))
              (line_i (expval->num (value-of i env)))
              (line_j (expval->num (value-of j env)))
              (line_color (expval->string_ (value-of color env))))
           (n_line line_x line_y line_i line_j (remove-dquote line_color))
           (bool-val #t)))
      
      (circle-exp (r i j outline_solid color)
         (let ((line_r (expval->num (value-of r env)))
              (line_i (expval->num (value-of i env)))
              (line_j (expval->num (value-of j env)))
              (outline_solid_ (expval->string_ (value-of outline_solid env)))
              (line_color (expval->string_ (value-of color env))))
           (n_circle line_r line_i line_j (remove-dquote outline_solid_) (remove-dquote line_color))
           (bool-val #t)))
      
      (triangle-exp (a b c i j outline_solid color)
         (let ((side_a (expval->num (value-of a env)))
              (side_b (expval->num (value-of b env)))
              (side_c (expval->num (value-of c env)))
              (point_i (expval->num (value-of i env)))
              (point_j (expval->num (value-of j env)))
              (outline_solid_ (expval->string_ (value-of outline_solid env)))
              (line_color (expval->string_ (value-of color env))))
           (n_triangle side_a side_b side_c point_i point_j (remove-dquote outline_solid_) (remove-dquote line_color))
           (bool-val #t)))
      
      (rectangle-exp (a b i j outline_solid color)
         (let ((side_a (expval->num (value-of a env)))
              (side_b (expval->num (value-of b env)))
              (point_i (expval->num (value-of i env)))
              (point_j (expval->num (value-of j env)))
              (outline_solid_ (expval->string_ (value-of outline_solid env)))
              (line_color (expval->string_ (value-of color env))))
           (n_rectangle side_a side_b point_i point_j (remove-dquote outline_solid_) (remove-dquote line_color))
           (bool-val #t)))
      
      (polygon-exp (r n i j outline_solid color)
         (let ((size (expval->num (value-of r env)))
              (number (expval->num (value-of n env)))
              (point_i (expval->num (value-of i env)))
              (point_j (expval->num (value-of j env)))
              (outline_solid_ (expval->string_ (value-of outline_solid env)))
              (line_color (expval->string_ (value-of color env))))
           (n_polygon size number point_i point_j (remove-dquote outline_solid_) (remove-dquote line_color))
           (bool-val #t)))
      
      (textout-exp (text i j size color)
         (let ((text_ (expval->string_ (value-of text env)))
              (point_i (expval->num (value-of i env)))
              (point_j (expval->num (value-of j env)))
              (size_ (expval->num (value-of size env)))
              (line_color (expval->string_ (value-of color env))))
           (n_text (remove-dquote text_) point_i point_j size_ (remove-dquote line_color))
           (bool-val #t)))
         
       (else
        (eopl:error 'value-of-1 "arg=~a" exp)))))

;; procedure에서 새로운 env(extend-env)를 만들어서 body expression을 실행시키는 apply-procedure이다.
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars var-types body saved-env)
        (value-of body (extend-env* vars (value-of-refs vals) saved-env)))
      (else
        (eopl:error 'apply-procedure "arg=~a" proc1)))))

; 2013. 5. 19 make : 김용규
; cast-exp와 instanceof-exp에서 subclass인지 확인하는 함수

(define is-subclass?
  (lambda (c-name1 c-name2)
    (if (eqv? c-name1 c-name2)
      #t
      (let ((s-name (class->super-name (lookup-class c-name1))))
        (if (eqv? #f s-name)
          #f
          (is-subclass? s-name c-name2))))))

(define check-equal
  (lambda (val1 val2 exp)
    (if (equal? val1 val2)
        #t
        (eopl:error 'check-equal "~a" exp))))

;; 용규 make ;;
;; let-exp (vars exps body) 사용 함수
;; return (ref1 ref2 ref3 ...)

(define value-of-refs
 (lambda (exps)
  (map (lambda (exp) (newref exp)) exps)))

;; lex-exp (vars exps body) 사용함수
;; return (var1 var2 var3 ...)

(define value-of-exps
 (lambda (exps env)
  (map (lambda (exp) (value-of exp env)) exps)))


;; ======================================================================

;; method에 있는 body를 실행하기 위해 environment를 만들어준다
;; method를 호출한 객체의 field들부터 초기화 해준 후 super class의 환경을 추가한다
;; 그 다음 method에 있는 expression을 새로운 envrionment를 통해 실행한다. 
(define apply-method
  (lambda (m self args)
    (cases method m
      (a-method (r-type vars var-types body s-name i-names f-names f-types)
        (let ((new-env (empty-env)))
          (begin
            (if (not (null? f-names)) ;; method에 field-names들이 null이라면
              (set! new-env (extend-env* f-names (object->fields self) new-env))
              'aaaaaa)
            (set! new-env (extend-env-with-self-and-super self s-name i-names new-env))
            (if (not (null? vars))
              (set! new-env (extend-env* vars (map newref args) new-env))
              'aaaaaaaaaaaaaaaaaaaaaa)
            (value-of body new-env))))
      (i-method (r-type vars var-types) ""))))




(define check
  (lambda (string)
    (type-of-program (scan&parse string))))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls body) 
        (initialize-static-class-env! class-decls)
        (for-each check-class-decl! class-decls)
        (type-of body (init-tenv))))))

(define type-of
  (lambda (exp tenv)
    (trace-check-enter exp tenv)
    (let ((val (type-of-1 exp tenv)))
      (trace-check-exit val)
      val)))

;exp * tenv -> type

(define type-of-1
  (lambda (exp tenv)
    (cases expression exp
      
      (const-exp (num) (int-type))
      
      (var-exp (var) (apply-tenv tenv var))
      
      (true-exp () (bool-type))
      (false-exp () (bool-type))
      
      (string-exp (str) (string-type))
      
      (diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
      
      (sum-exp (exp1 exp2)
               (let ((type1 (type-of exp1 tenv))
                     (type2 (type-of exp2 tenv)))
                 (check-equal-type! type1 (int-type) exp1)
                 (check-equal-type! type2 (int-type) exp2)
                 (int-type)))
      
      (mul-exp (exp1 exp2)
               (let ((type1 (type-of exp1 tenv))
                     (type2 (type-of exp2 tenv)))
                 (check-equal-type! type1 (int-type) exp1)
                 (check-equal-type! type2 (int-type) exp2)
                 (int-type)))
      
      (div-exp (exp1 exp2)
               (let ((type1 (type-of exp1 tenv))
                     (type2 (type-of exp2 tenv)))
                 (check-equal-type! type1 (int-type) exp1)
                 (check-equal-type! type2 (int-type) exp2)
                 (int-type)))
      
      (mod-exp (exp1 exp2)
               (let ((type1 (type-of exp1 tenv))
                     (type2 (type-of exp2 tenv)))
                 (check-equal-type! type1 (int-type) exp1)
                 (check-equal-type! type2 (int-type) exp2)
                 (int-type)))
      
      (sleep-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (void-type)))
      
      (zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
      
      (if-exp (test-exp true-exp false-exp)
              (let
                  ((test-type (type-of test-exp tenv))
                   (true-type (type-of true-exp tenv))
                   (false-type (type-of false-exp tenv)))
                ;; these tests either succeed or raise an error
                (check-equal-type! test-type (bool-type) test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      
      (let-exp (ids rands body) 
               (let ((new-tenv 
                      (extend-tenv 
                       ids
                       (types-of-exps rands tenv)
                       tenv)))
                 (type-of body new-tenv)))
      
      (proc-exp (bvars bvar-types body)
                (let ((result-type
                       (type-of body
                                (extend-tenv bvars bvar-types tenv))))
                  (proc-type bvar-types result-type)))
      
      (call-exp (rator rands) 
                (let ((rator-type (type-of rator tenv))
                      (rand-types  (types-of-exps rands tenv)))
                  (type-of-call rator-type rand-types rands exp)))
      
      (letrec-exp (proc-result-types proc-names
                                     bvarss bvar-typess proc-bodies
                                     letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-names
                          (map proc-type bvar-typess proc-result-types)
                          tenv)))
                    (for-each
                     (lambda (proc-result-type bvar-types bvars proc-body)
                       (let ((proc-body-type
                              (type-of proc-body
                                       (extend-tenv
                                        bvars
                                        bvar-types
                                        tenv-for-letrec-body)))) ;; !!
                         (check-equal-type!
                          proc-body-type proc-result-type proc-body)))
                     proc-result-types bvar-typess bvarss proc-bodies)
                    (type-of letrec-body tenv-for-letrec-body)))
      
      (begin-exp (exp1 exps)
                 (letrec 
                     ((type-of-begins
                       (lambda (e1 es)
                         (let ((v1 (type-of e1 tenv)))
                           (if (null? es)
                               v1
                               (type-of-begins (car es) (cdr es)))))))
                   (type-of-begins exp1 exps)))
      
      (assign-exp (id rhs)
                  (check-is-subtype!
                   (type-of rhs tenv)
                   (apply-tenv tenv id)
                   exp)
                  (void-type))
      
      (list-exp (exps) 
                (let ((type-of-car (type-of (car exps) tenv)))
                  (map
                   (lambda (exp)
                     (check-equal-type!
                      (type-of exp tenv)
                      type-of-car
                      exp))
                   (cdr exps))
                  (list-type type-of-car)))
      
      ;; object stuff begins here
      
      (new-object-exp (class-name rands)
                      (let ((arg-types (types-of-exps rands tenv))
                            (c (lookup-static-class class-name)))
                        (cases static-class c
                          (an-interface (method-tenv)
                                        (report-cant-instantiate-interface class-name))
                          (a-static-class (super-name i-names
                                                      field-names field-types method-tenv)
                                          ;; check the call to initialize
                                          (type-of-call
                                           (find-method-type
                                            class-name
                                            'initialize) 
                                           arg-types
                                           rands
                                           exp)
                                          ;; and return the class name as a type
                                          (class-type class-name)))))
      
      (self-exp ()
                (apply-tenv tenv '%self))
      
      (method-call-exp (obj-exp method-name rands)
                       (let ((arg-types (types-of-exps rands tenv))
                             (obj-type (type-of obj-exp tenv)))
                         (type-of-call
                          (find-method-type
                           (type->class-name obj-type)
                           method-name)
                          arg-types
                          rands
                          exp)))
      
      (super-call-exp (method-name rands)
                      (let ((arg-types (types-of-exps rands tenv))
                            (obj-type (apply-tenv tenv '%self)))
                        (type-of-call
                         (find-method-type
                          (apply-tenv tenv '%super)
                          method-name)
                         arg-types
                         rands
                         exp)))
      
      ;; this matches interp.scm:  interp.scm calls
      ;; object->class-name, which fails on a non-object, so we need
      ;; to make sure that obj-type is in fact a class type.
      ;; interp.scm calls is-subclass?, which never raises an error,
      ;; so we don't need to do anything with class-name here.
      
      (cast-exp (exp class-name)
                (let ((obj-type (type-of exp tenv)))
                  (if (class-type? obj-type)
                      (class-type class-name)
                      (report-bad-type-to-cast obj-type exp))))
      
      ;; instanceof in interp.scm behaves the same way as cast:  it
      ;; calls object->class-name on its argument, so we need to
      ;; check that the argument is some kind of object, but we
      ;; don't need to look at class-name at all.
      
      (instanceof-exp (exp class-name)
                      (let ((obj-type (type-of exp tenv)))
                        (if (class-type? obj-type)
                            (bool-type)
                            (report-bad-type-to-instanceof obj-type exp))))
      
      ;;그리기
      (line-exp (x y i j color)
                (let ((type_x (type-of x tenv))
                      (type_y (type-of y tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_x (int-type) type_x)
                  (check-equal-type! type_y (int-type) type_y)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (circle-exp (r i j outline_solid color)
                (let ((type_r (type-of r tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_line_solid (type-of outline_solid tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_r (int-type) type_r)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_line_solid (string-type) type_line_solid)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (triangle-exp (a b c i j outline_solid color)
                (let ((type_a (type-of a tenv))
                      (type_b (type-of b tenv))
                      (type_c (type-of c tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_line_solid (type-of outline_solid tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_a (int-type) type_a)
                  (check-equal-type! type_b (int-type) type_b)
                  (check-equal-type! type_c (int-type) type_c)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_line_solid (string-type) type_line_solid)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (rectangle-exp (a b i j outline_solid color)
                (let ((type_a (type-of a tenv))
                      (type_b (type-of b tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_line_solid (type-of outline_solid tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_a (int-type) type_a)
                  (check-equal-type! type_b (int-type) type_b)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_line_solid (string-type) type_line_solid)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (polygon-exp (r n i j outline_solid color)
                (let ((type_r (type-of r tenv))
                      (type_n (type-of n tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_line_solid (type-of outline_solid tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_r (int-type) type_r)
                  (check-equal-type! type_n (int-type) type_n)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_line_solid (string-type) type_line_solid)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (textout-exp (text i j size color)
                (let ((type_size (type-of size tenv))
                      (type_i (type-of i tenv))
                      (type_j (type-of j tenv))
                      (type_text (type-of text tenv))
                      (type_color (type-of color tenv)))
                  (check-equal-type! type_size (int-type) type_size)
                  (check-equal-type! type_i (int-type) type_i)
                  (check-equal-type! type_j (int-type) type_j)
                  (check-equal-type! type_text (string-type) type_text)
                  (check-equal-type! type_color (string-type) type_color)
                  (bool-type)))
      
      (else
       (eopl:error 'type-of-1 "~a" exp)))))

(define find-method-type
  (lambda (class-name id)
    (let ((m (maybe-find-method-type 
              (static-class->method-tenv (lookup-static-class class-name))
              id)))
      (if m m
          (eopl:error 'find-method 
                      "Unknown method ~s in class ~s"
                      id class-name)))))

(define report-cant-instantiate-interface
  (lambda (class-name)
    (eopl:error 'type-of-new-obj-exp
                "Can't instantiate interface ~s"
                class-name)))

(define types-of-exps
  (lambda (rands tenv)
    (map (lambda (exp) (type-of exp tenv)) rands)))

;; type-of-call : Type * Listof(Type) * Listof(Exp) -> Type
;; Page: 360
(define type-of-call
  (lambda (rator-type rand-types rands exp)
    ;(display "type-of-call")
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (unless (= (length arg-types) (length rand-types))
                   (report-wrong-number-of-arguments 
                    (map type-to-external-form arg-types) 
                    (map type-to-external-form rand-types) exp))
                 (for-each check-is-subtype! rand-types arg-types rands)
                 result-type)
      (else
       (report-rator-not-of-proc-type
        (type-to-external-form rator-type)
        exp)))))

(define type-to-external-form
  (lambda (ty)
    ;(display "type-to-external-form")
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
         (type-to-external-form arg-type)
         '->
         (type-to-external-form result-type)))
      (else
       (eopl:error 'type-to-external-form "~a" ty)))))

(define report-rator-not-of-proc-type
  (lambda (external-form-rator-type exp)
    (eopl:error 'type-of-call
                "Operator ~s is not of proc-type ~s"
                exp external-form-rator-type)))

(define report-wrong-number-of-arguments
  (lambda (arg-types rand-types exp)
    (eopl:error 'type-of-call
                "These are not the same: ~s and ~s in ~s"
                (map type-to-external-form arg-types)
                (map type-to-external-form rand-types)
                exp)))

;; check-class-decl! : ClassDecl -> Unspecified
;; Page: 367
(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name abs-method-decls)
                         #t)
      (a-class-decl (class-name super-name i-names 
                                field-types field-names method-decls)
                    (let ((sc (lookup-static-class class-name)))
                      (for-each 
                       (lambda (method-decl)
                         (check-method-decl! method-decl
                                             class-name super-name 
                                             (static-class->field-names sc)
                                             (static-class->field-types sc)))
                       method-decls))
                    (for-each 
                     (lambda (i-name)
                       (check-if-implements! class-name i-name))
                     i-names)
                    ))))


;; check-method-decl! :
;;   MethodDecl * ClassName * ClassName * Listof(FieldName) * \Listof(Type) 
;;    -> Unspecified
;; Page: 368
(define check-method-decl!
  (lambda (m-decl self-name s-name f-names f-types)
    (cases method-decl m-decl
      (a-method-decl (res-type m-name vars var-types body)
                     (let ((tenv
                            (extend-tenv
                             vars var-types
                             (extend-tenv-with-self-and-super
                              (class-type self-name)
                              s-name
                              (extend-tenv f-names f-types
                                           (init-tenv))))))
                       (let ((body-type (type-of body tenv)))
                         (check-is-subtype! body-type res-type m-decl)
                         (if (eqv? m-name 'initialize) #t
                             (let ((maybe-super-type
                                    (maybe-find-method-type
                                     (static-class->method-tenv
                                      (lookup-static-class s-name))
                                     m-name)))
                               (if maybe-super-type
                                   (check-is-subtype! 
                                    (proc-type var-types res-type)
                                    maybe-super-type body)
                                   #t)))))))))
(define maybe-find-method-type
  (lambda (m-env id)
    (cond
      ((assq id m-env) => cadr)
      (else #f))))


;; check-if-implements! : ClassName * InterfaceName -> Bool
;; Page: 369
(define check-if-implements!
  (lambda (c-name i-name)
    (cases static-class (lookup-static-class i-name)
      (a-static-class (s-name i-names f-names f-types
                              m-tenv)
                      (report-cant-implement-non-interface 
                       c-name i-name))
      (an-interface (method-tenv)
                    (let ((class-method-tenv
                           (static-class->method-tenv
                            (lookup-static-class c-name))))
                      (for-each
                       (lambda (method-binding)
                         (let ((m-name (car method-binding))
                               (m-type (cadr method-binding)))
                           (let ((c-method-type
                                  (maybe-find-method-type
                                   class-method-tenv
                                   m-name)))
                             (if c-method-type
                                 (check-is-subtype!
                                  c-method-type m-type c-name)
                                 (report-missing-method
                                  c-name i-name m-name)))))
                       method-tenv))))))

(define report-cant-implement-non-interface
  (lambda (c-name i-name)
    (eopl:error 'check-if-implements
                "Class ~s claims to implement non-interface ~s"
                c-name i-name)))

(define report-missing-method
  (lambda (c-name i-name i-m-name)
    (eopl:error 'check-if-implements
                "Class ~s claims to implement ~s, missing method ~s"
                c-name i-name i-m-name)))

;;;;;;;;;;;;;;;; types ;;;;;;;;;;;;;;;;

(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (equal? t1 t2)
        #t
        (eopl:error 'type-of
                    "Types didn't match: Original type - ~s, Input type - ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp))))

;; check-is-subtype! : Type * Type * Exp -> Unspecified
;; Page: 363
(define check-is-subtype!
  (lambda (ty1 ty2 exp)
    (if (is-subtype? ty1 ty2)
        #t
        (report-subtype-failure
         (type-to-external-form ty1)
         (type-to-external-form ty2)
         exp))))

(define report-subtype-failure
  (lambda (external-form-ty1 external-form-ty2 exp)
    (eopl:error 'check-is-subtype!
                "~s is not a subtype of ~s in~%~s"
                external-form-ty1
                external-form-ty2
                exp)))

;; need this for typing cast expressions
;; is-subtype? : Type * Type -> Bool
;; Page: 363
(define is-subtype? 
  (lambda (ty1 ty2)
    (cases type ty1
      (class-type (name1)
                  (cases type ty2
                    (class-type (name2)
                                (statically-is-subclass? name1 name2))
                    (else #f)))
      (proc-type (args1 res1)
                 (cases type ty2
                   (proc-type (args2 res2)
                              (and
                               (every2? is-subtype? args2 args1)
                               (is-subtype? res1 res2)))
                   (else #f)))
      (else (equal? ty1 ty2)))))

(define andmap
  (lambda (pred lst1 lst2)
    (cond
      ((and (null? lst1) (null? lst2)) #t)
      ((or (null? lst1) (null? lst2)) #f) ; or maybe throw error
      ((pred (car lst1) (car lst2))
       (andmap pred (cdr lst1) (cdr lst2)))
      (else #f))))

(define every2? andmap)

;; statically-is-subclass? : ClassName * ClassName -> Bool
;; Page: 363
(define statically-is-subclass?
  (lambda (name1 name2)
    (or
     (eqv? name1 name2)
     (let ((super-name
            (static-class->super-name
             (lookup-static-class name1))))
       (if super-name
           (statically-is-subclass? super-name name2)
           #f))
     (let ((interface-names
            (static-class->interface-names
             (lookup-static-class name1))))
       (memv name2 interface-names)))))

(define report-bad-type-to-cast 
  (lambda (type exp)
    (eopl:error 'bad-type-to-case
                "Can't cast non-object; ~s had type ~s"
                exp
                (type-to-external-form type))))

(define report-bad-type-to-instanceof
  (lambda (type exp)
    (eopl:error 'bad-type-to-case
                "Can't apply instanceof to non-object; ~s had type ~s"
                exp
                (type-to-external-form type))))


;; ======================================================================
;; tenv
;; ======================================================================
    
(provide (all-defined-out))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
   (syms (list-of symbol?))
   (vals (list-of type?))
   (tenv type-environment?))
  (extend-tenv-with-self-and-super
   (self type?)
   (super-name symbol?)
   (saved-env type-environment?)))

(define init-tenv
  (lambda ()
    ;(extend-tenv
    ; '(i v x)
    ; (list (int-type) (int-type) (int-type))
     (empty-tenv)))

(define apply-tenv
  (lambda (env search-sym)
    (cases type-environment env
      (empty-tenv ()
                  (eopl:error 'apply-tenv "No type found for ~s" search-sym))
      ;(extend-tenv (bvars types saved-env)
      ;             (cond
       ;              ((location search-sym bvars)
        ;              => (lambda (n) (list-ref types n)))
         ;            (else
         ;             (apply-tenv saved-env search-sym))))
      (extend-tenv (bvars types saved-tenv)
        (let ((n (location search-sym bvars)));vars 중에 search-sym이 있느냐?
          (if (>= n 0);있으면 vals의 n번째 항목 리턴
            (list-ref types n)
            (apply-tenv saved-tenv search-sym))))
      (extend-tenv-with-self-and-super (self-name super-name saved-env)
                                       (case search-sym
                                         ((%self) self-name)
                                         ((%super) super-name)
                                         (else (apply-tenv saved-env search-sym)))))))

;(define location
;  (lambda (sym syms)
    ;(cond
     ; ((null? syms) #f)
      ;((eqv? sym (car syms)) 0)
      ;((location sym (cdr syms)) => (lambda (n) (+ n 1)))
      ;(else #f))))




(define tenv->string-top
  (lambda (tenv)
    (format "[~a]" (tenv->string tenv))))

(define tenv->string
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv () "")
      (extend-tenv (vars types saved-tenv)
        (if (empty-tenv? saved-tenv)
            (string-append (arg:type->string vars types))
            (string-append (arg:type->string vars types) (tenv->string saved-tenv))))
      (extend-tenv-with-self-and-super (c-type s-name saved-tenv)
        (string-append (type->string c-type) (symbol->string s-name) (tenv->string saved-tenv)))
      (else
        (eopl:error 'tenv->string "~a" tenv)))))

(define empty-tenv?
  (lambda (tenv)
    (cases type-environment tenv
	   (empty-tenv () #t)
	   (else #f))))

;; ======================================================================

(define-datatype static-class static-class?
  (a-static-class (super-name (maybe symbol?)) (interface-names (list-of symbol?))
                  (field-names (list-of symbol?)) (field-types (list-of type?))
                  (method-tenv method-tenv?))
  (an-interface (method-tenv method-tenv?)))

(define method-tenv?
  (list-of 
   (lambda (p)
     (and 
      (pair? p)
      (symbol? (car p))
      (type? (cadr p))))))

(define static-class->field-names
  (lambda (class-name)
    (cases static-class class-name
      (a-static-class (s-name i-names f-names f-types m-tenv) f-names)
      (else
       eopl:error 'static-class->field-names "~a" class-name))))

(define static-class->field-types
  (lambda (class-name)
    (cases static-class class-name
      (a-static-class (s-name i-names f-names f-types m-tenv) f-types)
      (else
       eopl:error 'static-class->field-names "~a" class-name))))

(define static-class->method-tenv
  (lambda (class-name)
    (cases static-class class-name
      (a-static-class (s-name i-names f-names f-types m-tenv) m-tenv)
      (an-interface (m-tenv)
                    m-tenv))))

(define static-class->super-name
  (lambda (class-name)
    (cases static-class class-name
      (a-static-class (s-name i-names f-names f-types m-tenv) s-name)
      (else
       eopl:error 'static-class->field-names "~a" class-name))))

(define static-class->interface-names
  (lambda (class-name)
    (cases static-class class-name
      (a-static-class (s-name i-names f-names f-types m-tenv) i-names)
      (else
       eopl:error 'static-class->field-names "~a" class-name))))

(define the-static-class-env 'initialize)

(define empty-the-static-class-env!
  (lambda ()
    (set! the-static-class-env '())))

(define add-static-class-binding!
  (lambda (c-name c-decl)
    (set! the-static-class-env
         (cons 
          (list c-name c-decl)
          the-static-class-env))))

(define initialize-static-class-env!
  (lambda (c-decls)
    (empty-the-static-class-env!)
    (add-static-class-binding! 
     'object (a-static-class #f '() '() '() '()))
    (for-each add-class-decl-to-static-class-env! c-decls)))

(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl 
      (an-interface-decl (i-name abs-m-decls)
                         (let ((m-tenv
                                (abs-method-decls->method-tenv abs-m-decls)))
                           (check-no-dups! (map car m-tenv) i-name)
                           (add-static-class-binding!
                            i-name (an-interface m-tenv))))
      (a-class-decl (c-name s-name i-names
                            f-types f-names m-decls)
                    (let ((i-names
                           (append
                            (static-class->interface-names
                             (lookup-static-class s-name))
                            i-names))
                          (f-names
                           (append-new-field-names
                            (static-class->field-names
                             (lookup-static-class s-name))
                            f-names))
                          (f-types
                           (append
                            (static-class->field-types
                             (lookup-static-class s-name))
                            f-types))
                          (method-tenv
                           (let ((local-method-tenv
                                  (method-decls->method-tenv m-decls)))
                             (check-no-dups!
                              (map car local-method-tenv) c-name)
                             (merge-method-tenvs
                              (static-class->method-tenv
                               (lookup-static-class s-name))
                              local-method-tenv))))
                      (check-no-dups! i-names c-name)
                      (check-no-dups! f-names c-name)
                      (check-for-initialize! method-tenv c-name)
                      (add-static-class-binding! c-name
                                                 (a-static-class
                                                  s-name i-names f-names f-types method-tenv)))))))

(define append-new-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons 
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
        (append-new-field-names
         (cdr super-fields) new-fields))))))

(define lookup-static-class
  (lambda (name)
    (let ((maybe-pair (assq name the-static-class-env)))
      (if maybe-pair 
          (cadr maybe-pair)
          (eopl:error 'lookup-class "Unknown class name ~a" name)))))

(define check-no-dups!
  (lambda (lst c-name)
    (let loop ((rest lst))
      (cond
        ((null? rest) #t)
        ((memv (car rest) (cdr rest))
         (eopl:error 'check-no-dups! "Duplicate found among ~s in class ~s" lst
                     c-name))
        (else (loop (cdr rest)))))))

(define method-decls->method-tenv
  (lambda (m-decls)
    (map 
     (lambda (m-decl)
       (cases method-decl m-decl
         (a-method-decl (result-type m-name arg-ids arg-types body)
                        (list m-name (proc-type arg-types result-type)))))
     m-decls)))

(define abs-method-decls->method-tenv
  (lambda (abs-m-decls)
    (map 
     (lambda (abs-m-decl)
       (cases abstract-method-decl abs-m-decl
         (an-abstract-method-decl (result-type m-name arg-ids arg-types)
                                  (list m-name (proc-type arg-types result-type)))))
     abs-m-decls)))

(define merge-method-tenvs
  (lambda (super-tenv new-tenv)
    (append new-tenv super-tenv)))

(define check-for-initialize!
  (lambda (method-tenv class-name)
    (unless (maybe-find-method-type method-tenv 'initialize)
      (eopl:error 'check-for-initialize!
                  "No initialize method in class ~s"
                  class-name))))


;; ======================================================================

(define trace-check-enter
  (lambda (exp tenv)
    (and trace-flag
         (set! trace-no-of-spaces (+ trace-no-of-spaces 1))
         (trace-check-spaces trace-no-of-spaces)
         (display (format "+ exp=~a~n" (exp->string exp)))
         (trace-check-spaces trace-no-of-spaces)
         (display (format "| tenv=~a~n" (tenv->string-top tenv))))))

(define trace-check-exit
  (lambda (type)
    (and trace-flag
         (trace-check-spaces trace-no-of-spaces)
         (display (format "- type=~a~n" (type->string type)))
         (set! trace-no-of-spaces (- trace-no-of-spaces 1)))))

(define trace-check-spaces
  (lambda (n)
    (if (eqv? n 0)
        (display "")
        (begin (display "|")
               (trace-check-spaces (- n 1))))))