#lang eopl

(require "parse.scm" "store.scm" "format.scm")

(provide (all-defined-out))


; 2013. 5. 17 modify : 김용규
; class field-types를 넣어줌. interface를 저장할 수 있는 i-class를 만들어줌.
(define-datatype class class?
  (a-class
    (super-name (maybe symbol?))
    (interface-names (list-of symbol?))
    (field-types (list-of type?))
    (field-names (list-of symbol?))
    (method-env method-environment?))
  (i-class
   (method-env method-environment?)))



;; object의 super-name을 #f로 표현하기 때문에 함수 maybe는 pred가 #f 혹은 symbol이면 #t를 리턴함
(define maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v)))))

(define the-class-env '());list of (class-name,class)

;; -t 옵션 시 class-env 확인
(define class-env->string-top
  (lambda (class-env)
    (format "[~a]" (class-env->string class-env))))

;; (class->string (cadar class-env)) <- 왜 cadar이여야 하는지 모르겠다 ㅠㅠ
(define class-env->string
  (lambda (class-env)
    (if (null? (cdr class-env))
      (string-append "class-name=" (symbol->string (caar class-env))
                     " class=" (class->string (cadar class-env)))
      (string-append "class-name=" (symbol->string (caar class-env))
		     " class=" (class->string (cadar class-env))
		     "\n" (class-env->string (cdr class-env))))))

;; class를 string으로 변환해주는 procedure
(define class->string
  (lambda (c)
    (cases class c
      (a-class (s-name i-names f-types f-names m-env)
        (string-append "super-name:" (s-name->string s-name)
                       " interface-name:" (list-of-symbols->string i-names)
		       " field type : field-name:(" (field->string f-types f-names) ")"
		       " method-env:" "[" (method-env->string m-env) "]" ))
      (i-class (m-env)
        (string-append "method-env:" "[" (method-env->string m-env) "]")))))
  
  
; 2013. 5. 17 modify : 김용규
; field의 type과 field name을 출력해주는 함수
(define field->string
  (lambda (types names)
    (if (null? names)
      ""
      (if (null? (cdr names))
        (string-append (type->string (car types)) " " (symbol->string (car names)))
        (string-append (type->string (car types)) " " (symbol->string (car names)) ","
                       (field->string (cdr types) (cdr names)))))))

;; (list-of symbol?) 을 string으로 변환해주는 함수
(define list-of-symbols->string
  (lambda (list-symbol)
    (if (null? list-symbol) ""
      (if (null? (cdr list-symbol))
        (string-append (symbol->string (car list-symbol)))
        (string-append (symbol->string (car list-symbol)) " " (list-of-symbols->string (cdr list-symbol)))))))
   

;; super-name type이 (maybe symbol?) 이므로 맞춰서 변환해야 한다.
;; 만약 최상위 object라면 #f를 반환함
(define s-name->string
  (lambda (s-name)
    (if ((maybe symbol?) s-name)
      (if (eqv? s-name #f)
        "#f"
        (symbol->string s-name))
      (eopl:error "s-name->string"))))

;;; add-to-class-env! 함수는 class-env에 (class-name class)를 추가해주는 함수
;;; class-name은 symbol type , class는 class type이다.
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
      (cons (list class-name class) the-class-env))))

;; the-class-env에서 class가 있는지 찾는 procedure
;; 해당 class가 없다면 오류를 출력한다.
(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
        (eopl:error 'lookup-class "Unknown class name ~a" name)))))

; the-class-env에서 class 및 interface가 있는지 찾는 procedure
; names의 저장 형태가 (class-name (interface-names)) 저장되어 있기에 class-name부터 찾고 없을 시 interface-names를 차례로 검색
(define lookup-class-and-interface
  (lambda (names)
    (if (null? names)
      (eopl:error 'lookup-class-and-interface "Unknow class name")
      (if (list? (car names))
        (let ((exist (assq (caar names) the-class-env)))
          (if exist
            (cadr exist)
            (lookup-class-and-interface (cdar names))))
        (let ((exist (assq (car names) the-class-env)))
          (if exist
            (cadr exist)
            (lookup-class-and-interface (cdr names))))))))

;; the-class-env 환경을 초기화 시켜준다
;; 최상위 class이름은 object 이며 object의 super-class는 #f이다.
;; field-names와 method-env는 empty-list로 초기화 시켜준다
;; c-decls가 class랑 interface에 의해 다르게 설정
(define initialize-class-env!
  (lambda (c-decls)
;    (cases class-decl c-decls
;      (a-class-decl (c-name s-name i-names f-types f-names m-decls)
       (set! the-class-env
         (list (list 'object (a-class #f '() '() '() '() ))))
;      (an-interface-decl (i-name abs-m-decls)
;      (set! the-class-env
;         (list (list 'object (i-class '())))))
       (for-each initialize-class-decl! c-decls)))


; 2013. 5. 17 make : 김용규
; class에서 field-types만 extract 하는 procedure

(define class->field-types
  (lambda (class-name)
    (cases class class-name
      (a-class (s-name i-names f-types f-names m-env) f-types)
      (i-class (m-env) ""))))

;; class에서 field-names만 extract 하는 procedure
(define class->field-names
  (lambda (class-name)
    (cases class class-name
      (a-class (s-name i-names f-types f-names m-env) f-names)
      (i-class (m-env) ""))))


;; class에서 method-env만 extract 하는 procedure
(define class->method-env
  (lambda (class-name)
    (cases class class-name
      (a-class (s-name i-names f-types f-names  m-env) m-env)
      (i-class (m-env) m-env))))

(define class->super-name
  (lambda (class-name)
    (cases class class-name
      (a-class (s-name i-names f-types f-names  m-env) s-name)
      (i-class (m-env) m-env))))


;; 클래스의 정보를 the-class-env에 저장한다.
; 2013. 5. 18 수정
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name i-names f-types f-names m-decls)
        (let ((f-names (append-field-names
              (class->field-names (lookup-class s-name)) f-names)))
          (add-to-class-env!
            c-name
            (a-class s-name i-names f-types f-names
              (merge-method-envs
                (class->method-env (lookup-class s-name))
                (method-decls->method-env m-decls s-name i-names f-names f-types))))))
      (an-interface-decl (i-name abs-m-decls)
        (add-to-class-env!
         i-name
         (i-class 
          (abstract-method-decls->method-env abs-m-decls)))))))
      


;; field 이름들을 추가한다.
;; 만약 super-fields가 null이라면 new-fields를 return하고
;; 그렇지 않다면 new-fields와 일치되는 부분이 있다면 field shadow 표시를 해주고
;; 일치되는 부분이 없다면 리턴한다.
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields)
        new-fields)
      (else
        (cons
          (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
          (append-field-names (cdr super-fields) new-fields))))))


;;method의 field shadow
(define fresh-identifier-no 0)

;; field shadow를 표현해주는 fresh-identifier
;; field shadow시 superclass의 field를 identifier%number로 표시한다. 
(define fresh-identifier
  (lambda (identifier)
    (set! fresh-identifier-no (+ fresh-identifier-no 1))
    (string->symbol
      (string-append
        (symbol->string identifier) "%"
        (number->string fresh-identifier-no)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;method
;;;;;;;;;;;;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 2013. 5. 18 modify : 김용규
; 추상메소드랑 일반 메소드 구분 및 필드 추가
(define-datatype method method?
  (a-method
    (result-type type?)
    (vars (list-of symbol?))
    (var-types (list-of type?))
    (body expression?)
    (super-name symbol?)
    (interface-names (list-of symbol?))
    (field-names (list-of symbol?))
    (field-types (list-of type?)))
  (i-method
   (result-type type?)
   (vars (list-of symbol?))
   (var-types (list-of type?))))

;; method-environment는 주어진 class 내에 정의된 모든 method에 대한 정보를 가지고 있음
;; method-environment는 (method-name method) 형태의 pair list로 구성됨
(define method-environment?
  (list-of (lambda (p)
      (and (pair? p)
        (symbol? (car p))
        (method? (cadr p))))))


;; m-env의 형태는  ((method-name method) ...) 형태
;; method-name은 symbol type이며 method는 method type이다.
;; (method->string (cadar m-env) <- 왜 cadar이여야 하는지 모르겠다 ᅲᅲ
(define method-env->string
 (lambda (m-env)
  (if (null? m-env)
   ""
   (if (null? (cdr m-env))
    (string-append "method-name:" (symbol->string (caar m-env)) ","
      		  "method:" "(" (method->string (cadar m-env)) ")")
    (string-append "method-name:" (symbol->string (caar m-env)) ","
 		  "method:" "(" (method->string (cadar m-env)) ")"
 		  ", " (method-env->string (cdr m-env)))))))

;; (a-method vars body s-name i-names f-names f-types)
;; vars : (list-of symbol)
;; body : expression
;; s-name : symbol : super-name
;; i-names : (list-of symbol) : interface-names
;; f-names : (list-of symbol) : field-names
;; f-types : (list-of type) : field-types
(define method->string
 (lambda (m)
  (cases method m
   (a-method (r-type vars var-types body s-name i-names f-names f-types)
    (string-append "result-type:" (type->string r-type) "," 
                   " vars : var-types" "(" (arg:type->string vars var-types) ")"  ","
		   " body:" (exp->string body) "," 
		   " super-name:" (symbol->string s-name) ","
                   " interface-name:" "(" (list-of-symbols->string i-names) ")" ","
		   " field-names : field-types:" "(" (arg:type->string f-names f-types) ")"))
   (i-method (r-type vars var-types)
    (string-append "result-type:" (type->string r-type) "," 
                   " vars : var-types" "(" (arg:type->string vars var-types) ")")))))


;; method-decls에서 method-env 부분을 extract 하는 procedure
;; method-decls를 받아서 (method-name (a-method vars body super-name field-names))를 return 한다
;; 2013. 5. 17 a-method 추가된 result-type과 var-types 수정 부분 필요.
(define method-decls->method-env
  (lambda (m-decls super-name interface-names field-names field-types)
    (map
      (lambda (m-decl)
        (cases method-decl m-decl
          (a-method-decl (result-type method-name vars var-types body)
            (list method-name (a-method result-type vars var-types body super-name 
                                        interface-names field-names field-types)))))
      m-decls)))

; 2013. 5. 18 make : 김용규
; abstract-method-decls를 받아서 method-env 부분을 extract 하는 procedure
(define abstract-method-decls->method-env
  (lambda (abs-m-decls)
    (map
     (lambda (abs-m-decl)
       (cases abstract-method-decl abs-m-decl
         (an-abstract-method-decl (result-type method-name m-vars m-var-types)
            (list method-name (i-method result-type m-vars m-var-types)))))
     abs-m-decls)))

;Sym * Sym -> method
;; c-name에서 name에 해당하는 method가 있는지 찾는다
;; 있으면 method를 반환하고 그렇지 않다면 오류를 발생한다.
; 2013. 5. 18
; c-name이 (c-name (i-names))로 변환.
; extends되는 부분, c-name부터 검색하고 그 다음 i-names 검색
(define find-method-with-interface
  (lambda (c-names name)
    (let ((m-env (class->method-env (lookup-class-and-interface c-names))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (eopl:error 'find-method "Unknown method ~a" name))))))

(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (eopl:error 'find-method "Unknown method ~a" name))))))

; MethodEnv * MethodEnv -> MethodEnv

;; super-m-env와 new-m-env를 합쳐주는 procedure
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype object object?
  (an-object
    (class-name symbol?)
    (fields (list-of reference?))))

;; object에서 fields를 extract하는 procedure
(define object->fields
 (lambda (obj)
  (cases object obj
   (an-object (c-name fields) fields))))

;; class-name을 받아서 새로운 object를 만들어주는 new-object procedure
;; 해당 class들의 field를 0으로 초기화시킨다.
(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map
        (lambda (field-name)
          (newref (num-val 0)));초기 값을 0으로
        (class->field-names (lookup-class class-name))))))

;; class-name(field1 field2 ..) 이런식으로 만든다
;; ex) c3(1 2 3)
;; refs->string은 store.scm에 구현되어 있음
(define object->string
 (lambda (obj)
  (cases object obj
   (an-object (c-name fields)
    (string-append (symbol->string c-name) "(" (refs->string fields) ")")))))

;; object의 class-name을 리턴한다.
(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (c-name fiels) c-name)
      (else
        (eopl:error 'object->class-name "arg=~a" obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ======================================================================
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val reference?)
   (saved-env environment?))
  (extend-env*
   (vars (list-of symbol?))
   (vals (list-of reference?))
   (saved-env environment?))
  ; letrec-exp 저장
  ; letrec-exp(p-result-types p-names b-varss b-var-typess p-bodies letrec-body)의 cfg
  ; Expression ::= letrec{type Identifier ({Identifier : type}*(,)) = Expression}* in Expression  
  (extend-env-rec
   (p-result-types (list-of type?))
   (p-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
   (b-var-typess (list-of (list-of type?)))
   (p-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-rec*
   (p-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
   (p-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self object?)
   (super-name symbol?)
   (interface-names (list-of symbol?))
   (saved-env environment?)))  

;; ======================================================================


;; environment에서 해당 search-sym을 찾아서 원하는 값을 반환하는 apply-env procedure
(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "Can't found symbol ~a" search-sym))
      (extend-env (var val saved-env)
        (if (eqv? search-sym var)
          val
          (apply-env saved-env search-sym)))
      (extend-env* (vars vals saved-env)
        (let ((n (location search-sym vars)));vars 중에 search-sym이 있느냐?
          (if (>= n 0);있으면 vals의 n번째 항목 리턴
            (list-ref vals n)
            (apply-env saved-env search-sym))))
      (extend-env-rec (p-result-types p-names b-varss b-var-typess p-bodies saved-env)
        (let ((n (location search-sym p-names)))
          (if (>= n 0)
              (newref (proc-val (procedure (list-ref b-varss n) (list-ref p-bodies n) env)))
              (apply-env saved-env search-sym))))
      (extend-env-rec* (p-names b-varss p-bodies saved-env)
        (let ((n (location search-sym p-names)))
          (if (>= n 0)
            (newref (proc-val (procedure (list-ref b-varss n) (list-ref p-bodies n) env)))
            (apply-env saved-env search-sym))))
      

;; 2013. 3. 28 용규 추가
;; search-sym이 '%self이면 (self-exp ()) 또는 (super-call-exp)이라면 self의 object를 던져준다
;; search-sym이 '%super이면 super-class의 이름인 s-name을 던져준다
      (extend-env-with-self-and-super (self s-name i-names saved-env)
	(if (eqv? search-sym '%self)
	 self
	  (if (eqv? search-sym '%super)
	   (list s-name i-names)
	   (apply-env saved-env search-sym))))
      (else
        (eopl:error 'apply-env "arg=~a" env)))))


(define init-env 
  (lambda ()
    (let* ((tmp (empty-env))) tmp)))

(define location
  (lambda (var p-names)
    (letrec ((location_n (lambda (v list n)
            (if (null? list)
              -1
              (if (eqv? v (car list))
                n
                (location_n v (cdr list) (+ n 1)))))))
      (location_n var p-names 0))))

(define env->string-top
  (lambda (env)
    (format "[~a]" (env->string env))))

(define env->string
  (lambda (env)
    (cases environment env
      (empty-env () "")
      (extend-env (var val saved-env)
          (if (empty-env? saved-env)
	   (string-append (symbol->string var) "=" (number->string val))
	   (string-append (symbol->string var) "=" (number->string val) "," (env->string saved-env))))
;; 용규 추가 ;;
      (extend-env* (vars vals saved-env)
;;       (if (empty-env? saved-env)
       (if (null? (cdr vars))
	(if (empty-env? saved-env)
	 (string-append (symbol->string (car vars)) "=" (number->string (car vals)))
         (string-append (symbol->string (car vars)) "=" (number->string (car vals)) "," (env->string saved-env)))
        (if (empty-env? saved-env)
	 (string-append (symbol->string (car vars)) "=" (number->string (car vals)) ","  (symbol->string (cadr vars)) "=" (number->string (cadr vals)))
	 (string-append (symbol->string (car vars)) "=" (number->string (car vals)) "," (symbol->string (cadr vars)) "=" (number->string (cadr vals)) 
			"," (env->string saved-env)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (extend-env-rec (p-result-types p-names b-varss b-var-typess p-bodies saved-env)
       (if (null? p-names)
        ""
        (if (null? (cdr p-names))
         (string-append (type->string (car p-result-types)) " " (symbol->string (car p-names)) 
                        "(" (arg:type->string b-varss b-var-typess) ")" "=" (exp->string p-bodies))
         (string-append (type->string (car p-result-types)) " " (symbol->string (car p-names)) 
                        "(" (arg:type->string b-varss b-var-typess) ")" "=" (exp->string p-bodies)
                        (env->string saved-env))))) 
      (extend-env-rec* (p-names b-varss p-bodies saved-env)
        (cases environment saved-env
          (empty-env ()
            (format "~a" (env-recs->string p-names b-varss p-bodies saved-env)))
          (else
            (format "~a,~a" (env-recs->string p-names b-varss p-bodies saved-env) (env->string saved-env)))))

;; 용규 추가 ;;
;; self : object type
;; super-name : symbol type
;; saved-env : environment type
 
      (extend-env-with-self-and-super (self s-name i-names saved-env)
	(if (empty-env? saved-env)
	 (string-append "self=" (object->string self) "," 
			"super=" (symbol->string s-name) ","
                        "interface=" "(" (map (lambda (i-name) (symbol->string i-name)) i-names) ")")
	 (string-append  "self=" (object->string self) "," 
			"super=" (symbol->string s-name) ","
                        "interface=" "(" (map (lambda (i-name) (symbol->string i-name)) i-names) ")" ","
			(env->string saved-env))))
      (else
        (eopl:error 'env->string "arg=~a" env)))))

(define env-recs->string
  (lambda (p-names b-varss p-bodies saved-env)
    (cond
      ((null? p-names)
        (format ""))
      ((null? (cdr p-names))
        (format "~a(~a)=~a"
          (car p-names)
          (env-recs-vars->string (car b-varss))
          (exp->string (car p-bodies))))
      (else
        (format "~a(~a)=~a;~a"
          (car p-names)
          (env-recs-vars->string (car b-varss))
          (exp->string (car p-bodies))
          (env-recs->string
            (cdr p-names) (cdr b-varss)
            (cdr p-bodies) saved-env))))))

(define env-recs-vars->string
  (lambda (b-vars)
    (cond
      ((null? b-vars) "")
      ((null? (cdr b-vars))
        (format ",~a" (car b-vars)))
      (else
        (format "~a,~a" (car b-vars) (env-recs-vars->string (cdr b-vars)))))))

(define empty-env?
  (lambda (env)
    (cases environment env
      (empty-env () #t)
      (else #f))))

;; ======================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ======================================================================

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (proc-val (proc1 proc?))
  (obj-val (obj object?))
  (list-val (lst (list-of expval?))))

(define-datatype proc proc?
  (procedure (vars (list-of symbol?)) (var-types (list-of type?))
             (body expression?) (saved-env environment?)))

;; ======================================================================

(define expval->num
  (lambda (ev)
    (cases expval ev
      (num-val (num) num)
      (else (eopl:error 'expval->num "arg=~a" ev)))))

(define expval->bool
  (lambda (ev)
    (cases expval ev
      (bool-val (bool) bool)
      (else (eopl:error 'expval->num "arg=~a" ev)))))

(define expval->proc
  (lambda (ev)
    (cases expval ev
      (proc-val (proc1) proc1)
      (else (eopl:error 'expval->proc "arg=~a" ev)))))

(define expval->obj
  (lambda (ev)
    (cases expval ev
      (obj-val (obj) obj)
      (else (eopl:error 'expval->obj "arg=~a" ev)))))

(define expval->list
  (lambda (ev)
    (cases expval ev
      (list-val (lst) lst)
      (else (eopl:error 'expval->list "arg=~a" ev)))))

(define expval->string
  (lambda (ev)
    (cases expval ev
      (num-val (num)
        (format "~a" num))
      (bool-val (bool)
        (format "~a" bool))
      (proc-val (proc1)
        (cases proc proc1
          (procedure (vars v-types body saved-env)
            ;(format "proc(~a)~a~a" vars (exp->string body) (env->string-top saved-env)))
            (string-append "proc" "(" (arg:type->string vars v-types) ")" (exp->string body) (env->string-top saved-env)))
          (else
            (eopl:error 'expval->string "proc-val arg=~a" proc1))))

;; 용규 수정 ;;
;; fields는 list-of reference이기에 number의 list이다.
;; refs->string은 store.scm에 구현되어있다.
       (obj-val (obj)
	(cases object obj
	 (an-object (c-name fields)
	  (string-append (symbol->string c-name) "("
			 (refs->string fields) ")"))))

;; 2013. 3. 28 용규 수정
;; expval->string lst-val 수정
;; list-val은 (list-of expval) 이므로 각각의 expval을 구해야 한다
      (list-val (lsts)
       (string-append "(" (list-of-expval->string lsts) ")"))       
      (else
        (eopl:error 'expval->string "arg=~a" ev)))))


; procedure vars와 types를 표현하기 위한 함수
; 2013. 5. 17일
; craetor : 김용규

(define arg:type->string
  (lambda (args types)
    (if(null? args)
     ""
     (if (null? (cdr args))
      (string-append (symbol->string (car args)) ":" (type->string (car types)))
      (string-append (symbol->string (car args)) ":" (type->string (car types)) " " 
                     (arg:type->string (cdr args) (cdr types)))))))

;; list-of expval을 구하기 위한 함수

(define list-of-expval->string
  (lambda (list-expval)
    (if (null? list-expval)
      ""
      (if (null? (cdr list-expval))
        (string-append (expval->string (car list-expval)))
        (string-append (expval->string (car list-expval)) " " (list-of-expval->string (cdr list-expval)))))))

;; ======================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;store의 일부
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define store->string-top
  (lambda (store)
    (format "(~a)" (store->string store))))

(define store->string
  (lambda (store)
    (cond
      ((null? store)
        (format ""))
      ((null? (cdr store))
        (format "~a" (expval->string (car store))))
      (else
        (format "~a,~a" (expval->string (car store)) (store->string (cdr store)))))))
