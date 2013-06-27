#lang eopl

(require "format.scm")
(provide (all-defined-out))
;; ======================================================================

;; typed-object-oriented 언어 추가 된 expression
;; 2013. 5. 14(화) 수정자 김용규, 박승주
;; 수정내용
;; 1. ppt에 맞게 문법 순서 정리1
;; 2. 새로 추가 된 문법 추가


;; a-program(class-decls body)의 context free grammar
;; Program ::= {ClassDecl}* Expression
(define-datatype program program?
  (a-program (class-decls (list-of class-decl?)) (body expression?)))

;; a-class-decl(class-name super-name interface-names field-types field-names method-decls)의 cfg
;; ClassDecl ::= class Identifier extends Identifier {implements Identifier}*
;;               {field type Identifier}* {MethodDecl}*
(define-datatype class-decl class-decl?
  (a-class-decl (class-name symbol?) (super-name symbol?) (interface-names (list-of symbol?))
                (field-types (list-of type?)) (field-names (list-of symbol?))
                (method-decls (list-of method-decl?)))

;; an-interface-decl(interface-name abs-method-decls)의 cfg
;; ClassDecl ::= interface Identifier {AbstractMethodDecl}*
  (an-interface-decl (interface-name symbol?) (abs-method-decls (list-of abstract-method-decl?))))
   
;; a-method-decl(result-type method-name vars var-types body)의 cfg
;; MethodDecl ::= method type Identifier({Identifier : type}*(,)) Expression

(define-datatype method-decl method-decl?
  (a-method-decl (result-type type?) (method-name symbol?) 
                 (vars (list-of symbol?)) (var-types (list-of type?)) (body expression?)))

;; an-abstract-method-decl(result-type m-name m-vars m-var-types)의 cfg
;; AbstractMethodDecl ::= method type Identifier({Identifier : type}*(,))

(define-datatype abstract-method-decl abstract-method-decl?
   (an-abstract-method-decl (result-type type?) (m-name symbol?)
                            (m-vars (list-of symbol?)) (m-var-types (list-of type?))))


(define-datatype expression expression?
;; const-exp(num)의 context free grammar
;; Expression ::= Number
  (const-exp (num number?))
  
;; var-exp(var)의 context free grammar
;; Expression ::= Identifier
  (var-exp (var symbol?))
  
;; true-exp()의 context free grammar
;; Expression :: true
  (true-exp)

;; false-exp()의 context free grammar
;; Expression :: false
  (false-exp)
  
;; string-exp(str)의 context free grammar
  (string-exp (str string?))

;; zero-exp(exp1)의 cfg
;; Expression ::= zero?(Expression)  
  (zero?-exp (exp1 expression?))
  
;; if-exp(exp1 exp2 exp3)의 cfg
;; Expression ::= if Expression then Expression else Expression
  (if-exp (exp1 expression?) (exp2 expression?) (exp3 expression?))
  
;; assign-exp(var exp1)의 cfg
;; Expression ::= set Identifier = Expression  
  (assign-exp (var symbol?) (exp1 expression?))
  
;; begin-exp(exp1 exps)의 cfg
;; Expression ::= begin Expression {; Expression}* end
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  
;; sum-exp(exp1 exp2), diff-exp(exp1 exp2), mul-exp(exp1 exp2)
;; div-exp(exp1 exp2), mod-exp(exp1 exp2)의 context free grammar 
;; Expression ::= +(Expression, Expression)                  
;; Expression ::= -(Expression, Expression)                  
;; Expression ::= *(Expression, Expression)                                    
;; Expression ::= /(Expression, Expression)                                    
;; Expression ::= %(Expression, Expression)
  (sum-exp (exp1 expression?) (exp2 expression?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (mul-exp (exp1 expression?) (exp2 expression?))
  (div-exp (exp1 expression?) (exp2 expression?))
  (mod-exp (exp1 expression?) (exp2 expression?))
  
;; list-exp(lst)의 context free grammar
;; Expression ::= list({Expression}*(,))                  
  (list-exp (exps (list-of expression?)))
  
;; lex-exp(vars exps body)의 cfg
;; Expression ::= let {Identifier = Expression}* in Expression  
  (let-exp (vars (list-of symbol?)) (exps (list-of expression?)) (body expression?))
  
;; call-exp(rator rands)의 cfg
;; Expression ::= (Expression({Expression}*(,)))
  (call-exp (rator expression?) (rands (list-of expression?)))

;; proc-exp(vars types body)의 cfg
;; Expression ::= proc({Identifier : type}*(,)) Expression
  (proc-exp (vars (list-of symbol?)) (types (list-of type?)) (body expression?))

;; letrec-exp(p-result-types p-names b-varss b-var-typess p-bodies letrec-body)의 cfg
;; Expression ::= letrec{type Identifier ({Identifier : type}*(,)) = Expression}* in Expression  
  (letrec-exp (p-result-types (list-of type?)) (p-names symbol?) (b-varss (list-of (list-of symbol?)))
              (b-var-typess (list-of (list-of type?))) (p-bodies (list-of expression?)) 
              (letrec-body expression?))

;; new-object-exp (class-name rands)의 cfg
;; Expression ::= new Identifier({Expression}*(,))
  (new-object-exp (class-name symbol?) (rands (list-of expression?)))

;; method-call-exp(obj-exp method-name rands)의 cfg
;; Expression ::= send Expression Identifier({Expression}*(,))  
  (method-call-exp (obj-exp expression?) (method-name symbol?) (rands (list-of expression?)))
  
;; self-exp()의 cfg
;; Expression ::= self  
  (self-exp)
  
;; super-call-exp(method-name rands)의 cfg  
  (super-call-exp (method-name symbol?) (rands (list-of expression?)))
                  
                  
;; cast-exp(obj-exp c-name)의 cfg
;; Expression ::= cast Expression Identifier
  (cast-exp (obj-exp expression?) (c-name symbol?))              
  
;; instanceof-exp(exp name)의 cfg
;; Expression ::= instanceof Expression Identifier  
  (instanceof-exp (exp expression?) (name symbol?)))
;; ======================================================================

(define-datatype type type?
;; int-type()의 cfg
;; type ::= int  
  (int-type)
  
;; bool-type()의 cfg
;; type ::= bool  
  (bool-type)
  
;; string-type()의 cfg
;; type ::= string
  (string-type)
  
;; proc-type(arg-types result-type)의 cfg  
;; type ::= ({type}+(,) -> type)  
  (proc-type (arg-types (list-of type?)) (result-type type?))
  
;; void-type()의 cfg
;; type ::= void  
  (void-type)
  
;; class-type(class-name)의 cfg  
;; type ::= Identifier  
  (class-type (class-name symbol?))
  
;; list-type(type1)의 cfg  
;; type ::= listof type
  (list-type (type1 type?)))

(define type->class-name
  (lambda (ty)
    (cases type ty
      (class-type (class-name) class-name)
      (else
       (eopl:error 'type->class-name "~a" ty)))))

(define class-type?
  (lambda (ty)
    (cases type ty
      (class-type (name) #t)
      (else #f))))
  
(define scanner-rules
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "?" "-"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (string ("\"" (arbno any) "\"") string)))

(define parser-rules
  ;; Program ::= {ClassDecl}* Expression
  '((program ((arbno class-decl) expression) a-program)

    ;; ClassDecl ::= class Identifier extends Identifier {implements Identifier}*
    ;;               {field type Identifier}* {MethodDecl}*
    (class-decl ("class" identifier "extends" identifier (arbno "implements" identifier)
                         (arbno "field" type identifier) (arbno method-decl)) a-class-decl)
    
    ;; ClassDecl ::= interface Identifier {AbstractMethodDecl}*
    (class-decl ("interface" identifier (arbno abstract-method-decl)) an-interface-decl)
    
    ;; MethodDecl ::= method type Identifier({Identifier : type}*(,)) Expression
    (method-decl ("method" type identifier "(" (arbno identifier ":" type (arbno ",")) ")" expression) a-method-decl)
    
    ;; AbstractMethodDecl ::= method type Identifier({Identifier : type}*(,))
    (abstract-method-decl ("method" type identifier "(" (arbno identifier ":" type (arbno ",")) ")") an-abstract-method-decl)
    ;;======================================17:50 박승주 수정.============================
    ;; Expression ::= Number
    (expression (number) const-exp)
    
    ;; Expression ::= Identifier
    (expression (identifier) var-exp)
    
    ;; Expression ::= true
    (expression ("true") true-exp)
    
    ;; Expression ::= false
    (expression ("false") false-exp)
    
    ;; Expression :: String
    (expression (string) string-exp)
    
    ;; Expression ::= zero?(Expression)  
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    ;; Expression ::= if Expression then Expression else Expression
    (expression ("if" expression "then" expression "else" expression) if-exp)
    
    ;; Expression ::= set Identifier = Expression
    (expression ("set" identifier "=" expression) assign-exp)
    
    ;; Expression ::= begin Expression {; Expression}* end
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    
    ;; Expression ::= +(Expression, Expression) 
    (expression ("+" "(" expression "," expression ")") sum-exp)
    
    ;; Expression ::= -(Expression, Expression)                  
    (expression ("-" "(" expression "," expression ")") diff-exp)
    
    ;; Expression ::= *(Expression, Expression)                                    
    (expression ("*" "(" expression "," expression ")") mul-exp)
    
    ;; Expression ::= /(Expression, Expression)                                    
    (expression ("/" "(" expression "," expression ")") div-exp)
    
    ;; Expression ::= %(Expression, Expression)
    (expression ("%" "(" expression "," expression ")") mod-exp)
    
    ;; Expression ::= list({Expression}*(,))                  
    (expression ("list" "("(arbno expression (arbno ",")) ")") list-exp)
    
    ;; Expression ::= let {Identifier = Expression}* in Expression  
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    ;; Expression ::= (Expression({Expression}*(,)))
    (expression ("(" expression "(" (arbno expression (arbno ",")) ")" ")") call-exp)
    
    ;; Expression ::= proc({Identifier : type}*(,)) Expression
    (expression ("proc" "(" (arbno identifier ":" type (arbno ",")) ")" expression) proc-exp)
    
    ;; Expression ::= letrec{type Identifier ({Identifier : type}*(,)) = Expression}* in Expression  
    (expression ("letrec" (arbno type identifier "(" (arbno identifier ":" type (arbno ",")) ")" "=" 
                                 expression) "in" expression) letrec-exp)
    
    ;; Expression ::= new Identifier({Expression}*(,))
    (expression ("new" identifier "(" (arbno expression (arbno ",")) ")") new-object-exp)
    
    ;; Expression ::= send Expression Identifier({Expression}*(,))  
    (expression ("send" expression identifier "(" (arbno expression (arbno ",")) ")") method-call-exp)
    
    ;; Expression ::= self  
    (expression ("self") self-exp)
    
    ;; Expression ::== super Identifier ({Expression}*(,))
    (expression ("super" identifier "(" (arbno expression (arbno ",")) ")") super-call-exp)

    ;; Expression ::= cast Expression Identifier
    (expression ("cast" expression identifier) cast-exp)

    ;; Expression ::= instanceof Expression Identifier  
    (expression ("instanceof" expression identifier) instanceof-exp)
    
    ;; type ::= int
    (type ("int") int-type)
    
    ;; type ::= bool
    (type ("bool") bool-type)
    
    ;; type ::= string
    (type ("string") string-type)
    
    ;; type ::= ({type}+(,) -> type)
    (type ("(" type (arbno "," type) "->" type ")") proc-type)
    
    ;; type ::= void
    (type ("void") void-type)
    
    ;; type ::= Identifier
    (type (identifier) class-type)
    
    ;; type ::= listof type
    (type ("listof" type) list-type)))

;; 5.14일 19:42분  parser-rule 까지 수정완료.(박승주)
    
    
(define scan&parse
  (sllgen:make-string-parser scanner-rules parser-rules))

;; ======================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pgm->string
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls body)
       (string-append (exp->string body))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exp->string
  (lambda (exp)
    (cases expression exp
      (const-exp (num)
        (format "~a" num))
      (var-exp (var)
        (format "~a" var))
      (true-exp () "true")
      (false-exp () "false")
      (string-exp (str)
         (format "\"~a\"" str))
      (zero?-exp (exp1)
        (format "zero?(~a)" (exp->string exp1)))
      (diff-exp (exp1 exp2)
	(format "-(~a,~a)" (exp->string exp1) (exp->string exp2)))
      (if-exp (exp1 exp2 exp3)
        (format "if ~a then ~a else ~a" (exp->string exp1) (exp->string exp2) (exp->string exp3)))
      (let-exp (vars exps body)
	;;; 용규 수정 ;;;
        ;;(format "let ~a=~a in ~a" var (exp->string exp1) (exp->string body)))
	(string-append "let " (let->exp vars exps) " in " (exp->string body)))
      (proc-exp (vars types body)
        (format "proc(~a)~a" vars (exp->string body)))
      (call-exp (rator rand)
        (format "(~a ~a)" (exp->string rator) (exp->string rand)))
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (format "letrec ~a in ~a"
          (letrec ((proc-list->string (lambda (p-name-list b-var-list p-body-list)
                (cond
                  ((null? p-name-list)
                    (format ""))
                  ((null? (cdr p-name-list))
                    (format "~a(~a)=~a"
                       (car p-name-list) (car b-var-list) (exp->string (car p-body-list))))
                  (else
                    (format "~a(~a)=~a~a"
                       (car p-name-list) (car b-var-list) (exp->string (car p-body-list))
                       (proc-list->string (cdr p-name-list) (cdr b-var-list) (cdr p-body-list))))))))
            (proc-list->string p-names b-varss p-bodies))
          (exp->string letrec-body)))
      (begin-exp (exp1 exps)
        (format "begin ~a~a end" (exp->string exp1)
          (letrec ((exps->string (lambda (exp-list)
                (cond
                  ((null? exp-list)
                    (format ""))
                  ((null? (cdr exp-list))
		    (format ";~a" (exp->string (car exp-list))))
                  (else
		    (format ";~a~a" (exp->string (car exp-list)) (exps->string (cdr exp-list))))))))
              (exps->string exps))))
      (assign-exp (var exp1)
        (format "set ~a=~a" var (exp->string exp1)))
      (sum-exp (exp1 exp2)
        (format "+(~a,~a)" (exp->string exp1) (exp->string exp2)))
      (list-exp (exps)
	;;; 용규 수정 ;;;
	;;; (format "(~a)" ))
	(string-append "list " "(" (rands-expression exps) ")"))
;;class관련
      (new-object-exp (class-name rands)
        (string-append "new " (symbol->string class-name) "(" (rands-expression rands) ")"))
      (method-call-exp (obj-exp method-name rands)
        (string-append "send " (exp->string obj-exp) " " (symbol->string method-name) "(" (rands-expression rands) ")"))
      (self-exp ()
        (string-append "self"))
      (super-call-exp (method-name rands)
        (string-append "super " (symbol->string method-name) "(" (rands-expression rands) ")"))
      (instanceof-exp (exp name)
        (string-append "instanceof " (exp->string exp) (symbol->string name)))
      (cast-exp (obj-exp c-name)
        (string-append "cast " (exp->string obj-exp) (symbol->string c-name)))
      (else
        (eopl:error 'exp->string "arg=~a" exp)))))
;; ======================================================================

(define type->string
  (lambda (ty)
    (cases type ty
      (int-type () "int")
      (bool-type () "bool")
      (string-type () "string")
      (proc-type (arg-types result-type)
       (string-append "(" (args-type arg-types) ")" "->" (type->string result-type)))
      (void-type () "void")
      (class-type (class-name)
       (string-append (symbol->string class-name) " "))
      (list-type (type1)
       (string-append "listof " (type->string type1))))))


;; ======================================================================

;; ({Type}+(,)을 구현하는 args-type
(define args-type       
  (lambda (args)
    (if (null? args)
      ""
      (if (null? (cdr args))
        (string-append (type->string (car args)))
        (string-append (type->string (car args)) "," (args-type (cdr args)))))))
        
        
        
;; ({Expression}*) 구현하는 rands-expression
(define rands-expression
 (lambda (rands)
  (if (null? rands)
    ""
   (if (null? (cdr rands))
    (string-append (exp->string (car rands)))
    (string-append (exp->string (car rands)) "," (rands-expression (cdr rands)))))))

;; let {Identifier = Expression}* in Expression 에서 Identifier = Expression을 여러게 표현하기 위해 구현한 let->exp 함수

(define let->exp
 (lambda (vars exps)
  (if (null? vars)
   ""
   (if (null? (cdr vars))
    (string-append (symbol->string (car vars)) " = " (exp->string (car exps)))
    (string-append (symbol->string (car vars)) " = " (exp->string (car exps)) " " (let->exp (cdr vars) (cdr exps)))))))
