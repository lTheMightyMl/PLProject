#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "environments.rkt")
(require "data-structures.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define global-scope (empty-env))
(define return-stack '())

(define (value-of exp env is-global)
  (cases python-exp exp
    
    ; Hashem:
    (if (cond body else-body)
        (if
         (expval->bool (car (value-of cond env is-global)))
         (value-of body env is-global)
         (value-of else-body env is-global)))
    (for (counter expression statements)) ; todo
    (or (arg1 arg2)
        (begin
          (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
          (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
          (or arg1-bool-val arg2-bool-val)
          ))
    (and (arg1 arg2)
        (begin
          (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
          (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
          (and arg1-bool-val arg2-bool-val)
          ))
    (not (arg)
        (begin
          (define arg-bool-val (expval->bool (car (value-of arg env is-global))))
          (not arg-bool-val)))
    (compare-eq (arg1 arg2)
        (begin
          (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
          (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
          (equal? arg1-num-val arg2-num-val)
          ))
    (compare-lt (arg1 arg2)
        (begin
          (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
          (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
          (< arg1-num-val arg2-num-val)
          ))
    (compare-gt (arg1 arg2)
        (begin
          (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
          (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
          (> arg1-num-val arg2-num-val)
          ))
    (print (atom)
           (begin
             (print-handler atom)
             (list `() env #f)))


    ; Sadegh:
    (statements (stmts stmt)
                (begin
                  (define ret-val (value-of stmts env))
                  (define ret-env (cadr ret-val))
                  (define is-ret (caddr ret-val))
                  (if is-ret
                      (list '() ret-env #f)
                      (value-of stmt ret-env is-global)
                      )))
    (pass () (list '() env is-global #f))
    (break () (list '() env is-global #t))
    (assign (var val)
            (begin
              (define value (value-of val env is-global))
              (if is-global (set! global-scope (extend-env var value global-scope)))
              (list '() (extend-env var value env) #f)
              ))
    (return-void () (list '() env #t))
    (return-value (val)
                  (begin
                    (define value (car (value-of val env is-global)))
                    (set! return-stack (cons value return-stack))
                    (list '() env #t)
                    ))
    (define-global (var) (list '() (extend-env var (apply-env global-scope) env) #f))
    (define-function-with-params (p-name params p-def)
      (begin
        (define value (proc-val (procedure-with-params p-name params p-def)))
        (if is-global (set! global-scope (extend-env p-name value global-scope)))
        (list '() (extend-env p-name value env) #f)
        ))
    (define-function-without-params (p-name p-def)
      (begin
        (define value (proc-val (procedure-without-params p-name p-def)))
        (if is-global (set! global-scope (extend-env p-name value global-scope)))
        (list '() (extend-env p-name value env) #f)
        ))
    (else '())))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "
l = [1,2,3,4,5,6,7,8,9,10];
for a in l:
    if a<5:
       continue;
    else:
       if a>8:
          break;
       else:
          print(2);
       ;
    ;
;
")))

(simple-python-parser your-lexer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-handler
  (lambda (atom)
    (if
     (list? atom)
     (displayln (exp-val->printable atom))
     (begin
       (displayln (exp-val->printable (car atom)))
       (if (null? (cdr atom)) '() (print-handler (cdr atom)))
       ))))

; todo: exp-val->printable