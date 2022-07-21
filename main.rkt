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

(define your-lexer (lex-this simple-python-lexer (open-input-string "if a: a = 2; else: b = 2;;")))

(simple-python-parser your-lexer)
