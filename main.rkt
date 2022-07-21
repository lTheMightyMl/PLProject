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
    (for (id expression statements)
      (begin
        (define list (expval->list (value-of expression env is-global)))
        (null? list
               (list `() env #f)
               (handle-for id list statements))))
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
             (handle-print atom)
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
checked
def f(n: int = 0) -> int:
    a: int = n;
    b: int = n + 1;
    return a ** b;
;

def g() -> bool:
    c :bool = 1 < 7 and 13 > 17 or 1 == 1;
    print(c);
    return c;
;

l: list = [1, 3, 5, 7];
a: int = 0;

if g():
    a = l[3];
else:
    a = l[2];
;
print(a);
")))

(simple-python-parser your-lexer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define handle-print
  (lambda (atom)
    (if
     (list? atom)
     (displayln (expval->printable atom))
     (begin
       (displayln (expval->printable (car atom)))
       (if (null? (cdr atom)) '() (handle-print (cdr atom)))
       ))))

(define handle-for
  (lambda (id list statements old-env)
    (define new-step (value-of statements (extend-env id (car list) old-env)))
    (define step-val (car new-step))
    (define new-env (cadr new-step))
    (define flag (caddr new-step))
    (if (or (not (zero? flag)) (null? list))
        new-step
        (handle-for id (cdr list) statements new-env))
    ))