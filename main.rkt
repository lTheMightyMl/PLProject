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
    (call-function-with-no-argument (name)
                                    (list (value-of (proc->body (expval->proc (apply-env global-scope (identifier->id-symbol name)))) env is-global) env is-global))
    (for (id expression statements)
      (begin
        (define lis (expval->list (value-of expression env is-global)))
        (if (null? lis)
            (list `() env 0)
            (handle-for id lis statements))))
    (or (arg1 arg2)
        (begin
          (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
          (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
          (list (bool-val (or arg1-bool-val arg2-bool-val) env is-global
                          ))))
    (and (arg1 arg2)
         (begin
           (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
           (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
           (list (bool-val (and arg1-bool-val arg2-bool-val)) env is-global
                 )))
    (not (arg)
         (begin
           (define arg-bool-val (expval->bool (car (value-of arg env is-global))))
           (list (bool-val (not arg-bool-val)) env is-global)
           ))
    (compare-eq (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (equal? arg1-num-val arg2-num-val)) env is-global)
                  ))
    (compare-lt (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (< arg1-num-val arg2-num-val)) env is-global)
                  ))
    (compare-gt (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (> arg1-num-val arg2-num-val)) env is-global)
                  ))
    (print (atom)
           (begin
             (handle-print atom)
             (list `() env #f)))


    ; Sadegh:
    (statements (stmts stmt)
                (begin
                  (define ret-val (value-of stmts env is-global))
                  (define ret-env (cadr ret-val))
                  (define is-ret (caddr ret-val))
                  (case is-ret
                    ((0) (value-of stmt ret-env is-global))
                    ((1) (list '() ret-env 1))
                    ((2) (list '() ret-env 0))
                    )))
    (pass () (list '() env is-global 0))
    (break () (list '() env is-global 1))
    (continue () (list '() env is-global 2))
    (assign (var val)
            (begin
              (define value (car (value-of val env is-global)))
              (cond
                (is-global (set! global-scope (extend-env (string->symbol var) value global-scope)))
                (else '())
                )
              (list '() (extend-env (string->symbol var) value env) 0)
              ))
    (python-list (exps)
                 (list (list-val (car (value-of exps env is-global))) env 0))
    (single-expression (exp)
                       (list (list (car (value-of exp env is-global))) env 0))
    (multi-expression (exps exp)
                      (list (append (car (value-of exps env is-global)) (list (car (value-of exp env is-global)))) env 0))
    (return-void () (list '() env 1))
    (return-value (val)
                  (begin
                    (define value (car (value-of val env is-global)))
                    (set! return-stack (cons value return-stack))
                    (list '() env 1)
                    ))
    (define-global (var) (list '() (extend-env var (apply-env global-scope) env) #f))
    (define-function-with-params (p-name params p-def)
      (begin
        (define value (proc-val (procedure-with-params (identifier->id-symbol p-name) params p-def)))
        (cond
          (is-global (set! global-scope (extend-env (identifier->id-symbol p-name) value global-scope)))
          (else '())
          )
        (list '() (extend-env (identifier->id-symbol p-name) value env) 0)
        ))
    (define-function-without-params (p-name p-def)
      (begin
        (define value (proc-val (procedure-without-params (identifier->id-symbol p-name) p-def)))
        (cond
          (is-global (set! global-scope (extend-env (identifier->id-symbol p-name) value global-scope)))
          (else '())
          )
        (list '() (extend-env (identifier->id-symbol p-name) value env) 0)
        ))


    ; Namdar :
    ;      (compare-eq (arg1 arg2)
    ;                (let ([val1 (car (value-of arg1 env is-global))]
    ;                      [val2 (car (value-of arg2 env is-global))])
    ;                      (list (if (eqv? (expval->num val1) (expval->num val2))
    ;                        (bool-val #t) (bool-val #f)) env #f)));
    ;
    ;    (compare-lt (arg1 arg2)
    ;                (let ([val1 (car (value-of arg1 env is-global))]
    ;                      [val2 (car (value-of arg2 env is-global))])
    ;                      (list (if (< (expval->num val1) (expval->num val2))
    ;                        (bool-val #t) (bool-val #f)) env #f)))
    ;                        
    ;    (compare-gt (arg1 arg2)
    ;                (let ([val1 (car (value-of arg1 env is-global))]
    ;                      [val2 (car (value-of arg2 env is-global))])
    ;                      (list (if (> (expval->num val1) (expval->num val2))
    ;                        (bool-val #t) (bool-val #f)) env #f)))

    (add  (arg1 arg2)
          (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                [val2 (expval->num (car (value-of arg2 env is-global)))])
            (list (num-val (+ val1 val2)) env 0)))
                
    (subtract (arg1 arg2)
              (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                    [val2 (expval->num (car (value-of arg2 env is-global)))])
                (list (num-val (- val1 val2)) env 0)))
    (power  (arg1 arg2)
            (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                  [val2 (expval->num (car (value-of arg2 env is-global)))])
              (list (num-val (expt val1 val2)) env 0)))
    (multiply (arg1 arg2)
              (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                    [val2 (expval->num (car (value-of arg2 env is-global)))])
                (list (num-val (* val1 val2)) env 0)))
    (divide (arg1 arg2)
            (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                  [val2 (expval->num (car (value-of arg2 env is-global)))])
              (list (num-val (/ val1 val2)) env 0)))

    (plus (arg) (let ([val (car (value-of arg env is-global))])
                  (list val env 0)))

    (minus  (arg) (let ([val (expval->num (car (value-of arg env is-global)))])
                    (list (num-val(- 0 val)) env 0)))
    (number (num)
            (list num env 0))

    (else (begin
            (pretty-print "here")
            (pretty-print exp)))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "
def f(n = 0):
    a = n;
    b = n + 1;
    return a ** b;
;

def g():
    c = 1 < 7 and 13 > 17 or 1 == 1;
    print(c);
    return c;
;

l = [1, 3, 5, 7];

if g():
    h = f;
    d = h(3);
    print(d);
else:
    print(0);
;

x = 0;
if False:
    x = l[2];
else:
    x = l[3];
;
print(x);
")))

(value-of (simple-python-parser your-lexer) (empty-env) #t)

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
