#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "environments.rkt")
(require "data-structures.rkt")
(require "lexer.rkt")
(require "parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define handle-print
  (lambda (atom)
    (cond
      ((null? atom) (newline))
      ((list? atom) (handle-print (car atom)) (handle-print (cdr atom)))
      (else (displayln (expval->printable atom)))
      )))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-scope (empty-env))
(define return-stack '())
(define globals '())

(define (value-of exp env is-global)
  (cases python-exp exp

    (boolean (b)
             (list (bool-val b) env 0)
             )

    ; Hashem:
    (if (condition body else-body)
        (cond
         ((expval->bool (car (value-of condition env is-global))) (value-of body env is-global))
         (else (value-of else-body env is-global))
         ))
    
    (for (id expression statements)
      (begin
        (define lis (expval->list (value-of expression env is-global)))
        (if (null? lis)
            (list `() env 0)
            (handle-for id lis statements))))
    
    (or-dt (arg1 arg2)
           (begin
             (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
             (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
             (list (bool-val (or arg1-bool-val arg2-bool-val)) env 0)
             ))
    
    (and-dt (arg1 arg2)
            (begin
              (define arg1-bool-val (expval->bool (car (value-of arg1 env is-global))))
              (define arg2-bool-val (expval->bool (car (value-of arg2 env is-global))))
              (list (bool-val (and arg1-bool-val arg2-bool-val)) env 0)
              ))
    
    (not-dt (arg)
            (begin
              (define arg-bool-val (expval->bool (car (value-of arg env is-global))))
              (list (bool-val (not arg-bool-val)) env 0)
              ))
    
    (compare-eq (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (equal? arg1-num-val arg2-num-val)) env 0)
                  ))
    
    (compare-lt (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (< arg1-num-val arg2-num-val)) env 0)
                  ))
    
    (compare-gt (arg1 arg2)
                (begin
                  (define arg1-num-val (expval->num (car (value-of arg1 env is-global))))
                  (define arg2-num-val (expval->num (car (value-of arg2 env is-global))))
                  (list (bool-val (> arg1-num-val arg2-num-val)) env 0)
                  ))
    
    (print (exps)
           (begin
             (handle-print (expval->list (car (value-of exps env is-global))))
             (list `() env 0)))


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
                ((member (string->symbol var) (car globals)) (set! global-scope (extend-env (string->symbol var) value global-scope)))
                )
              (list '() (extend-env (string->symbol var) value env) 0)
              ))

    (python-list (exps)
                 (list (car (value-of exps env is-global)) env 0))

    (single-expression (exp)
                       (list (list-val (list (car (value-of exp env is-global)))) env 0))

    (multi-expression (exps exp)
                      (list (list-val (append (expval->list (car (value-of exps env is-global))) (list (car (value-of exp env is-global))))) env 0))

    (return-void ()
                 (begin
                   (set! return-stack (cons (none-val '()) return-stack))
                   (list '() env 1)
                 ))

    (identifier (name)
                (cond
                  (is-global (list (apply-env global-scope name) env 0))
                  (else (list (apply-env env name) env 0))
                ))

    (return-value (val)
                  (begin
                    (define value (car (value-of val env is-global)))
                    (set! return-stack (cons value return-stack))
                    (list '() env 1)
                    ))
    
    (define-global (var)
      (begin
        (set! globals (list (cons (identifier->id-symbol var) (car globals)) (cadr globals)))
        (list '() (extend-env (identifier->id-symbol var) (apply-env global-scope (identifier->id-symbol var)) env) 0))
      )

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

    (call-function-with-no-argument (name)
                                    (begin
                                      (set! globals (list '() globals))
                                      (value-of (proc->body (expval->proc (car (value-of name env is-global)))) (extend-env (identifier->id-symbol name) (car (value-of name env is-global)) (empty-env)) #f)
                                      (define res-val (car return-stack))
                                      (set! return-stack (cdr return-stack))
                                      (set! globals (cadr globals))
                                      (list res-val env 0))
                                    )



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
            (list (num-val num) env 0))

    (else (begin
            (pretty-print "here")
            (pretty-print exp)))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "
def g():
    global b;
    if b < 10:
        print(True);
        return b + 8;
    else:
        print(False);
        b = b - 7;
        return g();
    ;
;
b = 10;
print(g());
")))

;(simple-python-parser your-lexer)

(value-of (simple-python-parser your-lexer) (empty-env) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
