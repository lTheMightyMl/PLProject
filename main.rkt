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
(define globals '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define handle-print
  (lambda (atom)
    (cond
      ((null? atom) (newline))
      ((list? atom) (handle-print (car atom)) (handle-print (cdr atom)))
      (else (displayln (expval->printable atom)))
      )))

(define handle-for
  (lambda (id lis statements old-env is-global)
    (define iter-env (assign-helper id (car lis) old-env is-global))
    (define new-step (value-of statements iter-env is-global))
    (define step-val (car new-step))
    (define new-env (cadr new-step))
    (define flag (caddr new-step))
    (if (null? (cdr lis))
        (list step-val new-env 0)
        (case flag
          ((0 2) (handle-for id (cdr lis) statements new-env is-global))
          (else (list step-val new-env 0))
          ))
    ))

(define (assign-helper var value env is-global)
  (cond
    (is-global (set! global-scope (extend-env var value global-scope)))
    ((member var (car globals)) (set! global-scope (extend-env var value global-scope)))
    )
  (extend-env var value env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value-of exp env is-global)
  (cases python-exp exp

    (boolean (b)
             (list (bool-val b) env 0)
             )

    ; Hashem:
    (if-dt (condition body else-body)
           (cond
             ((expval->bool (car (value-of condition env is-global))) (value-of body env is-global))
             (else (value-of else-body env is-global))
             ))
    
    (for-dt (id expression statements)
            (define lis (expval->list (car (value-of expression env is-global))))
            (if (null? list)
                (list `() env 0)
                (handle-for (string->symbol id) lis statements env is-global)
                ))
    
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

    (get-index (arr-name index-number)
               (begin
                 (define arr (expval->list (car (value-of arr-name env is-global))))
                 (list (list-ref arr (expval->num (car (value-of index-number env is-global)))) env 0)
                 ))


    ; Sadegh:
    (statements (stmts stmt)
                (begin
                  (define ret-val (value-of stmts env is-global))
                  (define ret-env (cadr ret-val))
                  (define is-ret (caddr ret-val))
                  (case is-ret
                    ((0) (value-of stmt ret-env is-global))
                    ((1) (list '() ret-env 1))
                    ((2) (list '() ret-env 2))
                    )))

    (pass () (list '() env 0))

    (break () (list '() env 1))

    (continue () (list '() env 2))

    (assign (var val)
            (begin
              (define value (thunk-val ( delay (car (value-of val env is-global)))))
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
                   (set! return-stack (cons (none-val) return-stack))
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

    (assign-def (id exp)
                (list (list (string->symbol id) (car (value-of exp (empty-env) #f))) (empty-env) 0)
                )

    (single-param (param)
                  (list (list (car (value-of param (empty-env) #f))) (empty-env) 0)
                  )

    (params (params param)
            (list (append (car (value-of params (empty-env) #f)) (list (car (value-of param (empty-env) #f)))) (empty-env) 0)
            )

    ; (call-function-with-no-argument (name)
    ;                                 (begin
    ;                                   (set! globals (list '() globals))
    ;                                   (value-of (proc->body (expval->proc (car (value-of name env is-global)))) (extend-env (identifier->id-symbol name) (car (value-of name env is-global)) (empty-env)) #f)
    ;                                   (define res-val (car return-stack))
    ;                                   (set! return-stack (cdr return-stack))
    ;                                   (set! globals (cadr globals))
    ;                                   (list res-val env 0))
    ;                                 )


    ; Namdar :
    (call-function-with-no-argument (name) 
                                    (let ([func (expval->proc (car (value-of name env is-global)))])
                                      (apply-function func '())))
    (call-function-with-arguments (name args)
                                  (let ([func (expval->proc (car (value-of name env is-global)))]
                                        [args (car (value-of args env is-global))])
                                    (apply-function func args)
                                  ))
    (args (args arg)
          (list (append (car (value-of args env is-global)) (list (thunk-val (delay(car (value-of arg env is-global))))) env 0)))

    (single-arg (exp)
                (list (list (thunk-val (delay (car (value-of exp env is-global)))) env 0))
                )
    
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
              (let ([val1 (expval->num (car (value-of arg1 env is-global)))])
                (if (zero? val1)
                    (list (num-val 0) env 0)
                    (let ([val2 (expval->num (car (value-of arg2 env is-global)))])
                      (list (num-val (* val1 val2)) env 0)))))
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
            (pretty-print exp))))
)

(define (apply-function func args)
  (cases proc func
    (procedure-without-params (name body)
                              (begin
                                (set! globals (list '() globals))
                                (value-of body (extend-env name (proc-val func) (empty-env)) #f)
                                (define res-val (car return-stack))
                                (set! return-stack (cdr return-stack))
                                (set! globals (cadr globals))
                                (list res-val (empty-env) 0)
                                ))
    (procedure-with-params (name params body) 
                             (begin
                                (set! globals (list '() globals))
                                (define par-val (car (value-of params (empty-env) #f)))
                                (value-of body (extend-env name (proc-val func) (init-env-func par-val args (empty-env))) #f)
                                (define res-val (car return-stack))
                                (set! return-stack (cdr return-stack))
                                (set! globals (cadr globals))
                                (list res-val (empty-env) 0)
                                ))

    (else 'Error))
  )

  (define (init-env-func defaults args env)
    (cond
      [(null? defaults) env]
      [(null? args) (init-env-func (cdr defaults) null
                                    (extend-env (car(car defaults)) (cadr(car defaults)) env))]
      [else (init-env-func (cdr defaults) (cdr args)
                            (extend-env (car(car defaults)) (car args) env))]
      ))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "
if True:
    b = 7;
    print(b);
else:
    print(False);
;
")))

;(simple-python-parser your-lexer)

;(value-of (simple-python-parser your-lexer) (empty-env) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lexer-creator file-name)
  (lex-this simple-python-lexer (open-input-file file-name)))

(define (evaluate file-name)
  (value-of (simple-python-parser (lexer-creator file-name)) (empty-env) #t))

; (simple-python-parser (lexer-creator "a.txt"))

(evaluate "a.txt")
