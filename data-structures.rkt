(module data-structures racket

  (require "store.rkt")
  (require eopl)

  (provide (all-defined-out))
  (provide (all-from-out eopl))

  (define-datatype python-exp python-exp?
    (statements (statements python-exp?) (statement python-exp?))
    (single-param (param python-exp?))
    (params (params python-exp?) (param python-exp?))
    (args (args python-exp?) (arg python-exp?))
    (single-expression (exp python-exp?))
    (multi-expression (expressions python-exp?) (expression python-exp?))
    (atoms (atoms python-exp?) (atom python-exp?))
    (pass)
    (break)
    (continue)
    (number (num number?))
    (identifier (id symbol?))
    (boolean (bool boolean?))
    (assign (id string?) (val python-exp?))
    (assign-def (id string?) (val python-exp?))
    (return-void)
    (return-value (exp python-exp?))
    (define-global (exp python-exp?))
    (define-function-with-params (name python-exp?) (params python-exp?) (body python-exp?))
    (define-function-without-params (name python-exp?) (body python-exp?))
    (if-dt (conditions python-exp?) (body python-exp?) (else python-exp?))
    (for-dt (counter string?) (range python-exp?) (body python-exp?))
    (or-dt (arg1 python-exp?) (arg2 python-exp?))
    (and-dt (arg1 python-exp?) (arg2 python-exp?))
    (not-dt (arg python-exp?))
    (compare-eq (arg1 python-exp?) (arg2 python-exp?))
    (compare-lt (arg1 python-exp?) (arg2 python-exp?))
    (compare-gt (arg1 python-exp?) (arg2 python-exp?))
    (add (arg1 python-exp?) (arg2 python-exp?))
    (subtract (arg1 python-exp?) (arg2 python-exp?))
    (power (arg1 python-exp?) (arg2 python-exp?))
    (multiply (arg1 python-exp?) (arg2 python-exp?))
    (divide (arg1 python-exp?) (arg2 python-exp?))
    (plus (arg python-exp?))
    (minus (arg python-exp?))
    (get-index (id python-exp?) (index python-exp?))
    (call-function-with-no-argument (id python-exp?))
    (call-function-with-arguments (id python-exp?) (args python-exp?))
    (id-atom (id symbol?))
    (none)
    (python-list (exps python-exp?))
    (empty-list)
    (print (atom python-exp?))
    (single-arg (exp python-exp?)))

  (define (identifier->id-symbol x)
    (cases python-exp x
      (identifier (id) id)
      (else '()))
    )

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
     (bvar symbol?)
     (bval expval?)                 ; new for implicit-refs
     (saved-env environment?))
    (extend-env-rec*
     (proc-name symbol?)
     (proc-def expval?)
     (saved-env environment?)))

  (define-datatype proc proc?
    (procedure-with-params
     (name symbol?)
     (params python-exp?)
     (body python-exp?))
    (procedure-without-params
     (name symbol?)
     (body python-exp?)))

  (define (proc->body p)
    (cases proc p
      (procedure-with-params (name params body)
                             body)
      (procedure-without-params (name body)
                                body)
      ))

  (define-datatype expval expval?
    (num-val
     (num number?))
    (bool-val
     (bool boolean?))
    (list-val
     (lis list?))
    (none-val
     (none null?))
    (proc-val
     (p proc?))
    )

  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))


  (define expval->list
    (lambda (v)
      (cases expval v
        (list-val (lis) lis)
        (else (expval-extractor-error 'list v)))))

  (define expval->none
    (lambda (v)
      (cases expval v
        (none-val (none) none)
        (else (expval-extractor-error 'none v)))))

  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  (define expval->printable
    (lambda (exp)
      (cases expval exp
        (num-val (val) val)
        (bool-val (val) val)
        (list-val (val)
                  (cond
                    ((null? val) '())
                    (else (cons (expval->printable (car val)) (expval->printable (list-val (cdr val)))))
                    ))
        (none-val (val) val)
        (else `error)
        )))
  
  )