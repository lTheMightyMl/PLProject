(module data-structures racket

  (require "store.rkt")
  (require eopl)

  (provide (all-defined-out))
  (provide (all-from-out eopl))

  (define-datatype python-exp python-exp?
    (statements (statements python-exp?) (statement python-exp?))
    (params (params python-exp?) (param python-exp?))
    (args (args python-exp?) (arg python-exp?))
    (expressions (expressions python-exp?) (expression python-exp?))
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
    (if (conditions python-exp?) (body python-exp?) (else python-exp?))
    (for (counter string?) (range python-exp?) (body python-exp?))
    (or (arg1 python-exp?) (arg2 python-exp?))
    (and (arg1 python-exp?) (arg2 python-exp?))
    (not (arg python-exp?))
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
    (list-atom (lis python-exp?))
    (python-list (exps python-exp?))
    (empty-list)
    (print (atom python-exp?)))

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
    (lambda exp
      (cases expval exp
        (num-val (val) (expval->num val))
        (bool-val (val) (expval->bool val))
        (list-val (val) (expval->list val))
        (none-val (val) (expval->none val))
        (else `error)
        )))
  
  )