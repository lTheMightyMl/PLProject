(module data-structures racket

  (require (lib "eopl.ss" "eopl"))
  (require "store.rkt")

  (provide (all-defined-out))

  (define-datatype python-exp python-exp?
    (pass)
    (break)
    (continue)
    (number (num number?))
    (identifier (id identifier?))
    (assign (id string?) (val python-exp?))
    (return-void)
    (return-value (exp python-exp?))
    (define-global (exp python-exp?))
    (define-function-with-params (name python-exp?) (params list?) (body python-exp?))
    (define-function-without-params (name python-exp?) (body python-exp?))
    (if (conditions python-exp?) (body python-exp?) (else python-exp?))
    (for (counter string?) (range python-exp?) (body python-exp?))
    (or (arg1 boolean?) (arg2 boolean?))
    (and (arg1 boolean?) (arg2 boolean?))
    (not (arg boolean?))
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
    (get-index (id string?) (index python-exp?))
    (call-function-with-no-argument (id string?))
    (call-function-with-arguments (id string?) (args list?)))

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
     (bvar symbol?)
     (bval reference?)                 ; new for implicit-refs
     (saved-env environment?))
    (extend-env-rec*
     (proc-names (list-of symbol?))
     (b-vars (list-of symbol?))
     (proc-bodies (list-of python-exp?))
     (saved-env environment?)))

  (define-datatype proc proc?
    (procedure
     (bvar symbol?)
     (body python-exp?)
     (env environment?)))

  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val 
     (proc proc?))
    (ref-val
     (ref reference?))
    )

  )