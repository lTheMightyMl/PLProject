(module environments (lib "eopl.ss" "eopl")
  
  (require "data-structures.rkt")
  (require racket/promise)
  (provide init-env empty-env extend-env apply-env)

  ;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; (init-env) builds an empty environment  
  (define init-env 
    (lambda ()
      (empty-env)))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define apply-env
    (lambda (env search-var)
      (cases environment env
        (empty-env ()
                   (eopl:error 'apply-env "Empty env ~s" search-var))
        (extend-env (bvar bval saved-env)
                    (cond
                      ((eq? search-var bvar)
                       (cases expval bval
                         (thunk-val (th)
                                    (if (promise-running? th)
                                        (apply-env saved-env search-var)
                                        bval
                                        ))
                         (else bval)
                         )
                       )
                      (else (apply-env saved-env search-var))))
        )))

  )