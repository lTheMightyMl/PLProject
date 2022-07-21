(module environments (lib "eopl.ss" "eopl")
  
  (require "data-structures.rkt")
  (require "store.rkt")
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
                   (eopl:error 'apply-env "Empty stack"))
        (extend-env (bvar bval saved-env)
                    (if (eqv? search-var bvar)
                        bval
                        (apply-env saved-env search-var)))
        (extend-env-rec* (p-name p-def saved-env)
                         (if (eqv? search-var p-name)
                             p-def
                             (apply-env saved-env search-var))))))

  ;; location : Sym * Listof(Sym) -> Maybe(Int)
  ;; (location sym syms) returns the location of sym in syms or #f is
  ;; sym is not in syms.  We can specify this as follows:
  ;; if (memv sym syms)
  ;;   then (list-ref syms (location sym syms)) = sym
  ;;   else (location sym syms) = #f
  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms))
         => (lambda (n) 
              (+ n 1)))
        (else #f))))

  )