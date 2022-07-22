#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "environments.rkt")
(require "data-structures.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define global-scope '())
(define return-stack '())

(define (value-of exp env is-global)
  (cases python-exp exp
    (statements (stmts stmt)
                (value-of (value-of stmt) env))
    (compare-eq (arg1 arg2)
                (let ([val1 (car (value-of arg1 env is-global))]
                      [val2 (car (value-of arg2 env is-global))])
                      (list (if (eqv? (expval->num val1) (expval->num val2))
                        (bool-val #t) (bool-val #f)) env #f)))

    (compare-lt (arg1 arg2)
                (let ([val1 (car (value-of arg1 env is-global))]
                      [val2 (car (value-of arg2 env is-global))])
                      (list (if (< (expval->num val1) (expval->num val2))
                        (bool-val #t) (bool-val #f)) env #f)))
                        
    (compare-gt (arg1 arg2)
                (let ([val1 (car (value-of arg1 env is-global))]
                      [val2 (car (value-of arg2 env is-global))])
                      (list (if (> (expval->num val1) (expval->num val2))
                        (bool-val #t) (bool-val #f)) env #f)))

    (add  (arg1 arg2)
          (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                [val2 (expval->num (car (value-of arg2 env is-global)))])
                (list (num-val (+ val1 val2)) env #f)))
                
    (subtract (arg1 arg2)
              (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                    [val2 (expval->num (car (value-of arg2 env is-global)))])
                    (list (num-val (- val1 val2)) env #f)))
    (power  (arg1 arg2)
            (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                  [val2 (expval->num (car (value-of arg2 env is-global)))])
                  (list (num-val (expt val1 val2)) env #f)))
    (multiply (arg1 arg2)
              (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                    [val2 (expval->num (car (value-of arg2 env is-global)))])
                    (list (num-val (* val1 val2)) env #f)))
    (divide (arg1 arg2)
            (let ([val1 (expval->num (car (value-of arg1 env is-global)))]
                  [val2 (expval->num (car (value-of arg2 env is-global)))])
                  (list (num-val (/ val1 val2)) env #f)))

    (plus (arg) (let ([val (car (value-of arg env is-global))])
                  (list val env #f)))

    (minus  (arg) (let ([val (expval->num (car (value-of arg env is-global)))])
                  (list (num-val(- 0 val)) env #f)))
    

    (else '())))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "if a: a = 2; else: b = 2;;")))

(simple-python-parser your-lexer)
