#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "environments.rkt")
(require "data-structures.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define (value-of exp env)
  (cases python-exp exp
    (else '())))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "a=1; b=2; c=3; d=4;")))

(simple-python-parser your-lexer)


