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
    (else '())))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "if a: a = 2; else: b = 2;;")))

(simple-python-parser your-lexer)
