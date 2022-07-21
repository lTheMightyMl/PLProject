#lang racket

;(require (lib "eopl.ss" "eopl"))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "environments.rkt")
(require "data-structures.rkt")
(require "lexer.rkt")
(require "parser.rkt")

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "a=1; b=2; c=3; d=4;")))

(simple-python-parser your-lexer)


