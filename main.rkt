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
    (if (cond body else-body)
        (if
         cond
         (value-of body env)
         (value-of else-body env)))
    (print (atom)
           (begin
             (print-handler atom)
             (list `() env #f)))
    (else '())))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "
l = [1,2,3,4,5,6,7,8,9,10];
for a in l:
    if a<5:
       continue;
    else:
       if a>8:
          break;
       else:
          print(2);
       ;
    ;
;
")))

(simple-python-parser your-lexer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-handler
  (lambda (atom)
    (if
     (list? atom)
     (displayln (exp-val->printable atom))
     (begin
       (displayln (exp-val->printable (car atom)))
       (if (null? (cdr atom)) '() (print-handler (cdr atom)))
       ))))

; todo: exp-val->printable