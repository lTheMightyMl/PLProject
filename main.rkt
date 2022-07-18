#lang racket

(require (lib "eopl.ss" "eopl"))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-python-lexer
  (lexer
   ((eof) (token-EOF))
   (";" (token-semicolon))
   ("pass" (token-pass))
   ("break" (token-break))
   ("continue" (token-continue))
   ("equal" (token-equal-sign))
   ("return" (token-return))
   ("global" (token-global))
   ("def" (token-def))
   ("(" (token-lpar))
   (")" (token-rpar))
   (":" (token-colon))
   ("():" (token-empty-params))
   ("," (token-comma))
   ("if" (token-if))
   ("else" (token-else))
   ("for" (token-for))
   ("in" (token-in))
   ("or" (token-or))
   ("and" (token-and))
   ("not" (token-not))
   ("=" (token-equal-sign))
   ("==" (token-eq))
   ("<" (token-lt))
   (">" (token-gt))
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-multiply))
   ("/" (token-divide))
   ("**" (token-power))
   ("[" (token-lbra))
   ("]" (token-rbra))
   ("()" (token-empty-args))
   ("True" (token-true))
   ("False" (token-false))
   ("None" (token-none))
   ("[]" (token-empty-list))
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUMBER (string->number lexeme)))
   ((:: (:+ (:or #\_ (char-range #\a #\z) (char-range #\A #\Z))) (:* (:or #\_ (char-range #\0 #\9) (char-range #\a #\z) (char-range #\A #\Z))))
    (token-ID lexeme))
   (whitespace (simple-python-lexer input-port))))

(define string->variable-name
   (lambda (stx) #'stx))

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

(define-tokens a (NUMBER ID))
(define-empty-tokens b (EOF semicolon pass break continue equal-sign return global def rpar lpar colon empty-params comma if else for in or and not eq lt gt plus minus multiply divide power lbra rbra empty-args true false none empty-list))

(define simple-python-parser
  (parser
   (start Program)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (Program
     ((Statements) $1))
    
    (Statements
     ((Statement semicolon) (list $1))
     ((Statements Statement semicolon) (cons $2 $1)))
    
    (Statement
     ((Compound_stmt) $1)
     ((Simple_stmt) $1))
    
    (Simple_stmt
     ((Assignment) $1)
     ((Global_stmt) $1)
     ((Return_stmt) $1)
     ((pass) (pass))
     ((break) (break))
     ((continue) (continue)))

    (Compound_stmt
     ((Function_def) $1)
     ((If_stmt) $1)
     ((For_stmt) $1))

    (Assignment
     ((ID equal-sign Expression) (assign $1 $3)))

    (Return_stmt
     ((return) (return-void))
     ((return Expression) (return-value $2)))

    (Global_stmt
     ((global ID) (define-global $2)))

    (Function_def
     ((def ID lpar Params rpar colon Statements) (define-function-with-params $2 $4 $7))
     ((def ID empty-params colon Statements) (define-function-without-params $2 $5)))

    (Params
     ((Param_with_default) (list $1))
     ((Params comma Param_with_default) (append $1 $3)))

    (Param_with_default
     ((ID equal-sign Expression) (assign $1 $3)))

    (If_stmt
     ((if Expression colon Statements Else_block) (if $2 $4 $5)))

    (Else_block
     ((else colon Statements) $3))

    (For_stmt
     ((for ID in Expression colon Statements) (for $2 $4 $6)))

    (Expression
     ((Disjunction) $1))

    (Disjunction
     ((Conjunction) $1)
     ((Disjunction or Conjunction) (or $1 $3)))

    (Conjunction
     ((Inversion) $1)
     ((Conjunction and Inversion) (and $1 $3)))

    (Inversion
     ((not Inversion) (not $2))
     ((Comparison) $1))

    (Comparison
     ((Eq_Sum) $1)
     ((Lt_Sum) $1)
     ((Gt_Sum) $1)
     ((Sum) $1))

    (Eq_Sum
     ((Sum eq Sum) (compare-eq $1 $3)))

    (Lt_Sum
     ((Sum lt Sum) (compare-lt $1 $3)))

    (Gt_Sum
     ((Sum gt Sum) (compare-gt $1 $3)))

    (Sum
     ((Sum plus Term) (add $1 $3))
     ((Sum minus Term) (subtract $1 $3))
     ((Term) $1))

    (Term
     ((Term multiply Factor) (multiply $1 $3))
     ((Term divide Factor) (divide $1 $3))
     ((Factor) $1))

    (Factor
     ((plus Power) (plus $2))
     ((minus Power) (minus $2))
     ((Power) $1))

    (Power
     ((Atom power Factor) (power $1 $3))
     ((Primary) $1))

    (Primary
     ((Atom) $1)
     ((Primary lbra Expression rbra) (get-index $1 $3))
     ((Primary empty-params) (call-function-with-no-argument $1))
     ((Primary lpar Arguments rpar) (call-function-with-arguments $1 $3)))

    (Arguments
     ((Expression) (list $1))
     ((Arguments comma Expression) (append $1 $3)))

    (Atom
     ((ID) (identifier (string->variable-name $1)))
     ((true) #t)
     ((false) #f)
     ((none) null)
     ((NUMBER) (number $1))
     ((List) $1))

    (List
     ((lbra Expressions rbra) $2)
     ((empty-list) `()))

    (Expressions
     ((Expression) (list $1))
     ((Expressions comma Expression) (append $1 $3)))

    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "a=1; b=2; c=3; d=4;")))

(simple-python-parser your-lexer)


