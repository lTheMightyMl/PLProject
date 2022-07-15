#lang racket


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
     ((Statements) (list `statements $1)))
    
    (Statements
     ((Statement semicolon) (append $1))
     ((Statements Statement semicolon) (list $1 $2)))
    
    (Statement
     ((Compound_stmt) (list `a-compound-stmt $1))
     ((Simple_stmt) (list `a-simple-stmt $1)))
    
    (Simple_stmt
     ((Assignment) (list `a-assignment $1))
     ((Global_stmt) (list `a-global-stmt $1))
     ((Return_stmt) (list `a-return-stmt $1))
     ((pass) (list `pass))
     ((break) (list `break))
     ((continue) (list `continue)))

    (Compound_stmt
     ((Function_def) (list `a-function-def $1))
     ((If_stmt) (list `a-if-stmt $1))
     ((For_stmt) (list `a-for-stmt $1)))

    (Assignment
     ((ID equal-sign Expression) (list `assign $1 $3)))

    (Return_stmt
     ((return) (list `return_void))
     ((return Expression) (list `return_value $2)))

    (Global_stmt
     ((global ID) (list `define-global $2)))

    (Function_def
     ((def ID lpar Params rpar colon Statements) (list `define-function-with-params $2 (list `params $4) $7))
     ((def ID empty-params colon Statements) (list `define-function-without-params $2 $5)))

    (Params
     ((Param_with_default) (append $1))
     ((Params comma Param_with_default) (list $1 $3)))

    (Param_with_default
     ((ID equal-sign Expression) (list `assign $1 $3)))

    (If_stmt
     ((if Expression colon Statements Else_block) (list `if $2 $4 $5)))

    (Else_block
     ((else colon Statements) (list `an-else-block $3)))

    (For_stmt
     ((for ID in Expression colon Statements) (list `for $2 $4 $6)))

    (Expression
     ((Disjunction) (list `a-disjunction $1)))

    (Disjunction
     ((Conjunction) (list `a-conjunction $1))
     ((Disjunction or Conjunction) (list `or $1 $3)))

    (Conjunction
     ((Inversion) (list `an-inversion $1))
     ((Conjunction and Inversion) (list `and $1 $3)))

    (Inversion
     ((not Inversion) (list `not $2))
     ((Comparison) (list `a-comparison $1)))

    (Comparison
     ((Eq_Sum) (list `an-eq-sum $1))
     ((Lt_Sum) (list `a-lt-sum $1))
     ((Gt_Sum) (list `a-gt-sum $1))
     ((Sum) (list `a-sum $1)))

    (Eq_Sum
     ((Sum eq Sum) (list `compare-eq $1 $3)))

    (Lt_Sum
     ((Sum lt Sum) (list `compare-lt $1 $3)))

    (Gt_Sum
     ((Sum gt Sum) (list `compare-gt $1 $3)))

    (Sum
     ((Sum plus Term) (list `add $1 $3))
     ((Sum minus Term) (list `minus $1 $3))
     ((Term) (list `a-term $1)))

    (Term
     ((Term multiply Factor) (list `multiply $1 $3))
     ((Term divide Factor) (list `divide $1 $3))
     ((Factor) (list `a-factor $1)))

    (Factor
     ((plus Power) (list `plus $2))
     ((minus Power) (list `minus $2))
     ((Power) (list `a-power $1)))

    (Power
     ((Atom power Factor) (list `power $1 $3))
     ((Primary) (list `a-primary $1)))

    (Primary
     ((Atom) (list `an-atom $1))
     ((Primary lbra Expression rbra) (list `get-index $1 $3))
     ((Primary empty-params) (list `call-function-with-no-argument $1))
     ((Primary lpar Arguments rpar) (list `call-function-with-arguments $1 (list `arguments $3))))

    (Arguments
     ((Expression) (append $1))
     ((Arguments comma Expression) (list $1 $3)))

    (Atom
     ((ID) (list `an-id $1))
     ((true) (list `true #t))
     ((false) (list `false #f))
     ((none) (list `none null))
     ((NUMBER) (list `a-number $1))
     ((List) (list `a-list $1)))

    (List
     ((lbra Expressions rbra) (list `expressions $2))
     ((empty-list) (list `empty-list `())))

    (Expressions
     ((Expression) (append $1))
     ((Expressions comma Expression) (list $1 $3)))

    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define your-lexer (lex-this simple-python-lexer (open-input-string "a=1; b=2;")))

(simple-python-parser your-lexer)

