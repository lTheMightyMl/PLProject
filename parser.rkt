(module parser racket

  (require parser-tools/yacc)
  (require "data-structures.rkt")
  (require "lexer.rkt")

  (provide (all-defined-out))
  

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
     ((Statement semicolon) $1)
     ((Statements Statement semicolon) (statements $1 $2)))
    
    (Statement
     ((Compound_stmt) $1)
     ((Simple_stmt) $1))
    
    (Simple_stmt
     ((Assignment) $1)
     ((Global_stmt) $1)
     ((Return_stmt) $1)
     ((pass) (pass))
     ((break) (break))
     ((continue) (continue))
     ((Print) $1))

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
     ((Param_with_default) $1)
     ((Params comma Param_with_default) (params $1 $3)))

    (Param_with_default
     ((ID equal-sign Expression) (assign-def $1 $3)))

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
     ((Expression) $1)
     ((Arguments comma Expression) (args $1 $3)))
    
    (Atom
     ((ID) (identifier (string->symbol $1)))
     ((true) (boolean #t))
     ((false) (boolean #f))
     ((none) (none))
     ((NUMBER) (number $1))
     ((List) (list-atom $1)))

    (List
     ((lbra Expressions rbra) (python-list $2))
     ((empty-list) (empty-list)))

    (Expressions
     ((Expression) $1)
     ((Expressions comma Expression) (expressions $1 $3)))

    (Print
     ((print lpar Atom rpar) (print $3)))
    
    )))
)
