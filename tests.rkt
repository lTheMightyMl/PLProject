(define your-lexer (lex-this simple-python-lexer (open-input-string "def f():
    global a;
    a = a + 1;
    ;
a = 2;
print(a);
b = f();
print(a);")))

(define my-lexer (lex-this simple-python-lexer (open-input-string "def f():
     global a;
     a = a + 1;
     ;
 b = f();")))


(define his-lexer (lex-this simple-python-lexer (open-input-string "def add_one(n):
    return n + 1;
;

def f(n):
    a = add_one(n);
    b = print(a);
;
c = f(7);")))

