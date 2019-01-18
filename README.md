# Lisp interpreter

## Syntax

```lisp
; line comment
#| block comment |#
true                ; bool
"abc"               ; string
1                   ; integer
foo                 ; atom
'(1 2 3)            ; list
(foo bar baz)       ; s-expression
(lambda (x) (x))    ; lambda function
(define foo true)   ; variable definition
(let 
  (foo true bar false) 
  (= foo bar))      ; local variables
```
