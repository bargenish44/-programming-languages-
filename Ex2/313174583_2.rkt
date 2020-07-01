#lang pl
#|
                                                                         q1
Took me about 4 hours.
It took me a long time to understand the language and the parsers and once I realized the question was fine.
|#
; The ROL BNF and Parsing code:
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))     
 
;; The actual interpreter
#| BNF for the RegE language:
<ROL>  ::= {reg-len = <num> <RegE> }
<RegE> ::= {<Bits>}            (1)       
	  |{and <RegE> <RegE>} (2)       
	  |{or <RegE> <RegE>}  (3)     
	  |{shl <RegE>}        (4)
 
<Bits> ::= <Bit> | <Bit> <Bits>
<Bit> ::= 1 | 0

examples :
example 1 : (parse "{ reg-len =  4  {1 0 0 0}}") =>
<ROL> = { reg-len = <num> <RegE>
                      |      |
                      4     <Bits> (1)
                               |
                             <Bit> <Bits>
                               |      |
                               1    <Bit> <Bits>
                                      |      |
                                      0    <Bit> <Bits>
                                             |      |
                                             0    <Bit>
                                                    |
                                                    0

example 2 : (parse "{ reg-len = 3  {shl {0 1 0}}}") =>
<ROL> = { reg-len = <num> <RegE>
                      |      |
                      3   Shl(4) <RegE>
                                  |
                                <Bits> (1)
                                   |
                                 <Bit> <Bits>
                                   |      |
                                   0    <Bit> <Bits>
                                          |      |
                                          1    <Bit>
                                                 |      
                                                 0    

example 3 : (parse "{ reg-len =  2  {and {shl {1 0}} {shl {0 1}}}}")
<ROL> = { reg-len = <num> <RegE>
                      |      |
                      2  And(2) <RegE> <RegE>
                                 |          |
                       Shl(4) <RegE>        Shl(4) <RegE>
                              |                     |
                             <Bits>(1)              <Bits> (1)
                               |                       |
                             <Bit> <Bits>              <Bit> <Bits>
                               |      |                  |      |
                               1     <Bit>               0     <Bit>
                                       |                         |
                                       0                         1

|# 
 
;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE])
 
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))])) 

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len '= (number: n) regE)
     (if (> n 0) (parse-sexpr-RegL regE n)
         (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs 
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a)) (Reg (list->bit-list a))
                                      (error 'parse-sexpr "wrong number of bits in ~s" a))]
    [(list 'and lst1 lst2)
     (And (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'or lst1 lst2)
     (Or (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'shl lst)
     (Shl(parse-sexpr-RegL lst reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 
 
(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str))) 
 
 
 

;; tests
(test (parse "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4  {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4  {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0)))
                                                                              (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4  { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0)))
                                                                                                  (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2  { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 4  {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in") 
(test (parse "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (parse "{ {}} ") =error> "bad")
(test (parse "{ reg-len = 3 {or {2 1 1} {1 0 0}}}") =error> "bad syntax in (2 1 1)")

#|
                                                                         q2

1.a The problem is that we can't know the order of evaluation so set 1 might be evaluated before set 2 so get will return 2 (the last number to be set) or set 2
might be evaluated before set 1 so get will return 1 (the last number to be set).
1.b I suggest changing the semantics such that it be illegal to have set and get in the same MAE.

2. The BNF:
Took me about 3 hours, It took me a while to figure out how to include all the cases.

<MAE> ::= {seq <InnerSeq>}

<InnerSeq> ::= <SET-NO-GET> <SET> <NO-SET>
| <NO-SET-NO-GET>
| <SET-NO-GET> <NO-SET>

<SET-NO-GET> ::= {set <NO-SET-NO-GET>}

<SET> ::= {set <NO-SET>} | {set <NO-SET>} <SET>

<NO-SET> ::= <num>
 | { + <NO-SET> <NO-SET> }
 | { - <NO-SET> <NO-SET> }
 | { * <NO-SET> <NO-SET> }
 | { / <NO-SET> <NO-SET> }
 | get

<NO-SET-NO-GET> ::= <num>
 | { + <NO-SET-NO-GET> <NO-SET-NO-GET> }
 | { - <NO-SET-NO-GET> <NO-SET-NO-GET> }
 | { * <NO-SET-NO-GET> <NO-SET-NO-GET> }
 | { / <NO-SET-NO-GET> <NO-SET-NO-GET> }

examples:
example 1 : {seq {set {+ 313 1745}}
                 {set {* get get}}
                    {/ get 83}} =>

<MAE> = {seq <InnerSeq>}
                  |
        <SET-NO-GET> <SET>... <NO-SET>
          |               |            |
{set <NO-SET-NO-GET>} {set <NO-SET>}    { / <NO-SET> <NO-SET> }
         |                         |___           |         |
{ + <NO-SET-NO-GET> <NO-SET-NO-GET> }  |          get      <num 83>
           |                |  { * <NO-SET> <NO-SET> }
        <num 313>      <num 1745>      |       |
                                      get    get


example 2 : {seq {set {* 0525 5582}}
                    {- get 938}} =>
<MAE> = {seq <InnerSeq>}
                  |
            <SET-NO-GET> <NO-SET>
                 |           |
  {set <NO-SET-NO-GET>}    { - <NO-SET> <NO-SET> }
                |                     |         |
{ * <NO-SET-NO-GET> <NO-SET-NO-GET> } get     <num 38>
           |               |
          <num 0525>   <num 5582>


example 3 : {seq {- 8 2}} =>
<MAE> = {seq <InnerSeq>}
                  |
            <NO-SET-NO-GET>
                 |
          { - <NO-SET-NO-GET> <NO-SET-NO-GET> }
                    |                |
                  <num 8>         <num 2>



                                                                         q3

Took me about 1 hour.
Function square :
Input : Number
Output : Number
Procedure : get a number as input and returns it squared.
Function sum-of-squares : 
Input : List of numbers
Output : List
Procedure : get a list of numbers as input and returns the sum of each number squared.
|#

(: square : Number -> Number )
(define (square num) (* num num))

(: sum-of-squares : (Listof Number) -> Number )
(define ( sum-of-squares lst )
  (foldl + 0 (map (lambda (Number)(square Number)) lst)))

(test (square 1) => 1)
(test (square 0) => 0)
(test (square -6) => 36)
(test (square 0.5) => 0.25)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(-1 -2 3)) => 14)
(test (sum-of-squares '(1 3 4)) => 26)
(test (sum-of-squares '(-1 0.5 0.25)) => 1.3125)

#|
                                                                         q4
About 4 hours.
I had trouble with the tree fold function and it took me a while to execute it in a generic and good way.
In addition, it took me some time to understand the inst and then it was easy to realize the rest.
|#

(define-type BINTREE
  [Node BINTREE BINTREE]
  [Leaf Number])

; Increase the number by 1
(: add1 : Number -> Number)
(define (add1 num)
  (+ num 1))
#|
Function tree-map :
Input : callbeck function that gets number and returns number and a BINTREE
Output : BINTREE
Procedure : Uses the function we received in the input on each of the leaves in the tree.
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f Tree)
  (cases Tree
    [(Node left right) (Node (tree-map f left) (tree-map f right))]
    [(Leaf num) (Leaf (f num))]))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) 
      => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Leaf 6.4)) => (Leaf 7.4))
(test (tree-map add1 (Node (Leaf 1) (Leaf 3))) 
      => (Node (Leaf 2) (Leaf 4)))

; Returns the sum of the two numbers
(: add : Number Number -> Number)
(define (add num1 num2)
  (+ num1 num2))
; Returns the same number (Identity function)
(: idn : Number -> Number)
(define (idn num)
  num)

#|
Function tree-fold : 
Input : Two callbeck function and a BINTREE
Output : BINTREE
Procedure : Recursively performs the appropriate function on the tree, with a node-specific function and a different leaf function.
|#
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold f1 f2 Tree)
  (cases Tree
    [(Node left right) (f1 (tree-fold f1 f2 left) (tree-fold f1 f2 right))]
    [(Leaf num) (f2 num)]))
(test (tree-fold add idn (Node (Leaf -1) (Node (Leaf 1) (Leaf 5)))) 
      => 5)
(test (tree-fold add add1 (Node (Leaf -1) (Node (Leaf 1) (Leaf 5)))) 
      => 8)

(: tree-flatten : BINTREE -> (Listof Number)) 
;; flattens a binary tree to a list of its values in 
;; left-to-right order 
(define (tree-flatten Tree) 
  (tree-fold (inst append Number) (inst list Number) Tree))

(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => '(1 2 3))

; switchs the tree nodes
(: switch-nodes : BINTREE BINTREE -> BINTREE)
(define (switch-nodes Tree1 Tree2)
  (Node Tree2 Tree1))

#|
Function tree-reverse :
Input : BINTREE
Output : BINTREE
Procedure : Return the input reversed BINTREE, Recursive function.
Uses tree-fold function.
|#
(: tree-reverse : BINTREE -> BINTREE )
(define (tree-reverse Tree)
  ((inst tree-fold BINTREE) switch-nodes (lambda (x) (Leaf x)) Tree))

(test (tree-reverse (Node (Leaf 0) (Node (Leaf 1) (Leaf 2)))) 
      => (Node (Node (Leaf 2) (Leaf 1)) (Leaf 0)))
(test (tree-reverse (Node (Leaf 1) (Leaf 0.5))) => (Node (Leaf 0.5) (Leaf 1)))
(test (tree-reverse (Leaf 3)) => (Leaf 3))
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Leaf 0.5))))
 (tree-flatten (tree-reverse (Node (Leaf 1) (Leaf 0.5))))) => #t)
