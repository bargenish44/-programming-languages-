;; The Flang interpreter, using environments

#lang pl

#|
The homework is basically about adding function with 2 params and call with two args and changing the Add Sub Mul and Div to a cluster function.
Difficults : I had a problem with the meaning of createGlobalEnv function and with the order of calls error at eval.
Took me about 4 hours. 
|#
#| The grammar:
<FLANG> ::= <num>
 | { with { <id> <FLANG> } <FLANG> }
 | <id>
 | { fun { <id> } <FLANG> } ;;a function may have a
 single formal parameter
 | { fun { <id> <id> } <FLANG> } ;; or two formal parameters
 | { call <FLANG> <FLANG> } ;;a function has either a
 single actual parameter
 | { call <FLANG> <FLANG> <FLANG> } ;; or two actual parameters

  Evaluation rules:
    eval(N,env)                = N
    eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
    eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
    eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
    eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
    eval(x,env)                = lookup(x,env)
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x} E},env)      = <{fun {x} E}, env>
    eval({call E1 E2},env1)
             = eval(Ef,extend(x,eval(E2,env1),env2))
                               if eval(E1,env1) = <{fun {x} Ef}, env2>
             = error!          otherwise



eval: Evaluation rules:
 eval(N,env) = N
 eval(x,env) = lookup(x,env)
 eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
 eval({fun {x1} E},env) = <{fun {x1} E}, env>
 eval({fun {x1 x2} E},env) = <{fun {x1 x2} E}, env>
 eval({call E-op E1},env1)
 = eval(Ef,extend(x1,eval(E1,env),envf))
 if eval(E-op,env) = <{fun {x} Ef}, envf>
 = error! otherwise
 eval({call E-op E1 E2},env1)
 = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf))
 if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
 = error! Otherwise
  |#

(define-type FLANG
  [Num Number]
  [Id Symbol]
  [Add FLANG FLANG] ; Never created by user
  [Sub FLANG FLANG] ; Never created by user
  [Mul FLANG FLANG] ; Never created by user
  [Div FLANG FLANG] ; Never created by user
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Fun-TWO Symbol Symbol FLANG]
  [Call FLANG FLANG]
  [Call-TWO FLANG FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [(list 'fun (list (symbol: name) (symbol: name2)) body)
        (Fun-TWO name name2 (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    ;;[(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    ;;[(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    ;;[(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    ;;[(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list 'call fun arg arg2) (Call-TWO (parse-sexpr fun) (parse-sexpr arg) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  [FunV-TWO Symbol Symbol FLANG ENV])

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (: NumV->number : VAL -> Number)
  (define (NumV->number v)
    (cases v
      [(NumV n) n]
      [else (error 'arith-op "expects a number, got: ~s" v)]))
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id bound-body)
     (FunV bound-id bound-body env)]
    [(Fun-TWO bound-id bound-id2 bound-body)
     (FunV-TWO bound-id bound-id2 bound-body env)]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         [(FunV-TWO bound-id bound-id2 bound-body f-env)
          (error 'eval "expected two arguments, got one in: ~s"
                 fval)]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]
    [(Call-TWO fun-expr arg-expr arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV-TWO bound-id bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env)
                        (Extend bound-id2 (eval arg-expr2 env) f-env)))]
         [(FunV bound-id bound-body f-env)
          (error 'eval "expected a single argument, got two in: ~s"
                 fval)]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]))
;; this function returns a environment where + - * / are known as cluster functions.
;; return ->
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend '/ (FunV-TWO 's1 's2 (Div (Id 's1) (Id 's2)) (EmptyEnv)) 
          (Extend '* (FunV-TWO 's1 's2 (Mul (Id 's1) (Id 's2)) (EmptyEnv))
                  (Extend '- (FunV-TWO 's1 's2 (Sub (Id 's1) (Id 's2)) (EmptyEnv)) 
                          (Extend '+ (FunV-TWO 's1 's2 (Add (Id 's1) (Id 's2)) (EmptyEnv))
                                  (EmptyEnv))))))

(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))
#|
;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
                {with {foo {fun {x} {+ x 1}}}
                  {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
                {with {f {fun {y} {+ x y}}}
                  {with {x 5}
                    {call f 4}}}}")
      => 7) 
(test (run "{call {with {x 3}
                      {fun {y} {+ x y}}}
                    4}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                          {fun {x} {fun {y} {+ x y}}}}
                    123}")
      => 124)

|#
;; tests
(test (run "{call + 4 5}") => 9)
(test (run "{with {add3 {fun {x} {call + x 3}}}
 {call add3 1}}")
      => 4)
(test (run "{with {x 3}
 {with {f {fun {y} {call + x y}}}
 {with {x 5}
 {call f 4}}}}")
      => 7)
(test (run "{call {fun {x y} {call + x { call - y 1}}} 4 2}") => 5)
(test (run "{with {first {fun {x y} x}}
 {with {second {fun {x y} y}}
 {call first {call second 2 123} 124}}}")
      => 123)
(test (run "{+ 4 5}") =error> "parse-sexpr: bad syntax in")
(test (run "{* 4 5}") =error> "parse-sexpr: bad syntax in")
(test (run "{with {add3 {fun {x} {call + x 3}}}
 {call add3 1 2}}")
      =error> "expected a single argument, got two in: ")
(test (run "{with {add3 {fun {x stam} {call + x 3}}}
 {call add3 1}}")
      =error> "expected two arguments, got one in: ")
(test (run "{fun {y} y}") =error> "evaluation returned a non-number")
(test (run "{fun {x} {+ x 1} {+ 4 5}}") =error> "bad `fun' syntax")
(test (run "{call {fun {x y} {call * x { call / y 1}}} 4 2}") => 8)
(test (run "{with num : x {call + 34 1} {call - x 30}}") =error> "parse-sexpr: bad `with' syntax")
(test (run "{call + 4 x}") =error> "no binding")
(test (run "{call + 5 {fun {y} y}}") =error> "expects a number")
(test (run "{with {add3 {fun {x z} {call x 3}}}
 {call 2 1}}") =error> "`call' expects a function")
(test (run "{with {add3 {fun {z} {call x 3}}}
 {call 2 1 3}}")=error> "`call' expects a function" )
(test (run "{with {mycons {fun {f s} {fun {loc-sel} {call loc-sel f s}}}}
    {with {myfirst {fun {p} {call p {fun {a b} a}}}}
        {with {mysecond {fun {p} {call p {fun {a b} b}}}}
            {with {p1 {call mycons 1 2}}
                {with {p2 {call mycons 3 4}}
                    {call + {call myfirst p1}
                            {call mysecond p2}}}}}}}") => 5)