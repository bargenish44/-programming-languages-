#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}     ;;here i fillD <SOL> <SOL> because it make sence 
        |  { union <SOL> <SOL> }        ;;here i fillD <SOL> <SOL> because it make sence
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL]   ;;here i fillD Number <SOL> because it gets a scalar and a SOL
    [Inter SOL SOL]      ;;here i fillD <SOL> <SOL> because it make sence
    [Union SOL SOL]      ;;here i fillD <SOL> <SOL> because it make sence
    [Id    Symbol]
;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun   Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

;;this function is a tail-rec function that gets a number and a SET and returns if there is another instance of the variable in the list.
  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))

  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))

;; this func gets a set and removes all of his duplicates elements.
;;it was easy - about 1 min.
  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
          [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
 (test (remove-duplicates '(1)) => '(1))
;; this func gets a set and sort and removes all of his duplicates elements.
;;it was easy - about 1 min.
  (: create-sorted-set : SET -> SET)
  (define (create-sorted-set l)
    (let ([l (remove-duplicates l)]) (sort l <)))

(test (create-sorted-set '(5 3 4)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '(3 4 5 1 3 4)) => '(1 3 4 5))
(test (create-sorted-set '(1)) => '(1))

;; this func gets two Sets union them and removes all of his duplicates elements.
;;it was easy - about 1 min.
  (: set-union : SET SET -> SET)
  (define (set-union A B)
    (create-sorted-set (append A B)))

(test (set-union '(5 3 4) '(1 3 2)) => '(1 2 3 4 5))
(test (set-union '( 3 2 3 5 6) '(6 8 7)) => '(2 3 5 6 7 8))
(test (set-union '(3 4 5 1 3 4 0) '(2 3 1 4 5 7 9 6 8)) => '(0 1 2 3 4 5 6 7 8 9))
(test (set-union '(1) '()) => '(1))

;; this func gets two Sets intersection them and remove all of his duplicates elements and sort them.
;;the part of the filter was bit tricky - about 10 min.

  (: set-intersection : SET SET -> SET)
  (define (set-intersection A B)
    (: mem-filter : Number -> Boolean)
    (define (mem-filter n)
      (ismember? n A))
    (create-sorted-set (filter mem-filter B)))


(test (set-intersection '(5 3 4) '(1 3 2)) => '(3))
(test (set-intersection '( 3 2 3 5 6) '(6 8 7)) => '(6))
(test (set-intersection '(3 4 5 1 3 4 0) '(2 3 1 4 5 7 9 6 8)) => '(1 3 4 5))
(test (set-intersection '(1) '()) => '())


;; ---------------------------------------------------------
;; Parser
  ;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.

;; this part wasn't hard, it was very logical. took me about 14 min.

  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: ns) ...) (Set (create-sorted-set ns) )] ;; sort and remove-duplicates , i had to sort and remove the duplicates , it was easy 2 min.
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
         ;; It took me 10 min to understand which element to use in the second name symbol and named but it was similar to the olds exams.
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice ;; it was easy the answer was in the tests.
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;;it was like call2 from ex5
      [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;;it was like call2 from ex5
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

    


  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters

  
;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env) ,extend(x1,eval(E1,env),envf)) )
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env)
             =  eval(Ef,extend(x2,eval(E2,env) ,extend(x1,eval(E1,env),env)))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

|#

;; Types for environments, values, and a lookup function


;;this part wasn't easy and wasn't hard, it was ok.
;; it Took me 1 hour.
  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 
;; this func gets a VAL and returns a set if the VAL type is SetV and error otherwise.
  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))

;; this func gets a number and Val and mult each number element from this VAL by the given number and returns a new VAL where each num element equal to element * the given num.
;; it was pretty easy. 
(: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op (SetV->set s))))

(test (smult-set 1 (SetV '(1 2 3 4))) => (SetV '(1 2 3 4)))
(test (smult-set 2 (SetV '(1 2 3 4))) => (SetV '(2 4 6 8)))
(test (smult-set 0 (SetV '(1 2 3 4))) => (SetV '(0 0 0 0)))
(test (smult-set 0 (SetV '())) => (SetV '()))
(test (smult-set 0 (FunV 'x 'y (Set '(1 2 3)) (EmptyEnv))) =error> "expects a set")



  ;; this func gets a function that gets 2 sets and returns set, and two VALS and called the function on the VALS, and return the output of the function
  ;;wrapped by SetV. it was pretty easy.
 (: set-op : (SET SET -> SET) VAL VAL -> VAL )
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))

(test (set-op set-union (SetV '(1 2 3 4)) (SetV '(1 2 3 4 7))) => (SetV '(1 2 3 4 7)))
(test (set-op set-intersection (SetV '(1 3 4)) (SetV '(1 2 3 7))) => (SetV '(1 3)))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV (create-sorted-set S))] ;Basiclly I don't need to sort and remove duplicates because I already did it in the parser, but u asked for it so I added it.
      [(Smult n set) (smult-set n (eval set env))] ;; easy 
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))] ;; easy
      [(Union l r) (set-op set-union (eval l env) (eval r env))] ;; easy
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
      [(CallS fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body
                (Extend bound-id1 (eval arg-expr1 env) ;; similar to ex5, and you talked a lot on the possible change the environment from static to dynamic.
                        (Extend bound-id2 (eval arg-expr2 env) f-env)))]
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body
                (Extend bound-id1 (eval arg-expr1 env)
                        (Extend bound-id2 (eval arg-expr2 env) env)))]  ;; similar to ex5, and you talked a lot on the possible change the environment from static to dynamic.
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))


(test (eval (Smult 3 (Set '(1 2 3 4))) (EmptyEnv)) => (SetV '(3 6 9 12)))

(test (eval (Set '(1 2 3 4)) (EmptyEnv)) => (SetV '(1 2 3 4)))
(test (eval (Union (Set '(1 2 3)) (Set '(2 3 4))) (EmptyEnv)) => (SetV '(1 2 3 4)))
(test (eval (Inter (Set '(1 2 3)) (Set '(2 3 4))) (EmptyEnv)) => (SetV '(2 3)))
(test (eval (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))) (EmptyEnv)) => (SetV '(2 3 6 9)))

(test (eval (CallD (Fun 'S
                  'S
                  (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))) (EmptyEnv)) => (SetV '(2 3 6 9)))

;; It took me 5 hours. The first and second part was fine because you gave us the answer (the first gave me the left part and the second the right).
;; The real difficulty was the pair, I used the material from the last two lectures as a reference and the code for pair as seen in class.
;;I figured out I need to convert the code from class to fit the code here. The conversation was very challenging as it depended on many aspects of the new code.
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
    (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'cons (FunV 'f 's (Fun 'a 'b (CallS (Id 'a) (Id 'f) (Id 's))) (EmptyEnv))
                    (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv)) 
                                    (EmptyEnv)))))

  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (createGlobalEnv))]) 
       (cases result
         [(SetV S) S]
         [else result]))) ;;fun

;; q 5.b
(test (run "{with {cons {fun {f s} {call-static cons f s}}}
 cons}") => (FunV
 'f
 's
 (CallS (Id 'cons) (Id 'f) (Id 's))
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend
   'cons
   (FunV 'f 's (Fun 'a 'b (CallS (Id 'a) (Id 'f) (Id 's))) (EmptyEnv))
   (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv)) (EmptyEnv))))))
(test (run "{with {first {fun {p spare-param}
 {call-static p {fun {a b} a} {}}}}
 first}") => (FunV
 'p
 'spare-param
 (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '()))
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend
   'cons
   (FunV 'f 's (Fun 'a 'b (CallS (Id 'a) (Id 'f) (Id 's))) (EmptyEnv))
   (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv)) (EmptyEnv))))))
(test (run "{with {second {fun {p spare-param}
 {call-static p {fun {a b} b} {}}}}
 second}") => (FunV
 'p
 'spare-param
 (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '()))
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend
   'cons
   (FunV 'f 's (Fun 'a 'b (CallS (Id 'a) (Id 'f) (Id 's))) (EmptyEnv))
   (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv)) (EmptyEnv))))))

(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))
(test (run "{scalar-mult 3 {scalar-mult 3 {2 3 4}}}") => '(18 27 36))
(test (run "x") =error> "no binding")
(test (run "{with {x 4}}") =error> "parse-sexpr: bad `with' syntax" )
(test (run "{+ 4 5}") =error> "parse-sexpr: bad syntax in")
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-dynamic {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{call-static {1 2 3} {1 2 3} {1 2 3}}")=error> "`call-static' expects a function" )
(test (run "{call-dynamic {1 2 3} {1 2 3} {1 2 3}}")=error> "`call-dynamic' expects a function" )

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
 {with {S {intersect {call-static first p {}}
 {call-static second p {}}}}
 {call-static {fun {x y} {union x S}}
 {scalar-mult 3 S}
 {4 5 7 6 9 8 8 8}}}}")
 => '(2 3 6 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
 {with {foo {fun {x y} {intersect x y}}}
 {call-static p foo {}}}}")
 => '(2 3))






;; q6
#|
section 1: I used Racet Documentation for the exercise.
section 2: The types that exist in the language we have defined are:
Set - that represents a set which is an order and unduplicated list.
Smult - that represent multiplication by a scalar and contains 2 args, the first element is a number and the second one is an SOL
       (that can be either a Smult) and the purpose of this type is to multiplication each number on the list by the scalar.
Inter - that represents an Intersection between two SOL, it holds 2 SOLS and while evaluating this Constructor we return an order and unduplicated list
        of the numbers that exist in both SOL (SET -> list).
Union - that represents a Union between two SOL, it holds 2 SOLS and while evaluating this Constructor we return an order and unduplicated list
        of the numbers that exist at the list in one 0f the SOL (SET -> list).
Id - used for unique name for expressions which will make our code readable and more efficient.
Fun - Simple function that holds two params and had a name.
CallD - allowing us to call a function in a dynamic environment, holds 3 SOLS where the first one is a function and the second and third one are
        arguments for the functions.  
CallS- allowing us to call a function in a static environment, holds 3 SOLS where the first one is a function and the second and third one are
        arguments for the functions.
section 3 : 
For overcoming the difference in the number of parameters I duplicate the id (name) from the with and duplicate the with named,
        and change the with to (callS (fun))
I used to call static because I wanted to save the function as a cluster and used my createGlobalEnv function.
section 4 :
ismember? a procedure used only tail-rec.
The advantage of tail recursion is that it prevents all calls from being read in a read cartridge and instead in the cartridge memory
        there will only be the next call that needs to be made.
section 5 :
I used only call-static and not call-dynamic because I want that cons will be a cluster function that contains pair,
        So in case, I called the function of the pair with a first selector I will be given back the first element of the pair.
I couldn't use call-dynamic because I need the function to be a cluster (to save the elements in the function environment)
        which is not possible in dynamic because a function does not save her environment
        (in our case it saved but in eval, we don't use this environment we used the environment of the call).
section 6 :
What would happen is that cons would not remember his 2 elements so when we will try to use them we would get an error
        because when we use CallD we would look for these elements in the real global environment of pl but they don't exist there,
        unlike callS who would remember his environment and knew where to look for them.
|#
