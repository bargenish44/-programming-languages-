#lang pl
#|
                                              q1
This func checks if one of the strings contains the string "pl" as a suffix.
The string gets an input list of strings and returns the first string that contains "pl" as a suffix or false if none of the strings contains it.
I used tail-recursion that send each string to the helper function and get back a boolean that represents whether it contains the suffix "pl" or not,
if the return value is true the function return that string and done, else it recalls herself with the rest of the list.
if the list is empty the function returns false.
the help function gets a string as an input and returns boolean, first, it checks if the string length is at least 2, otherwise, it returns false.
then it checks if the suffix is "pl" if yes it returns true, otherwise false.
This question wasn't difficult, but it still took me one day because I have experienced syntax problems with the language.
|#

( : plSuffixContained : (Listof String) -> (U String #f))
(define (plSuffixContained list)
  (: plSuffixContainedHelper : String -> Boolean)
  (define (plSuffixContainedHelper str)
    (cond
      (( < (string-length str) 2)#f)
      ((and(if(eq? (string-ref str (- (string-length str) 2)) #\p) #t #f)(if(eq? (string-ref str (- (string-length str) 1)) #\l) #t #f)) #t)
      (else #f)
      ))
  (cond
    ((null? list) #f)
    ((plSuffixContainedHelper (first list)) (first list))
    (else (plSuffixContained (rest list)))
    ))
(test (plSuffixContained '("yyyt" "TplT" "plTT" "PLPl" "plplpl")) => "plplpl")
(test (plSuffixContained '()) => #f)
(test (plSuffixContained '("plaPl" "PLpl" "pl")) => "PLpl")
(test (plSuffixContained '("abc" "abpL" "abPl" "abplt")) => #f)
(test (plSuffixContained '("p" "l" "ppll" "plPl")) => #f)

#|
                                              q2.1
In this question i had to define a write-poly function that consumes a list of coefficients (numbers) 𝑎1,𝑎2,…,𝑎𝑛 and returns the polynomial
(in a reversed order of coefficients) "𝑎1𝑥𝑛 + 𝑎2𝑥𝑛−1 + ⋯+ 𝑎𝑛". 
I used 3 functions for this question, the first one is help function that called calc-pow.
her input is a number, the output is a string.
Its purpose is to calculate the representation of power.
while a power that equals to one represents only with x,
a number less than 1 represent with anything (Recalls that it is only power and not a coefficient).
a number bigger than 1 represents as "x^" and the power.
My main function input is a list of strings and her output is a string.
she checks if the list is empty if true she returned empty string as I was asked.
otherwise, it calls the help tail-recursion function that called write-poly-rec with the values: empty string (the current poly as string), the list and true,
I used boolean To avoid chaining zeros, as for example list of '(0 0 0 ) supposed to return "0" but '(0 0 8) should return "8".
The help function uses cond :
if the list is empty (end of the list) it returns "0" if the string is ""
(When we got here it means that our list wasn't empty on start so we will return 0 and not ""), otherwise it return the str.
if the first value in the string is zero we will recall the function with the rest list when all other data doesn't change.
The next check is if the first number is negative else it is positive,
in both ways it calls write-poly-rec func with the first list as a string , here we have a small difference between the cases:
in case of a positive number, we want to have a "+" sign only if it isn't the first number in our poly list,
that's why I used the boolean.
both cases (positive and negative) recall the help func with the new str that the append bassed on the first number as a string and the power
that the first help function return them (the first help func input is the list length-1) and the rest of the list and #f (it have already chained a number).
This question was quite annoying because of the multiple cases, at first, I got into trouble with all the cases and it took me
quite a while to figure out how to do this, but after consulting with some other students (of course without passing code from one to other)
I managed to come to a solution, estimated as two days.
|#

(: calc-pow : Number -> String)
(define (calc-pow power)
  (cond
    ((> power 1) (string-append "x^" (number->string power)))
    ((= power 1) "x")
    (else "")))

(: write-poly : (Listof Number) -> String)
(define (write-poly list)
  (: write-poly-rec : String (Listof Number) Boolean -> String)
  (define (write-poly-rec str list fst)
    (cond
      ((null? list) (if (eq? str "") "0" str))
      ((= (first list) 0) (write-poly-rec str (rest list) fst))
      ((< (first list) 0) (write-poly-rec (string-append str (number->string (first list)) (calc-pow(- (length list) 1))) (rest list) #f))
      (else (write-poly-rec (string-append str (if fst "" "+") (number->string (first list)) (calc-pow(- (length list) 1))) (rest list) #f))))
  (if
   (null? list) ""
   (write-poly-rec "" list #t))) 

(test (write-poly '(2 3 4))=> "2x^2+3x+4")
(test (write-poly '(0 0 0))=> "0")
(test (write-poly '(0 1 3 2 0))=> "1x^3+3x^2+2x")
(test (write-poly '(-1 -2 -3))=> "-1x^2-2x-3")
(test (write-poly '(0))=> "0")
(test (write-poly '(0 0 6)) => "6")
(test (write-poly ' (0 8 5 -1)) => "8x^2+5x-1")
(test (write-poly '()) => "")
(test (write-poly ' (8.2 -5.4 -1)) => "8.2x^2-5.4x-1")
(test (write-poly '(0 0 6.3333333334)) => "6.3333333334")
(test (write-poly '(0 0 -6)) => "-6")

#|
                                              q2.2
For this question, I used help tail-recursion function.
The main function gets as input a number called xNumber that represent the value of x and a list, the purpose is to calculate the result of the given poly with the x value,
When a monomial pow is the amount of monomial to be followed by the poly.
example '(2 3 4) -> 2x^2+3x+4.
The main func checks if the list is empty if it's true it returns 0, otherwise it calls the help function with the list and zero because this is the result so far.
The help function doesn't need to get the xNumber because it is realized within it, so it has access to it.
the help function stopping condition is if we have reached the end of the list if true then it returns the sum.
otherwise, it recalls herself with the rest list and the new sum that it calculates (i used except for this).
The help func input is list and sum.
The output is the result of the poly.
This question was easy it took me 5 minutes.
|#

(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly xNumber list)
  (: compute-poly-rec : (Listof Number) Number -> Number)
  (define (compute-poly-rec list sum)
    (if
     (null? list) sum
     (compute-poly-rec (rest list) (+ sum (* (first list) (expt xNumber (- (length list) 1)))))))
  (if
   (null? list) 0
   (compute-poly-rec list 0)))
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 1 '(2)) => 2)
(test (compute-poly 0 '(4 3 -2 0)) => 0)
(test (compute-poly 0.5 '(4 3 -2 0)) => 0.25)
(test (compute-poly -2 '(3 2 6)) => 14)

#|
                                              q3
In this question, I created a new type called KeyStack that has 2 constructors: one of the types of EmptyKS and one of the type: Push Symbol String KeyStack.
The first two sections were easy and did not take long.

search-stack -
In the third section, I used a tail-rec function that gets a symbol and a KeyStack type.
The function search for the first symbol that is the same as the one we received in the function, if she finds the symbol the func returns the string of the current KeyStack otherwise, it recall herself and sends her KeyStack value.
In the case of EmptyKs keyStack, it returns false.
This question is quite tricky because of the use of cases,
but after repeating the second lecture, I figured out how to solve it. It took me about two hours.

pop-stack -
In the fourth section, I also used tail-rec func that implements the pop method of a given Keystack.
If the KeyStack is from the type EmptyKS than it returns false, otherwise it returns the KeyStack value of the current KeyStack.
it wasn't difficult after the last section, it took me about 5 minutes.
|#

( define-type KeyStack
   [EmptyKS]
   [Push Symbol String KeyStack])

(: search-stack : Symbol KeyStack -> (U String #f))
( define ( search-stack sym ks)
   (cases ks 
   [(EmptyKS) #f]
   [(Push sm st nsk) (if (eq? sm sym) st (search-stack sym nsk))]
   ))

(: pop-stack : KeyStack -> (U KeyStack #f))
( define (pop-stack ks)
   (cases ks
     [(EmptyKS) #f]
     [(Push sm st nsk) nsk]))

(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A"  (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'a "a" (Push 'b "b" (Push 'a "aA"  (EmptyKS)))) => (Push 'a "a" (Push 'b "b" (Push 'a "aA" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA") 
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'a (Push 'a "A1" (Push 'b "B" (Push 'a "A2" (EmptyKS))))) => "A1")
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS)))) 
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "A1" (Push 'b "B" (Push 'a "A2" (EmptyKS))))) => (Push 'b "B" (Push 'a "A2" (EmptyKS))))

;q4

(: is-odd? : Natural -> Boolean)
#|
This func get a natural number and returns a boolean that says if the number is odd.
The if condition checks if the given number is zero, if this is the case it returns false, else it calls the is-even? func with x-1.
examples : 0 as input returns false because zero is even.
3 as input call is-even? with 2 value, and the is-even? will call is-odd? with 1 value, the number isn't equal to zero so it calls is-even? with 0,
then the number is 0 so the function returns true, and indeed 3 is odd.
last example: 2 as input to is-odd? func, it will call is-even? with 1 value, the number not equal to zero => call is-odd? with 0 value,
now the zero? condition of the is-odd? the function is true so we return false, indeed 2 isn't odd.
|#
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1)))) 
 
(: is-even? : Natural -> Boolean)
#|
This func get a natural number and returns a boolean that says if the number is even.
The if condition checks if the given number is zero, if this is the case it returns true, else it calls the is-odd? func with x-1.
examples:
3 as input call is-odd? with 2 value, and the is-odd? will call is-even? with 1 value, the number isn't equal to zero so it calls is-odd? with 0,
then the number is 0 so the function returns false, and indeed 3 isn't even.
2 as input to is-even? func, it will call is-odd? with 1 value, the number not equal to zero => call is-even? with 0 value,
now the zero? condition of the is-even? the function is true so we return true, indeed 2 is even.
|#
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
The func calls the function on each element on the list.
each element that given to the function must be from the same type as the elements in the list.
The given function returns boolean.
The function returns false if one or more of the list element returns false by the given func,
and returns true if all of the list element returns true by the given func or! the list is empty.
|#
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))


;; An example for the usefulness of this polymorphic function
(: all-even? :   (Listof Natural) -> Boolean)
#|
The func calls the is-even? function on each element on the list.
The function checks if all the list elements are even OR the list is empty => true otherwise(at list one of the elements is odd) false.
Input: List of Naturals.
Output: True => all list elements are even or the list is empty otherwise, false.
|#
(define (all-even? lst)
  (every? is-even? lst))


;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
#|
The function gets two functions and two lists, the first func gets type A as the first list.
The second func gets type B as the second list.
The main function calls the first function with the first list element value for each value of the list, the same as with the second list and function.
The given functions return boolean.
Input: Two functions and two lists
Output: True, if all elements in the lists return true by the given functions or both of the lists is empty, otherwise false.
|#
(define (every2? pred1 pred2  lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))
