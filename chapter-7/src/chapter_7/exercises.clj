(ns chapter-7.exercises
  (:require [clojure.test :refer [with-test is are]]))


;; Exercise 1:
;;   Use the 'list' function, quoting, and 'read-string' to create a list that,
;;   when evaluated, prints your first name and your favorite sci-fi movie.
(defn name-with-movie
  [first-name fav-scifi-movie]
  (list (read-string "println") first-name fav-scifi-movie))

(with-test
  (def ex1)
  (is (= (name-with-movie '"Steven" '"Jurrasic Park")
         '(println "Steven" "Jurrasic Park"))))
 
; chapter-7.core> (test #'ex/ex1)
;              => :ok


;; Exercise 2:
;;   Create an infix fuction that takes a list [e.g. (1 + 3 * 4 - 5)] and
;;   transforms it into lists that can be evaluated by Clojure using operator
;;   precedence rules.
(def op-precedence {'* 1 '/ 1 '+ 0 '- 0})

(defn is-operator?
  [sym]
  (get op-precedence sym))

(defn precedence-lower?
  [op1 op2]
  (<= (op-precedence op1) (op-precedence op2)))


(defn flatten1
  "Flattens a collection by one level"
  [coll]
  (apply concat coll))


(defn shunting-yard
  "Applies the 'shunting-yard algorithm' to convert infix to reverse Polish notation"
  [tokens]
  (flatten1
    (reduce
      (fn [[output stack] token]
        (if (is-operator? token)
          (let [[higher lower] (split-with (partial precedence-lower? token) stack)]
            [(vec (concat output higher)) (cons token lower)])
          [(conj output token) stack]))
      [[] ()]
      tokens)))


(defn rpn->sexpr
  "Transforms a list in reverse Polish notation (RPN) to a list of
  s-expressions. Walks the RPN list and inserts inner list nodes in
  '(op num1 num2)' form.
  E.g. (rpn->sexpr (1 3 4 * + 5 -)) => (- (+ 1 (* 3 4)) 5)"
  [rpn-list]
  (first
    (reduce
      (fn [s-exprs token]
        (if (is-operator? token)
          (let [[expr1 expr2 & tail] s-exprs] (cons (list token expr2 expr1) tail))
          (cons token s-exprs)))
      ()
      rpn-list)))


(def infix (comp rpn->sexpr shunting-yard))

(def infix-calc (comp eval infix))


(with-test
  ; Test both the notation transformation & calculation
  (def ex2-infix-tests (juxt infix infix-calc))
  (are [input results] (= (ex2-infix-tests input) results)
       '(1 +  2) ['(+ 1  2)  3]
       '(1 * -9) ['(* 1 -9) -9]
       '(1 + 3 * 4 - 5) ['(- (+ 1 (* 3 4)) 5)   8]
       '(1 - 3 * 4 - 5) ['(- (- 1 (* 3 4)) 5) -16])
  )

; chapter-7.core> (test #'ex/ex2-infix-tests)
;              => :ok
