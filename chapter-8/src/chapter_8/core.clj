(ns chapter-8.core
  (:gen-class))

;; A macro expansion to show how '(when A B C D)' is really a macro for
;; '(if A (do B C D))'
(def expand-when
  (macroexpand
    '(when boolean-expression
       expression-1
       expression-2
       expression-3)))

; => (if boolean-expression
; =>   (do expression-1
; =>       expression-2
; =>       expression-3))


(defmacro infix
  "Use this macro when you pine for the notation of your childhood"
  [infixed-expr]
  (list (second infixed-expr) (first infixed-expr) (last infixed-expr)))

; => (infix (1 + 1))
; => 2
; => (macroexpand '(infix (1 + 1)))
; => (+ 1 1)

(defmacro infix-2
  [[operand1 op operand2]]
  (list op operand1 operand2))


(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form returns logical
  false (nil or false), 'and' returns that value and does not evaluate any of
  the remaining expressions, otherwise it returns the value of the last expr.
  When called without arguments, e.g. '(and)', it returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))


; (defmacro my-print-whoopsie
;   [expression]
;   (list let [result expression]
;         (list println result)
;         result))
; 
; => java.lang.RuntimeExpression: Can't take value of a macro: #'clojure.core/let

(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))


;; Source code for Clojure's 'when' macro, which provides an example of "quoting"
(defmacro when
  "Evaluates 'test'. If result is logical true, evaluates the body of an
  implicit 'do' form."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))


;; Another example of source for a built-in macro, 'unless'
(defmacro unless
  "Inverted 'if'"
  [test & branches]
  (conj (reverse branches) test 'if))


;; An example of "syntax quoting" (uses 'def' to bind result to a symbol)
;; Also demonstrates use of tilde to ensure that '(inc 1)' is evaluated
(def ex-tilde
  `(+ 1 ~(inc 1)))
; => (clojure.core/+ 1 2)

;; Now, the same form without the tilde (i.e. '(inc 1)' is not evaluated)
(def ex-no-tilde
  `(+ 1 (inc 1)))
; => (clojure.core/+ 1 (clojure.core/inc 1))

;; Finally, an example to show how using 'list' w/ quoting is less concise than
;; just using "syntax quoting" (the "syntax quoting" example is 'ex-tilde')
(def ex-quote-list
  (list '+ 1 (inc 1)))


;; Now that both kinds of "quoting" have been covered, the following examples
;; demonstrate how "syntax quoting" can make a macro definition more concise
(defmacro code-critic-long
  "Phrases are courtesy of Hermes Conrad from Futurama"
  [bad good]
  (list 'do
        (list 'println
              "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))
; chapter_8.core=> (code-critic-long (1 + 1) (+ 1 1))
;               => Great squid of Madrid, this is bad code: (1 + 1)
;               => Sweet gorilla of Manila, this is good code: (+ 1 1)

(defmacro code-critic-short
  "Phrases are courtesy of Hermes Conrad from Futurama"
  [bad good]
  `(do (println "Great squid of Madrid, this is bad code:"
                (quote ~bad))
       (println "Sweet gorilla of Manila, this is good code:"
                (quote ~good))))


;;;
;; Refactoring a Macro and Unquote Splicing
;;;

; The 'code-critic-short' macro defined above still isn't "ideal".
; It contains unnecessary duplication (the two 'println' forms), which could be
; handled by a helper function instead. Typically, it is better to move the
; "guts" of a macro into one, or more, functions, since functions are easier to
; think about and use than macros.
(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))

; Now use 'map' to avoid making two calls to 'criticize-code'
;(defmacro code-critic
;  [bad good]
;  `(do ~(map #(apply criticize-code %)
;             [["Great squid of Madrid, this is bad code:" bad]
;              ["Sweet gorilla of Manila, this is good code:" good]])))
;
; => NullPointerException
