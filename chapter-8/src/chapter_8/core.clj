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


(defmacro my-print-whoopsie
  [expression]
  (list let [result expression]
        (list println result)
        result))

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
