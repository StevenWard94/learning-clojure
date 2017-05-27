(ns chapter-7.core
  (:gen-class))

(defmacro backwards
  [form]
  (reverse form))


(def addition-list (list + 1 2))

(eval (list 'def 'lucky-number (concat addition-list [10])))


(def three (read-string "(+ 1 2)"))


;; These things evaluate to themselves:
;    true
;; => true
;;
;    false
;; => false
;;
;    {}
;; => {}
;;
;    :huzzah
;; => :huzzah
;;
;    ()
;; => ()


(defn exclaim
  [exclamation]
  (str exclamation "!"))


(defmacro ignore-last-operand
  [function-call]
  (butlast function-call))


(defmacro infix
  [infixed]
  (list (second infixed)
        (first infixed)
        (last infixed)))


(defn read-resource'
  "Read a resource into a string"
  [path]
  (read-string (slurp (clojure.java.io/resource path))))


(defn read-resource
  "Read a resource into a string - using the '->' macro"
  [path]
  (-> path
      clojure.java.io/resource
      slurp
      read-string))
