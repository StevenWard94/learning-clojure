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
