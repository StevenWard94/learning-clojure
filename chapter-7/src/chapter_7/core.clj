(ns chapter-7.core
  (:gen-class))

(defmacro backwards
  [form]
  (reverse form))


(def addition-list (list + 1 2))
