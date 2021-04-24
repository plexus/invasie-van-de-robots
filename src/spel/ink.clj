(ns spel.ink)

(defmacro inline-file [path]
  (slurp path))
