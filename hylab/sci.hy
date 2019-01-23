(require [hylab.cl [*]])

(eval-and-compile

  (import [toolz.curried :as tz])
  
  (deftag t [expr]
    "Cast form to a tuple."
    `(tuple ~expr))
  
  (deftag $ [expr]
    "Curry a form."
    `(tz.curry ~@expr))
  )

;; quick python evaluation
(eval-and-compile
  
  (import [hylab.parser [PyParser]])
  
  (defun pyparse (expr-str)
    (let ((ps (PyParser)))
      (.parse ps expr-str)))
  
  (defmacro $ (expr)
    (read-str (pyparse expr))))

;; numpy utils
(eval-and-compile
  (defun parse-colon (sym)
    "Parse a colon expression used in index"
    (list (map
            (lambda (x)
              (if (empty? x)
                None
                (int x)))
            (.split (str sym) ":"))))

  (defun parse-indexing (sym)
    "Parse each index notation"
    (cond/cl
      ((in ":" (str sym)) `(slice ~@(parse-colon sym)))
      ((in "..." (str sym)) 'Ellipsis)
      (True sym)))

  (defmacro nget (ar &rest keys)
    "Get numpy array elements with numpy indexing
     Example: (nget ar 10:50 0)
              (nget ar ... 1)
              (nget ar : None)"
    `(get ~ar (, ~@(map parse-indexing keys))))

  (defmacro lget (lst &rest indices)
    "Get elements with numpy like indexing
     List or tuples will be automatically converted
     to numpy array"
    `(nget (np.array ~lst) ~@indices))

  )
