;; Codes borrowed from
;; https://github.com/riktor/hycl

;; Created by: riktor
;; Modified by: guanyilun

(import
  [hy [HyKeyword]]
  [hy.contrib.hy-repr [hy-repr]])

(import [numpy :as np])
(import [matplotlib.pyplot :as plt])

(require
  [hy.contrib.loop [loop]])

(eval-and-compile
  (import
    [functools :as ft])

  (defn cons [a b]
    (+ (HyExpression [a]) b)))

(eval-and-compile
  ;; renamed functions
  (defmacro! setf (&rest args)
    `(do
       (setv ~@(get args (slice 0 (- (len args) 2))))
       (setv ~@(get args (slice -2 None)))
       ~(get args -2)))

  (defmacro typep (obj objtype)
    `(is (type ~obj) ~objtype))

  ;; todo: &optional cannnot accept () form. (now only [])
  (defmacro defun (name lambda-list doc &rest body)
    (setv lambda-list (lfor el lambda-list
                            (if (is (type el) HyExpression)
                                (list el)
                                el)))
    (if (not (typep doc str))
        `(defn ~name ~(list lambda-list) ~@(cons doc (HyExpression body)))
        `(defn ~name ~(list lambda-list) doc ~@body)))

  (defun eq (x y)
    (is x y))

  (defun equals (x y)
    (= x y))

  ;; numerical functions
  (defun mod (n m)
    (% n m))

  (defun zerop (n)
    (= n 0))

  (defun plusp (n)
    (> n 0))

  (defun minusp (n)
    (< n 0))

  (defun oddp (n)
    (zerop (mod n 2)))

  (defun evenp (n)
    (not (oddp n)))

  (defun divisible (n m)
    (zerop (mod n m)))

  (defmacro incf (n  &optional [delta 1])
    `(setf ~n (+ ~n ~delta)))

  (defmacro decf (n &optional [delta 1])
    `(setf ~n (- ~n ~delta)))

  (defmacro 1+ (n)
    `(+ ~n 1))

  ;; list functions
  (setf nil (HyExpression ()))

  (defun null (ls)
    (= nil ls))

  (defun lst (&rest args)
    (HyExpression args))

  (defun length (list)
    (len list))

  (defun emptyp (ls)
    (zerop (length ls)))

  (defun consp (el)
    (and (not (= el nil))
      (typep el HyExpression)))

  (defun car (ls)
    (first ls))

  (defun cdr (ls)
    (cut ls 1))

  (defun caar (ls)
    (-> ls car car))

  (defun cddr (ls)
    (-> ls cdr cdr))

  (defun cadr (ls)
    (-> ls cdr car))

  (defun cdar (ls)
    (-> ls car cdr))

  (defun apply (fn ls)
    (fn #*ls))

  (defmacro push (el ls)
    `(setf ~ls (cons ~el ~ls)))

  (defun nreverse (ls)
    (.reverse ls)
    ls)

  (defun nconc (x y)
    (.extend x y)
    x)

  (defun last (ls)
    (get ls (dec (length ls))))

  (defun mapcan (func ls)
    (loop
      ((ls ls)
        (acc ()))
      (if ls
        (recur (cdr ls) (nconc acc (func (car ls))))
        (HyExpression acc))))

  (defun append (ls1 ls2)
    (+ ls1 ls2))

  ;; macros
  (defmacro progn (&rest body)
    `(do ~@body)))

(eval-and-compile
  (defun mapcar (func &rest seqs)
    (HyExpression
      (apply (ft.partial map func) seqs)))

  (defun group (src n)
    (HyExpression (apply zip (* [(iter src)] n)))))

(eval-and-compile

  (defmacro lambda (lambda-list &rest body)
    `(fn ~(list lambda-list) ~@body))

  (defmacro! let (var:val &rest body)
    `((lambda ~(mapcar car var:val) ~@body)
      ~@(mapcar cadr var:val)))

  (defmacro! let* (var:val &rest body)
    (loop
      ((ls (nreverse var:val))
       (acc body))
      (if ls
          (recur (cdr ls) `(let (~(car ls))
                                ~@(if (= acc body)
                                      body
                                      `(~acc))))
          acc)))

  (defmacro! prog1 (&rest body)
    `(let ((~g!sexp-1 ~(car body)))
          (progn
            ~@(cdr body)
            ~g!sexp-1)))

  (defun pushr (ls el)
    (.append ls el))

  (defun pushl (ls el)
    (.insert ls 0 el))

  )

(eval-and-compile
  (defun flatten-1 (ls)
    (let ((acc ()))
         (for [el ls]
           (if (consp el)
               (nconc acc el)
               (.append acc el)))
         acc))

  (defmacro cond/cl (&rest branches)
    (loop
      ((ls branches)
       (cont (lambda (x) x)))
      (if ls
          (recur (cdr ls) (lambda (x) (cont `(if ~(caar ls)
                                                 (progn ~@(cdar ls))
                                                 ~x))))
          (cont None))))

  (defmacro! case (exp &rest branches)
    `(let ((~g!val ~exp))
          (cond/cl ~@(list (map (lambda (br)
                                  (if (= (car br) 'otherwise)
                                      `(True ~@(cdr br))
                                      `((eq ~g!val ~(car br)) ~@(cdr br))))
                                branches)))))

  (defun subseq (seq start end)
    (case (type seq)
          (str (.join "" ))
          (list (list (islice seq start end)))
          (HySymbol (HySymbol (.join "" (islice seq start end))))
          (HyExpression (HyExpression (islice seq start end)))
          (HyKeyword (HyKeyword (.join "" (islice seq start end))))
          (otherwise (raise TypeError))))

  (defun destruc (pat seq n)
    (if (null pat)
        nil
        (let ((rest (cond/cl ((not (consp pat)) pat)
                             ((eq (car pat) '&rest) (cadr pat))
                             (True nil))))
             (if rest
                 `((~rest (subseq ~seq 0 ~n)))
                 (let ((p (car pat))
                       (rec (destruc (cdr pat) seq (+ n 1))))
                      (if (not (consp p))
                          (cons `(~p (get ~seq ~n))
                                rec)
                          (let ((var (gensym)))
                               (cons (cons `(~var (get ~seq ~n))
                                           (destruc p var 0))
                                     rec))))))))

  (defmacro assign (source target)
    (lfor i (range (len source))
      `(setf ~(get source i) (get ~target ~i))))
    
  (defun dbind-ex (binds body)
    (if (null binds)
        `(progn ~@body)
        `(let ~(mapcar (lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                       binds)
              ~(dbind-ex (mapcan (lambda (b)
                                   (if (consp (car b))
                                       (cdr b)
                                       nil))
                                 binds)
                         body))))

  (defmacro! dbind (pat seq &rest body)
    `(let ((~g!seq ~seq))
          ~(dbind-ex (destruc pat g!seq 0) body)))


  (defmacro values (&rest returns)
    `(tuple ~returns))

  ;; multiple-value-bind
  (defmacro mvb (var-list expr &rest body)
    `(dbind ~var-list ~expr ~@body))

  ;; errors
  (defmacro! ignore-errors (&rest body)
    `(try
       ~@body
       (except [~g!err Exception]
         nil)))

  (defmacro! unwind-protect (protected &rest body)
    `(try
       ~protected
       (except [~g!err Exception]
         ~@body
         (raise ~g!err))))
  )


;; pipe utils
(eval-and-compile
  (defun nreplace-el (from to tree &optional guard)
    (loop
      ((tree tree)
       (guard guard))
      (for [i (range (len tree))]
        (setv el (get tree i))
        (cond/cl ((consp el)  (if (= (get el 0) guard)
                                  (continue)
                                  (recur el guard)))
                 ((and (is (type el) (type from)) (= el from)) (setf (get tree i) to)))))
    tree)

  (defmacro! => (&rest args)
    (let ((replaced (nreplace-el '_ g!it (cdr args) '=>))
          (cur `(let ((~g!it ~(get args 0)))
                     ~g!it)))
         (for [sexp (cdr args)]
           (setf cur (if (in g!it (flatten [sexp]))
                         `(let ((~g!it ~cur))
                               ~sexp)
                         (if (consp sexp)
                             (HyExpression (+ [(get sexp 0)] [cur] (cdr sexp)))
                             (+ (HyExpression [sexp]) [cur])))))
        cur)))

(eval-and-compile
  (defun slurp (path &optional (encoding "latin-1"))
    (.read (open path 'r :encoding encoding)))

  (defun slurpls (path &optional (delim None) (encoding "latin-1"))
    (with (fr (open path 'r :encoding encoding))
      (if delim
          (lfor
            line fr
            (if delim
               (.split (.strip line) delim)
               (.strip line)))
          (.readlines fr))))

  (defun barf (cont path)
    (with (fw (open path 'w))
      (.write fw cont)))

  (defun barfls (ls path &optional (delim None))
    (with (fw (open path 'w))
      (if delim
          (for [lst ls]
            (if delim
                (=> lst
                    (map str _)
                    (.join delim _)
                    (print :file fw))
                (print lst :file fw))))))
  )


(eval-and-compile
  (import [toolz.curried :as tz])
  
  (deftag t [expr]
    "Cast form to a tuple."
    `(tuple ~expr))

  (deftag a [ls]
    `(np.array (list ~ls)))
    
  (deftag l [ls]
    `(list ~ls))

  (deftag h [ls]
    `(HyExpression ~ls))
    
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

;; plt utilities
(eval-and-compile
    
  (require [hy.extra.anaphoric [ap-map]])

  (defun split-when (pred? lst)
    (setf indices #l(ap-map (.index lst it) (filter keyword? lst)))
    (pushl indices 0)
    (pushr indices (len lst))
    (lfor
      i (range 1 (len indices))
      (cut lst (get indices (dec i)) (get indices i)))
    )

  (defun keywords-to-doto (lst)
    (lfor i lst
      (=> (car i)
          (name)
          (+ "." _)
          (read-str)
          (, _ #*(cdr i))
          (HyExpression))))

  (defmacro plot (&rest body)
    `(progn
       (plt.plot ~@(car (split-when keyword? body)))
         (doto plt
           ~@(keywords-to-doto (cdr (split-when keyword? body)))))))


;; print utils
;; f-string inspired macro
(eval-and-compile
  (import re)

  (defun f-parser (s)
    (, (re.sub "{.*?}" "{}" s)
      #*(re.findall "{(.*?)}" s)))
  
  (deftag f [s]
    `(.format
       ~(car (f-parser s))
       ~@(map read-str (cdr (f-parser s))))))
  

