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

  ;; for mere clarity 
  (defmacro defparameter (&rest args)
    `(setv ~@args))

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

  ;; unicode alias for lambda
  (defmacro λ (lambda-list &rest body)
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
  
  (deftag sym [expr]
    `(HySymbol ~expr))

  (deftag . [sym]
    `(HySymbol (+ "." ~sym)))
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
    "Parse f-string fields and opts"
    (, (.format (re.sub "{(.*?)}" "{}" s)
             #*(lfor m (re.findall "{(.*?)}" s)
                     (if (in ":" m)
                         (+ "{:" (.join "" (cut (.split m ":") -1)) "}")
                         "{}")))
       #*(lfor m (re.findall "{(.*?)}" s)
               (if (in ":" m)
                   (.join "" (cut (.split m ":") 0 -1))
                   m))))
  
  (deftag f [s]
    `(.format
       ~(car (f-parser s))
       ~@(map read-str (cdr (f-parser s))))))



;; iterate macro
;; borrowed from https://github.com/riktor/hyiter
;; commit: db2757327da313e7147131230c80d8bcef60b516

(defun matchp (clause ptn)
  (if (not (= (length clause) (length ptn)))
      False
      (let ((flag True))
        (for [tp (zip clause ptn)]
          (when (and (not (= (get tp 1) '_)) (not (= (get tp 0) (get tp 1)))) 
            (setf flag False)
            (break)))
        flag)))

(defun flatten-destruc (ls)
  (let ((acc ()))
    (for [el ls]
      (if (consp (car el))
          (.extend acc (flatten-destruc el))
          (.append acc el)))
    acc))

(defun parse-clause (clause parsed ret-sym)   
  (cond/cl
    ((keyword? clause)
     (setf (get parsed :loop-tag) clause))
    ;; initially clause
    ((= (car clause) 'initially)
      (setf (get parsed :initially) (cdr clause)))

    ;; finally clause
    ((= (car clause) 'finally)
      (setf (get parsed :finally) (cdr clause)))
    
    ;; with clause
    ((matchp clause '(with _ = _))
      (if (not (consp (get clause 1)))
          (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
          (setf (get parsed :with)
                (append (nreverse (flatten-destruc (destruc (get clause 1) (get clause 3) 0))) 
                        (get parsed :with)))))
    
    ;; for = clause
    ((matchp clause '(for _ = _))
      (if (not (consp (get clause 1)))
          (progn
            (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
            (push `(~(get clause 1) ~(get clause 3)) (get parsed :for)))
          (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) (get clause 3) 0)))))
            (setf (get parsed :with)
                  (append destr (get parsed :with)))
            (setf (get parsed :for)
                  (append destr (get parsed :for))))))
    
    ((matchp clause '(for _ = _ then _))
      (let ((__cur__  (gensym)))
        (if (not (consp (get clause 1)))
            (progn
              (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
              (push `(~(get clause 1) ~(get clause 5)) (get parsed :for)))
            (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) __cur__ 0)))))
              (push `(~__cur__ (first ~(get clause 3)) ) (get parsed :with))
              (setf (get parsed :with)
                    (append destr (get parsed :with)))
              (push `(~__cur__ (first ~(get clause 5)) ) (get parsed :with))
              (setf (get parsed :for)
                    (append destr (get parsed :for)))))))

    ;; for in clause
    ((matchp clause '(for _ in _))
      (let ((__it__  (gensym "it"))
             (__cur__ (gensym "cur")))
        (push `(~__it__ (iter ~(get clause 3))) (get parsed :with))
        (push `(~__cur__ (next ~__it__) ) (get parsed :with))
        (if (not (consp (get clause 1)))
            (progn                 
              (push `(~(get clause 1) ~__cur__) (get parsed :with))
              (push `(~__cur__ (next ~__it__)) (get parsed :for))
              (push `(~(get clause 1) ~__cur__) (get parsed :for)))
            (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) __cur__ 0)))))
              (setf (get parsed :with)
                    (append destr (get parsed :with)))
              (push `(~__cur__ (next ~__it__)) (get parsed :for))
              (setf (get parsed :for)
                    (append destr (get parsed :for)))))))

    ;; for from clause    
    ((matchp clause '(for _ from _))      
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for)))
    ((matchp clause '(for _ from _ below _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for))
      (push `(not (< ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ upto _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for))
      (push `(not (<= ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ below _ by _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) ~(get clause 7))) (get parsed :for))
      (push `(not (< ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ upto _ by _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) ~(get clause 7))) (get parsed :for))
      (push `(not (<= ~(get clause 1) ~(get clause 5))) (get parsed :break)))

    ((matchp clause '(repeat _))
      (let ((__counter__ (gensym "counter")))
        (push `(~__counter__ 0) (get parsed :with))
        (push `(~__counter__ (+ ~__counter__ 1)) (get parsed :for))
        (push `(not (< ~__counter__ ~(get clause 1))) (get parsed :break))))
    
    ((matchp clause '(drop _))
      (let ((__counter__ (gensym "counter")))
        (push `(~__counter__ 0) (get parsed :with))
        (push `(~__counter__ (+ ~__counter__ 1)) (get parsed :for))
        (push `(< ~__counter__ ~(get clause 1)) (get parsed :drop))))
    ((matchp clause '(dropwhile _))
      (push (get clause 1) (get parsed :drop)))

    ((matchp clause '(while _))
      (push `(not ~(get clause 1)) (get parsed :break)))

    ((matchp clause '(until _))
      (push (get clause 1) (get parsed :break)))
    
    (True
      (push clause (get parsed :body)))))


(defun parse-clauses (clauses ret-sym)
  (let ((parsed-clauses {:with nil
                         :initially nil
                         :for nil
                         :body nil
                         :finally nil
                         :break nil
                         :drop nil
                         :loop-tag nil}))
    (for [el clauses]
      (parse-clause el parsed-clauses ret-sym))
    (nreverse (get parsed-clauses :for))
    (nreverse (get parsed-clauses :with))
    (nreverse (get parsed-clauses :break))
    (nreverse (get parsed-clauses :body))
    parsed-clauses))

(defmacro! nreplace-clauses (tree condition replace)
  `(let ((!acc! ())
          (~g!orig-tree ~tree)
          (~g!replaced [False]))
     (loop     
       ((~g!tree ~g!orig-tree))       
       (progn
         (for [~g!ind (range (len ~g!tree))]         
           (setf !el! (get ~g!tree ~g!ind))         
           (when ~condition           
             (setf (get ~g!tree ~g!ind) ~replace
                   (get ~g!replaced 0) True))
           (when (and (consp !el!) (not (= (get !el! 0) 'itr)))
             (recur !el!)))))
     [~g!orig-tree !acc! (get ~g!replaced 0)]))

(defun replace-collect (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'collect))
                    `(~(cond/cl
                         ((matchp !el! '(collect _)) update-fn-sym)
                         ((matchp !el! '(collect _ into _))
                           (let ((sym (gensym 'update)))
                             (.append !acc! [(get !el! 3) sym])
                             sym)))
                      ~(get !el! 1))))

(defun replace-append (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'append))
                    `(~(cond/cl
                         ((matchp !el! '(append _)) update-fn-sym)
                         ((matchp !el! '(append _ into _))
                           (let ((sym (gensym 'update)))
                             (.append !acc! [(get !el! 3) sym])
                             sym)))
                      ~(get !el! 1))))

(defun replace-maximize (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'maximize))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(maximize _)) ret-sym)
                                 ((matchp !el! '(maximize _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (max ~sym ~(get !el! 1))))))

(defun replace-minimize (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'minimize))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(minimize _)) ret-sym)
                                 ((matchp !el! '(minimize _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (min ~sym ~(get !el! 1))))))

(defun replace-sum (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'sum))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(sum _)) ret-sym)
                                 ((matchp !el! '(sum _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (+ ~sym ~(get !el! 1))))))

(defun replace-count (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'count))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(count _)) ret-sym)
                                 ((matchp !el! '(count _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(when ~(get !el! 1) 
                         (setv ~sym (+ ~sym 1))))))

(defun replace-return (body)
  (first (nreplace-clauses body
                           (and (typep !el! HyExpression)
                                (or (= (car !el!) 'return) (= (car !el!) 'return-from)))          
                           (cond/cl
                             ((matchp !el! '(return _))
                               `(raise (hyiter.core.Return ~(get !el! 1))))
                             ((matchp !el! '(return-from _ _))
                               `(raise (hyiter.core.TaggedReturn ~(get !el! 1) ~(get !el! 2))))))) )

(defun replace-continue (parsed-for body)
  (first (nreplace-clauses body
                           (and (typep !el! HyExpression) (= (car !el!) 'continue))
                           `(do
                              (setv ~@(flatten-1 parsed-for))
                              (continue)))))


(defclass Return [Exception]
  (defn __init__ [self val]
    (setf (. self val) val)))

(defclass TaggedReturn [Exception]
  (defn __init__ [self  tag val]
    (setf (. self tag) tag)
    (setf (. self val) val)))

(defun get-init-val (replacer)
  (cond/cl
    ((in replacer [replace-collect replace-append]) [])
    ((= replacer replace-minimize) 10000000000000)
    ((= replacer replace-maximize) -10000000000000)
    ((in replacer [replace-sum replace-count]) 0)))

(defun get-update-fn (replacer acc-sym)
  (cond/cl
    ((= replacer replace-collect) `(. ~acc-sym append))
    ((= replacer replace-append) `(. ~acc-sym extend))))

(defmacro/g! itr (&rest clauses)  
  (let ((g!parsed (parse-clauses clauses g!ret)))
    (let ((body (get g!parsed :body))
           (accs nil)
           (res nil)
           (init-var nil)
          (update-fn nil))
      (for [el [replace-collect replace-append
                replace-minimize replace-sum replace-maximize 
                replace-count]]   
        (setf res (el g!ret g!update body)
              body (get res 0)
              acc-ls (get res 1)
              flag (get res 2))
        (when flag
          (setf accs (append accs (mapcan (lambda (ls)
                                            (setf init-list [(get ls 0) (get-init-val el)])
                                            (when (get ls 1)
                                              (.extend init-list [(get ls 1) (get-update-fn el (get ls 0))]))
                                            init-list)
                                          acc-ls))
                init-var (get-init-val el)
                update-fn (get-update-fn el g!ret))))
      `(do
         (import hyiter.core)
         (try       
           (do         
             (setv ~g!tag ~(get g!parsed :loop-tag))
             (setv ~g!ret ~init-var
                   ~g!update ~update-fn)             
             ~@(replace-return (get g!parsed :initially))             
             (setv ~@accs)
             (try
               (do
                 (setv ~@(flatten-1 (get g!parsed :with)))             
                 (while True
                   ~(when (get g!parsed :drop)
                      `(when (and ~@(get g!parsed :drop))
                         (setv ~@(flatten-1 (get g!parsed :for)))
                         (continue)))               
                   ~@(replace-continue (get g!parsed :for)
                                       (replace-return body)) 
                   (setv ~@(flatten-1 (get g!parsed :for)))
                   (when (or ~@(get g!parsed :break))
                     (break))))
               (except [e StopIteration]
                 None))         
             ~@(replace-return (get g!parsed :finally)) 
             ~g!ret)
           (except [r hyiter.core.Return]
             (. r val))
           (except [tr hyiter.core.TaggedReturn]
             (if (= (. tr tag) ~g!tag)
                 (. tr val)
                 (raise tr))))))))
