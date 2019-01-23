;; Codes taken from
;; https://github.com/riktor/hycl

;; Created by: riktor
;; Modified by: guanyilun

(import
  [hy [HyKeyword]]
  [hy.contrib.hy-repr [hy-repr]])

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
    ;; Beware of humongous stdout(in repl)!!
    `(do
       (setv ~@(get args (slice 0 (- (len args) 2))))       
       (setv ~@(get args (slice -2 None)))
       ~(get args -2)))
  
  (defmacro typep (obj objtype)
    `(is (type ~obj) ~objtype))

  ;; todo: &optional cannnot accept () form. (now only stupid [])
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
  ;; ------------------- DO NOT SET nil!!-----------------------------
  (setf nil (HyExpression ()))
  ;; ------------------- DO NOT SET nil!!-----------------------------

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

  (defmacro when (condition &rest body)
    `(if ~condition
         (progn
           ~@body)))

  (defmacro unless (condition &rest body)
    `(if (not ~condition)
         (progn
           ~@body)))  
  
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


