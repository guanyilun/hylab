(import [hylab.core [*]])
(require [hylab.core [*]])

;; test setf and defun
(defun test_setf ()
  (setf x 10)
  (defun plus-one (x)
    (inc x))
  (assert (= (plus-one x) 11)))

;; test defparameter
(defun test_defparameter ()
  (defparameter *global-var* 10)
  (assert (= *global-var* 10)))

;; test np import and nget shape
(defun test_np ()
  (setf x (np.random.randn 100 100 100))
  (assert (= (. (nget x 0 1 :10) shape)
             (, 10)))
  (assert (= (. (nget x ... 0:10) shape)
             (, 100 100 10)))
  (assert (= (. (nget x ... None) shape)
             (, 100 100 100 1))))

;; test numpy slicing
(defun test_np_slice ()
  (setf a (.reshape (np.arange 36) (, 6 6)))
  (assert (np.all (= (nget a (, 0 1 2 3 4) (, 1 2 3 4 5))
                     (np.array [1 8 15 22 29]))))
  (assert (np.all (= (nget a 3: (, 0 2 5))
                     (np.array [[18 20 23] [24 26 29] [30 32 35]]))))
  (setf mask (np.array (, 1 0 1 0 0 1) :dtype bool))
  (assert (np.all (= (nget a mask 2)
                     (np.array [2 14 32]))))
  (assert (np.all (= (nget a ::2 3)
                     (np.array [3 15 27]))))
  (assert (np.all (= (nget a ::2 3 None)
                     (np.array [[3] [15] [27]]))))
  (assert (np.all (= (nget a ... 0)
                     (np.array [0 6 12 18 24 30])))))

;; test python -> hy translation macro
(defun test_python_hy_translation ()
  (assert (np.all (= (let ((x (np.linspace 0 10 10)))
                       ($ "np.sin(x)**2+np.cos(x)**2"))
                     (np.ones 10)))))

;; test lambda expression
(defun test_lambda_expression ()
  (assert (= #l(map (lambda (x) (1+ x)) [1 2 3])
             [2 3 4]))
  (assert (= #l(map (λ (x) (1+ x)) [1 2 3])
             [2 3 4])))

;; test f-string macro
(defun test_fstring ()
  (setf age 99
        height 2.2)
  (assert (= #f"I am {age} years old, and I am {height} m tall"
             "I am 99 years old, and I am 2.2 m tall"))
  (assert (= #f"I am {(inc age)} years old, and I am {(round (* np.pi height) 2)} m tall"
             "I am 100 years old, and I am 6.91 m tall"))
  (assert (= #f"Pi = {np.pi:8.2f}"
             "Pi =     3.14"))
  (setf n "cat")
  (assert (= #f"Animal: {n:5}"
             "Animal: cat  "))
  (assert (= #f"Animal: {n:>5}"
             "Animal:   cat"))
  (assert (= #f"Animal: {n:^5}"
             "Animal:  cat "))
  (assert (= #f"Animal: {n:*^5}"
             "Animal: *cat*")))

;; test itr
;; TODO: Need to be fixed for hy==0.17.0
;; (defun test_itr ()
;;   (assert (= (itr
;;                (for i in (range 10))
;;                (when (= (% i 2) 0)
;;                  (collect (* 10 i))))
;;              [0 20 40 60 80]))
;;   (assert (= (itr
;;                (for i in (range 10))
;;                (maximize i))
;;              9))
;;   (assert (= (itr
;;                (for i in (range 10))
;;                (count (= (% i 2) 0)))
;;              5)))
