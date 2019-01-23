(require [hylab.cl [*]])

;; sharp macros
(eval-and-compile
  
  (defmacro! pr (o!arg)
    `(do
       (print ~o!arg)
       ~o!arg))

  (deftag p [code]
    "debug print"
    `(pr ~code))

  (deftag r [regex]
    "regexp"
    `(do
       (import re)
       (re.compile ~regex)))

  (deftag g [path]
    "glob"
    `(do
       (import glob)
       (glob.glob ~path)))   
  
  (import os fnmatch)
  
  (defun path-genr (fname dir)
    (for [tp (os.walk dir)]       
      (for [f (get tp 2)]
        (if (fnmatch.fnmatch f fname)
            (yield (os.path.join (get tp 0) f))))))

  (deftag f [dir-fname]
    "find file name"
    `(path-genr ~(get dir-fname 1) ~(get dir-fname 0)))
  
  (deftag sh [command]
    `(do
       (import subprocess)
       (setf proc (subprocess.Popen ~command
                                    :shell True
                                    :stdin subprocess.PIPE
                                    :stdout subprocess.PIPE
                                    :stderr subprocess.PIPE)
             (, stdout-data stderr-data) (proc.communicate))
       (print (.decode  stderr-data "ascii"))
       (.decode stdout-data "ascii")))
  
  (defun ts ()
    "timestamp"
    (do
      (import datetime)       
      (datetime.datetime.now)))
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
         cur))
  
  (defmacro first_ (ls)
    `(get ~ls 0))
  
  (defmacro last_ (ls)
    `(get ~ls -1))
  
  (defmacro spl/ (str)
    `(.split str "/"))
  
  (defmacro spl/t (str)
    `(.split str "/t"))
  
  (defmacro splc (str)
    `(.split str ","))
  
  (defmacro getext (str)
    `(=> ~str spl/ last_))
  
  (defmacro getid (str)
    `(=> ~str spl/ first_))
  
  )


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
  
  

