(require [hylab.core [*]])

;; sharp macros
(eval-and-compile
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

