(require [hylab.core [*]])

(eval-and-compile
  (deftag bp []
    ;; breakpoint
    `(do (import ptpdb) (ptpdb.set_trace)))

  (import [io [StringIO]]
          traceback
          sys
          code
          [IPython.terminal.embed [InteractiveShellEmbed :as ise]]
          [IPython.lib.pretty [pretty]])

  (defclass CallStackViewer [object]
    (defun __init__ (self tb)
      (setf self.tb tb
            self.frames []
            self.injected-symbols (set ["dv" "get_locs"]))
      (while tb.tb-next
        (setf tb tb.tb-next)
        (.append self.frames tb.tb-frame))
      (setf self.last-frame (get self.frames -1)))

    (defun get-locs (self &optional (n 5))
      (setf locs [])
      (for [frame (get self.frames (slice (- n) None))]
        (setf code frame.f-code
              args (dict-comp
                     arg (get frame.f-locals arg)
                     (arg (get code.co-varnames (slice None code.co-argcount))))
              loc-vars (dict-comp
                         k v
                         ((, k v) (.items frame.f-locals))
                         (if (not-in k self.injected-symbols))))
        (.append locs (, code.co-name args loc-vars)))
      locs))

  (defun debug (f)
    ;; postmortem (in failed call stack)
    ;; 1. Don't use in hy-mode repl
    ;; 2. your script will fail for circular importing in IPython/core/completer.py. so run a script with $ hy -c "(import script)(script.main)"
    (with-decorator (ft.wraps f)
      (defun wrapper (&rest args &kwargs kwargs)
        (try
          (f #*args #**kwargs)
          (except [e Exception]
            (print "Debug mode activated")
            (setf (, type value tb) (sys.exc-info)
                  buf (StringIO))
            (traceback.print-exc :file buf)
            (setf stb (pr (.getvalue buf))
                  dv (CallStackViewer tb)
                  frame dv.last-frame
                  ns frame.f-locals
                  (get ns 'dv) dv
                  (get ns 'get-locs) (lambda (&optional [n 5]) (dv.get-locs :n n)))
            (.mainloop (ise) :local-ns ns)))))
    wrapper)

  (deftag d [function-defininition-form]
    ;; Try this! Enjoy!
    ;; #d
    ;; (defun test ()
    ;;   (setf a 10)
    ;;   (/ 1 0))
    `(with-decorator debug
       ~function-defininition-form))

  (defmacro me (sexp)
    ;; use it with this emacs-lisp-command)
    ;; (defun pp-macroexpand ()
    ;;   (interactive "*")
    ;;   (when (get-buffer "*Hy Macroexpand*")
    ;;     (kill-buffer "*Hy Macroexpand*"))
    ;;   (let ((beg nil)
    ;;          (end nil))
    ;;     (beginning-of-defun)
    ;;     (setf beg (point))
    ;;     (end-of-defun)
    ;;     (setf end (point))
    ;;     (let ((sexp (car (read-from-string
    ;;                        (buffer-substring beg end))))
    ;;            (buf (get-buffer-create "*Hy Macroexpand*")))
    ;;       (pp sexp buf)
    ;;       (display-buffer buf))))
    `(=> ~sexp
         macroexpand-1
         hy-repr
         (.replace ";" "")
         (.replace "(." "(!dot")
         (get _ (slice 1 (len _)))
         print))
  )
