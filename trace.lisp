;;; package tracer
;;; Copyright 1998 Philip H. Chu
;;; Permission is granted to use, modify and distribute this code.
;;; Send comments and suggestions to philipchu@technicat.com

(defpackage "TRACE"
  (:use "COMMON-LISP")
  (:export "TRACE-ALL"))


(in-package "TRACE")

;;;

(defmethod trace-all ((package-list list))
  (dolist (p package-list)
    (trace-all (find-package p))))

(defmethod trace-all (p)
  (trace-all (find-package p)))

(defmethod trace-all ((p package))
  (do-external-symbols (sym p)
    (when (and (fboundp sym)
               (not (macro-function sym))
               (not (find-symbol (symbol-name sym) :common-lisp))
               #+allegro
               (not (find-symbol (symbol-name sym) :excl)))
      (eval `(trace ,sym)))))




