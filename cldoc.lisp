;;; package documenter
;;; Copyright 1998 Philip H. Chu
;;; Permission is granted to use, modify and distribute this code.
;;; Send comments and suggestions to philipchu@technicat.com

(defpackage "CLDOC"
  (:use "COMMON-LISP")
  (:export "DOC"))


(in-package "CLDOC")

(defmethod doc ((package-list list) directory)
  ""
  (dolist (package package-list)
    (doc package directory)))

(defmethod doc ((p symbol) directory)
  "generate doc for package"
  (doc (find-package p) directory))

(defmethod doc ((p package) directory)
  "generate doc for package"
  (let ((file (make-pathname :directory directory
                             :name (package-name p)
                             :type "html")))
  (with-open-file (f file :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (format f "<html><title>~a</title>~%<body>" (package-name p))
    (format f "~%<h1>~a</h1>" (package-name p))
    (print-package p f)
    (print-contents f)
    (print-macros p f)
    (print-classes p f)
    (print-functions p f)
    (format f "~%</body>~%</html>"))))

(defun print-contents (&optional (stream t))
  (format stream "~%<ul>")
  (format stream "~%<li><a href=\"#macros\">Macros</a>")
  (format stream "~%<li><a href=\"#functions\">Functions</a>")
  (format stream "~%<li><a href=\"#genfuns\">Generic Functions</a>")
  (format stream "~%<li><a href=\"#classes\">Classes</a>")
  (format stream "~%</ul>")
  )

(defun print-package (p &optional (stream t))
  (let ((used (package-use-list p)))
    (when used
      (format stream "~%uses: ")
      (dolist (subp used)
        (format stream " <a href=\"~a.html\">~a</a>"
                (package-name subp) (package-name subp)))))
  (let ((used (package-nicknames p)))
    (when used
      (format stream "~%<p>nicknames: ")
      (dolist (subp used)
        (format stream " ~a" subp)))))

(defun print-functions (p &optional (stream t))
  (format stream "~%<h3><a link=\"functions\">Functions</a></h3>")
  (do-external-symbols (sym p)
    (when (and (fboundp sym)
               (symbol-function sym))
      (let ((fun (symbol-function sym)))
        (when (not (or (macro-function sym)
                       (typep fun 'standard-generic-function)))
          (let ((doc (or (documentation sym 'function)
                         "No documentation.")))
            (print-name "function" sym stream)
            (print-args fun stream)
            (print-doc doc stream)))))))


(defun print-generic-functions (p &optional (stream t))
  (format stream "~%<h3>Functions</h3>")
  (do-external-symbols (sym p)
    (when (and (fboundp sym)
               (symbol-function sym))
      (let ((fun (symbol-function sym)))
        (when (typep fun 'standard-generic-function)
          (let ((doc (or (documentation sym 'function)
                         "No documentation.")))
            (print-name "generic function" sym stream)
            (print-args fun stream)
            (print-doc doc stream)))))))



(defun print-classes (p &optional (stream t))
  (format stream "~%<h3>Classes</h3>")
  (do-external-symbols (sym p)
    (let ((class (find-class sym nil)))
      (when (typep class 'standard-class)
        (let ((doc (or (documentation class)
                       "no documentation")))
          (print-name "class" sym stream)
          (print-doc doc stream))))))


(defun print-macros (p &optional (stream t))
    (format stream "~%<h3>Macros</h3>")
    (do-external-symbols (sym p)
      (let ((fun (macro-function sym)))
        (when fun
          (let ((doc (or (documentation sym 'function)
                         "No documentation.")))
            (print-name "macro" sym stream)
            (print-args fun stream)
            (print-doc doc stream))))))

(defun print-name (type sym &optional (stream t))
  (format stream "~%<p>~%<em>~a</em> <b>~a</b>" type sym))

(defun print-doc (doc &optional (stream t))
  (format stream "~%<p>~%<pre>~a</pre>" doc))

(defun print-args (fun &optional (stream t))
#+allegro
  (format stream " ~a" (excl:arglist fun))
  ;(format stream " ( ~{~a ~})" (excl:arglist fun))
  )


