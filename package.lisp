;;;; /home/mc/lisp/work/nil-compat/package.lisp

(defpackage #:nil
  (:use #:cl))

(defpackage #:si
  (:use #:cl)
  (:export :package-symbolconc))

(defpackage #:utils
  (:use #:cl #:nil))