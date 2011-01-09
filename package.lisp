;;;; /home/mc/lisp/work/nil-compat/package.lisp

(defpackage #:nil
  (:use #:cl)
  (:shadow :let :let*)
  ;; LET
  (:export :let :let* :desetq)
  ;; nil-compat
  (:export :symbolconc
           :haulong
           :+&
           :-&
           :*&
           :/&
           :\\&
           :1+&
           :1-&
           :abs&
           :signum&
           :^&
           :to-string
           :?format
           ))

(defpackage #:si
  (:use #:cl)
  (:export :package-symbolconc))

(defpackage #:utils
  (:use #:cl))