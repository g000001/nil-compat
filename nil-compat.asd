;;;; /home/mc/lisp/work/nil-compat/nil-compat.asd

(asdf:defsystem #:nil-compat
  :components ((:file "package")
               (:file "nil-si" :depends-on ("package"))
               (:file "nil-compat" :depends-on ("nil-si"))
               (:file "LET" :depends-on ("nil-si"))
               ))

