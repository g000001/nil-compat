;;;; /home/mc/lisp/work/nil-compat/nil-compat.lisp

(in-package #:nili)

;; P70
;; 9.6 Symbol Concatenation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nil:symbolconc (&rest frobs)
    (declare (dynamic-extent frobs))
    (apply #'package-symbolconc *package* frobs)))

;; P84
;; Maclisp compat
(defun nil:haulong (integer)
  (values (ceiling (log (1+ (abs integer)) 2))))

;; haipart

;; 10.1 Fixnum-Only Arithmetic
(macrolet ((define-fixnum-only-arithmetic-function-rest (name)
               `(defun ,(package-symbolconc :nil name "&") (number &rest more-numbers)
                  (declare (optimize (safety 0) (speed 3)))
                  (declare (fixnum number))
                  (flet ((op (x y)
                           (declare (fixnum x y))
                           (the fixnum (,name x y))))
                    (reduce #'op more-numbers :initial-value number))))
           (define-fixnum-only-arithmetic-function (name &rest vars)
               (let ((name (package-symbolconc :nil
                                               (if (symbolp name) name (first name))
                                               "&"))
                     (fun (if (symbolp name) name (second name))))
                 `(progn
                    (declaim (inline ,name))
                    (defun ,name (,@vars)
                      (declare (optimize (safety 0) (speed 3)))
                      (declare (fixnum ,@vars))
                      (the fixnum (,fun ,@vars)))))))
  ;; 10.11.2 Arithmetic Operations
  (define-fixnum-only-arithmetic-function-rest +)
  (define-fixnum-only-arithmetic-function-rest -)
  (define-fixnum-only-arithmetic-function-rest *)
  (define-fixnum-only-arithmetic-function-rest /)
  (define-fixnum-only-arithmetic-function-rest <)
  (define-fixnum-only-arithmetic-function-rest >)
  (define-fixnum-only-arithmetic-function-rest <=)
  (define-fixnum-only-arithmetic-function-rest =)
  (define-fixnum-only-arithmetic-function-rest /=)
  (define-fixnum-only-arithmetic-function-rest >=)
  (define-fixnum-only-arithmetic-function-rest logxor)
  (define-fixnum-only-arithmetic-function (\\ rem) fixnum1 fixnum2) ;renamed
  (define-fixnum-only-arithmetic-function 1+ fixnum)
  (define-fixnum-only-arithmetic-function 1- fixnum)
  (define-fixnum-only-arithmetic-function abs fixnum)
  (define-fixnum-only-arithmetic-function signum fixnum)
  (define-fixnum-only-arithmetic-function logtest fixnum1 fixnum2)
  (define-fixnum-only-arithmetic-function (^ expt) fixnum1 fixnum2))


;; 13.1 String Coercion
(defun nil:to-string (frobozz)
  (etypecase frobozz
    (null (make-string 0))
    (string frobozz)
    (bit-vector
     (map 'string
          (lambda (x) (character (princ-to-string x)))
          frobozz))
    ((or list vector)
     (coerce frobozz 'string))))

;; 19.6 Format
(defun nil:?format (destination control-string &rest args)
  (apply #'format (if destination
                      *terminal-io*
                      *standard-output*)
                  control-string
                  args))


;; P202
;; 19.8.1.2 Merging and Defaulting
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar nil:*scratch-pathname-defaults* #p"/tmp/"))

(in-package :utils)

;; 21.5.4 Related Utilities
;; P228
(defun print-into-file (expression &optional (filename "gazonk.del"))
  (with-open-file (out (merge-pathnames nil:*scratch-pathname-defaults*
                                        filename)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print expression out)))

(defun pp-into-file (expression &optional (filename "gazonk.del"))
  (with-open-file (out (merge-pathnames nil:*scratch-pathname-defaults*
                                        filename)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (pprint expression out)))
