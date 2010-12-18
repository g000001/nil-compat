;;;; /home/mc/lisp/work/nil-compat/nil-compat.lisp

(in-package :si)

;; P70
;; 9.6 Symbol Concatenation
(defun package-symbolconc (package-spec &rest frobs)
  (values
   (intern
    (with-output-to-string (out)
      (dolist (elt frobs)
        (unless (typep elt '(or symbol string fixnum character))
          (error "The value ~A is not of type (OR SYMBOL STRING FIXNUM CHARACTER)."
                 elt))
        (let ((*print-base* 10.))
          (princ elt out))))
    package-spec)))

(define-compiler-macro package-symbolconc (&whole form package-spec &rest frobs)
  (flet ((quoted-symbol-p (x)
                          (and (consp x)
                               (eq 'quote (first x))
                               (symbolp (second x)))))
    (if (every (lambda (x)
                 (or (typep x '(or fixnum string character))
                     (quoted-symbol-p x)))
               frobs)
        `(package-symbolconc
          ,package-spec
          ,(with-output-to-string (out)
             (dolist (elt frobs)
               (let ((*print-base* 10.))
                 (princ (if (quoted-symbol-p elt)
                            (eval elt)
                            elt)
                        out)))))
        form)))

(in-package #:nil)

;; P70
;; 9.6 Symbol Concatenation
(defun symbolconc (&rest frobs)
  (declare (dynamic-extent frobs))
  (apply #'si:package-symbolconc *package* frobs))

;; P84
;; Maclisp compat
(defun haulong (integer)
  (values (ceiling (log (1+ (abs integer)) 2))))

;; haipart

;; 10.1 Fixnum-Only Arithmetic
(macrolet ((define-fixnum-only-arithmetic-function-rest (name)
             `(defun ,(symbolconc name "&") (number &rest more-numbers)
                (declare (optimize (safety 0) (speed 3)))
                (declare (fixnum number))
                (flet ((op (x y)
                           (declare (fixnum x y))
                           (the fixnum (,name x y))))
                  (reduce #'op more-numbers :initial-value number))))
           (define-fixnum-only-arithmetic-function (name &rest vars)
             (let ((name (symbolconc (if (symbolp name) name (first name))
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
  (define-fixnum-only-arithmetic-function (\\ gcd) fixnum1 fixnum2) ;renamed
  (define-fixnum-only-arithmetic-function 1+ fixnum)
  (define-fixnum-only-arithmetic-function 1- fixnum)
  (define-fixnum-only-arithmetic-function abs fixnum)
  (define-fixnum-only-arithmetic-function signum fixnum)
  (define-fixnum-only-arithmetic-function (^ expt) fixnum1 fixnum2))

;; 13.1 String Coercion
(defun to-string (frobozz)
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
(defun ?format (destination control-string &rest args)
  (apply #'format (if destination
                      *terminal-io*
                      *standard-output*)
                  control-string
                  args))


;; P202
;; 19.8.1.2 Merging and Defaulting
(defvar *scratch-pathname-defaults* #p"/tmp/")

(in-package :utils)

;; 21.5.4 Related Utilities
;; P228
(defun print-into-file (expression &optional (filename "gazonk.del"))
  (with-open-file (out (merge-pathnames nil::*scratch-pathname-defaults*
                                        filename)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print expression out)))

(defun pp-into-file (expression &optional (filename "gazonk.del"))
  (with-open-file (out (merge-pathnames nil::*scratch-pathname-defaults*
                                        filename)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (pprint expression out)))
