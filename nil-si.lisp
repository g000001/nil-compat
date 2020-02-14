(in-package :nili)

;; types
(deftype pair () 'cons)

(deftype vector-s () 'vector)

(deftype extend () 'vector)

(deftype constant (&rest args)
  (declare (ignore args))
  `(satisfies constantp))

(defun memq (item list)
  (declare (list list))
  (member item list :test #'eq))

(defun delq (item list)
  (declare (list list))
  (delete item list :test #'eq))

(defun vector-length (vec)
  (declare (vector vec))
  (length vec))

(defun pairp (obj)
  (typep obj 'pair))

(defmacro TYPECASEQ (item &body clauses)
  `(typecase ,item
     ,@(mapcar (lambda (c)
                 (if (consp (car c))
                     `((OR ,@(car c)) ,@(cdr c))
                     c))
               clauses)))

(defmacro FIXNUMP (w)
  `(TYPEP ,w 'FIXNUM))

(defun vref (vector index)
  (declare (vector vector))
  (aref vector index))

;; P70
;; 9.6 Symbol Concatenation
(defun package-symbolconc (package-spec &rest frobs)
  (values
   (intern
    (with-standard-io-syntax
      (with-output-to-string (out)
        (dolist (elt frobs)
          (unless (typep elt '(or symbol string fixnum character))
            (error "The value ~A is not of type (OR SYMBOL STRING FIXNUM CHARACTER)."
                   elt))
          (let ((*print-base* 10.))
            (princ elt out)))))
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
