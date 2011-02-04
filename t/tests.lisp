(defpackage :nil-test
  (:use :cl :nili :5am))

(in-package :nil-test)

(do-symbols (s :nili)
  (shadowing-import s))

(def-suite nil-test-all)

(in-suite nil-test-all)

(test package-symbolconc
  (is (eq (package-symbolconc :nil-test :foo :bar :baz 1)
          'foobarbaz1))
  (is (eq (package-symbolconc :nil-test "FOO" :bar :baz 1)
          'foobarbaz1))
  (is (eq (si:package-symbolconc :nil-test "FOO" :bar :baz #\1)
          'foobarbaz1)))

(test nil:symbolconc
  (is (eq (nil:symbolconc :foo :bar :baz 1)
          'foobarbaz1))
  (is (eq (nil:symbolconc "FOO" :bar :baz 1)
          'foobarbaz1))
  (is (eq (nil:symbolconc  "FOO" :bar :baz #\1)
          'foobarbaz1)))

(test pair
  (is-false (typep '() 'pair))
  (is-true (typep '(a) 'pair))
  (is-true (pairp '(a))))

(test let
  (is (null (nil:let () )))
  (signals (error)
    (nil:let (x x) x))
  (is (equal (nil:let (x y z) (list x y z))
             (list nil nil nil)))
  (is (equal (nil:let ((x) (y) (z)) (list x y z))
             (list nil nil nil)))
  (is (equal (nil:let ((x 1) (y 2) (z 3)) (list x y z))
             (list 1 2 3)))
  (is (equal (nil:let (((x.x x.y)
                        (list 1.1 1.2))
                       (y 2)
                       (z 3))
               (list x.x x.y y z))
             (list 1.1 1.2 2 3)))
  (is (equal (nil:let (((x.x x.y)
                        (list 1.1 1.2))
                       y
                       z)
               (list x.x x.y y z))
             (list 1.1 1.2 nil nil)))
  (signals (error)
    (nil:let (((x x)
               (list 1.1 1.2))
              y
              z)
      (list x x y z))))


#|(let-expander-1 '(
                  (((x.x x.y)
                    (list 1.1 1.2))
                   y
                   z)
                  (list x.x x.y y z)))|#
;=> ((LAMBDA (#:G2936 Y Z X.Y X.X)
;      (SETQ X.X (CAR #:G2936))
;      (SETQ X.Y (CAR (CDR #:G2936)))
;      (LIST X.X X.Y Y Z))
;    (LIST 1.1 1.2) NIL NIL NIL NIL)

#|(|LET.find-rightmost| '())|#


(run! 'nil-test-all)

