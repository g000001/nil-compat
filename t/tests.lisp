(defpackage :nil-test
  (:use :cl :nil :5am))

(in-package :nil-test)

(do-symbols (s :nil)
  (shadowing-import s))

(def-suite nil-test-all)

(in-suite nil-test-all)

(test |PACKAGE-SYMBOLCONC|
  (is (eq (si:package-symbolconc :nil-test :foo :bar :baz 1)
          'nil-test::foobarbaz1))
  (is (eq (si:package-symbolconc :nil-test "FOO" :bar :baz 1)
          'nil-test::foobarbaz1))
  (is (eq (si:package-symbolconc :nil-test "FOO" :bar :baz #\1)
          'nil-test::foobarbaz1)))

(test |SYMBOLCONC|
  (is (eq (symbolconc :foo :bar :baz 1)
          'nil-test::foobarbaz1))
  (is (eq (symbolconc "FOO" :bar :baz 1)
          'nil-test::foobarbaz1))
  (is (eq (symbolconc  "FOO" :bar :baz #\1)
          'nil-test::foobarbaz1)))

(run! 'nil-test-all)


