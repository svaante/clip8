(defpackage clip8/tests/main
  (:use :cl
        :clip8
        :rove))
(in-package :clip8/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clip8)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
