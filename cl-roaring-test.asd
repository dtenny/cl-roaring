(in-package :cl-user)

(defpackage :cl-roaring-test-asd
  (:use :cl :asdf))

(in-package :cl-roaring-test-asd)

(defsystem :cl-roaring-test
  :version "EXPERIMENTAL-1"
  :license "MIT"
  :author "Dave Tenny"
  :description "tests for cl-roaring"
  :depends-on (:cl-roaring :fiveam :alexandria)
  :components ((:file "cl-roaring-test")))
