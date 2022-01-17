(in-package :cl-user)

(defpackage :cl-roaring-asd
  (:use :cl :asdf))

(in-package :cl-roaring-asd)

(defsystem :cl-roaring
  :version "EXPERIMENTAL-1"
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements an interface to the C-language Roaring Bitmap API."
  :serial t
  :depends-on (:cffi :trivial-garbage)  ;alias :tg
  :components ((:file "package")
               (:file "cl-roaring")))
