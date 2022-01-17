(in-package :cl-user)

(defpackage :cl-roaring
  (:use :cl :cffi)
  (:shadow #:remove #:subsetp #:intersection #:nintersection #:union #:nunion)
  (:documentation "Interface to C-language Roaring Bitmap APIs.")
  (:export
   ;; Conditions
   #:bitmap-error
   #:allocation-error
   #:deallocated-bitmap-error

   ;; Functions
   #:add
   #:add-checked
   #:add-many
   #:and-cardinality
   #:and-not
   #:and-not-cardinality
   #:and-not-in-place
   #:cardinality
   #:clear
   #:containsp
   #:copy
   #:create
   #:emptyp
   #:equals
   #:free
   #:intersection
   #:intersectp
   #:jaccard-index
   #:map-bitmap
   #:maximum
   #:minimum
   #:nintersection
   #:nunion
   #:or-cardinality
   #:overwrite
   #:remove
   #:remove-checked
   #:remove-many
   #:shrink-to-fit
   #:strict-subsetp
   #:subsetp
   #:to-vector
   #:union
   #:with-bitmap
   #:xor
   #:xor-cardinality
   #:xor-in-place
   ))
