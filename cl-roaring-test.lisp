(in-package :cl-user)

(defpackage :cl-roaring-test
  (:use :cl :fiveam :cl-roaring)
  (:import-from :alexandria :define-constant)
  (:shadowing-import-from :cl-roaring 
                          #:remove #:subsetp
                          #:intersection #:nintersection
                          #:union #:nunion)
  (:export #:run-tests)
  (:documentation "Tests for the :cl-roaring package."))

(in-package :cl-roaring-test)

;;;; Apologies, testing is all over the map, not least because I have a love/hate thing going
;;;; with 5AM.  Meanwhile, we're mostly making sure all the foreign calls work as expected.
;;;; We are NOT verifying the semantics of all the called-out-to functions. 
;;;; All the exported functions of :cl-roaring should be called here at least once,
;;;; hopefully with all relevant argument permutations.

(defconstant +container-size+ (expt 2 16)
  "Possible threshold of interest for Roaring Bitmaps")

(defconstant +max-value+ (- (expt 2 32) 1))

(define-constant +test-vals+
  (list 0 1 2 
        ;; bracketing first +container-size+ values
        (- +container-size+ 2) (- +container-size+ 1) 
        +container-size+
        (+ +container-size+  1) (+ +container-size+ 2)

        ;; bracketing next +container-size+ values
        (- (* +container-size+ 2) 2) (- (* +container-size+ 2) 1) 
        (* +container-size+ 2)
        (+ (* +container-size+ 2)  1) (+ (* +container-size+ 2) 2)

        ;; bracketing +max-value+
        (- +max-value+ 2) (- +max-value+ 1) 
        +max-value+)
  :test 'equal
  :documentation "Test values for most test cases, in ascending order of value.")

(def-suite test-suite :description "cl-roaring tests")
(in-suite test-suite)

(test add-checked-contains-cardinality
  ;; Also testing emptyp and clear.
  (with-bitmap (bitmap (create))
    (is (emptyp bitmap))
    (dolist (x +test-vals+)
      (is (not (containsp x bitmap)))
      (is (add-checked x bitmap))
      (is (containsp x bitmap))
      (is (not (add-checked x bitmap))))
    (is (= (cardinality bitmap) (and-cardinality bitmap bitmap)))
    (is (not (emptyp bitmap)))
    (is (= (length +test-vals+) (cardinality bitmap)))
    (clear bitmap)
    (is (emptyp bitmap))))

(test add-contains-cardinality
  (with-bitmap (bitmap (create))
    (dolist (x +test-vals+)
      (is (not (containsp x bitmap)))
      (add x bitmap)
      (is (containsp x bitmap)))
    (is (= (cardinality bitmap) (and-cardinality bitmap bitmap)))
    (is (= (length +test-vals+) (cardinality bitmap)))))

(test remove-checked
  (with-bitmap (bitmap (create :initial-contents +test-vals+))
    (dolist (x +test-vals+)
      (is (containsp x bitmap))
      (is (remove-checked x bitmap))
      (is (not (containsp x bitmap)))
      (is (not (remove-checked x bitmap))))
    (is (= 0 (cardinality bitmap)))))

(test or-cardinality
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(3 4)))
      (is (= 4 (or-cardinality bitmap1 bitmap2))))))

(test and-not-cardinality
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(3 4)))
      (with-bitmap (bitmap3 (and-not bitmap1 bitmap2))
        (is (equalp #(1 2) (to-vector bitmap3))))
      (with-bitmap (bitmap3 (and-not bitmap2 bitmap1))
        (is (equalp #(4) (to-vector bitmap3))))
      (with-bitmap (bitmap3 (and-not bitmap1 bitmap1))
        (is (equalp #() (to-vector bitmap3))))
      (is (= 2 (and-not-cardinality bitmap1 bitmap2)))
      (is (= 1 (and-not-cardinality bitmap2 bitmap1)))
      (is (= 0 (and-not-cardinality bitmap2 bitmap2))))))

(test and-not-in-place
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(3 4)))
      (is (eq bitmap1 (and-not-in-place bitmap1 bitmap2)))
      (is (equalp #(1 2) (to-vector bitmap1)))
      (add 3 bitmap1)
      (is (eq bitmap2 (and-not-in-place bitmap2 bitmap1)))
      (is (equalp #(4) (to-vector bitmap2)))
      (signals error (and-not-in-place bitmap1 bitmap1)))))

(test xor-cardinality
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(3 4)))
      (is (= 3 (xor-cardinality bitmap1 bitmap2)))
      (add 2 bitmap2)
      (is (= 2 (xor-cardinality bitmap1 bitmap2))))))

(test xor-xor-in-place
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(3 4)))
      (with-bitmap (bitmap3 (xor bitmap1 bitmap2))
        (is (equalp #(1 2 4) (to-vector bitmap3)))
        (is (eq bitmap1 (xor-in-place bitmap1 bitmap2)))
        (is (equalp #(1 2 4) (to-vector bitmap1)))))))

(test copy-and-deallocation-check
  (with-bitmap (bitmap (create))
    (add-many +test-vals+ bitmap)
    (with-bitmap (bitmap2 (copy bitmap))
      (is (equalp (to-vector bitmap) (to-vector bitmap2)))
      (free bitmap2)
      (signals deallocated-bitmap-error (copy bitmap2)))))

(test overwrite
  (with-bitmap (bitmap (create))
    (add-many +test-vals+ bitmap)
    (with-bitmap (bitmap2 (create))
      (overwrite bitmap2 bitmap)
      (is (= (cardinality bitmap2) (cardinality bitmap)))
      (is (equalp (to-vector bitmap) (to-vector bitmap2))))))

(test add-many
  ;; seq is list
  (with-bitmap (bitmap (create))
    (add-many +test-vals+ bitmap)
    (is (= (length +test-vals+) (cardinality bitmap)))
    (dolist (x +test-vals+)
      (is (containsp x bitmap))))
  ;; seq is vector
  (with-bitmap (bitmap (create))
    (let ((vec (coerce +test-vals+ 'simple-vector)))
      (is (subtypep (type-of vec) 'simple-vector))
      (add-many vec bitmap)
      (is (= (length vec) (cardinality bitmap))))
    (dolist (x +test-vals+)
      (is (containsp x bitmap))))
  ;; seq is (array (unsigned-byte 32))
  (with-bitmap (bitmap (create))
    (let ((vec (make-array (length +test-vals+) :element-type '(unsigned-byte 32)
                           :initial-contents +test-vals+)))
      (is (typep vec '(array (unsigned-byte 32))))
      (add-many vec bitmap)
      (is (= (length vec) (cardinality bitmap))))
    (dolist (x +test-vals+)
      (is (containsp x bitmap)))))

(test remove-many
  ;; seq is list
  (with-bitmap (bitmap (create :initial-contents +test-vals+))
    (is (= (length +test-vals+) (cardinality bitmap)))
    (remove-many +test-vals+ bitmap)
    (is (= 0 (cardinality bitmap))))
  ;; seq is vector
  (with-bitmap (bitmap (create :initial-contents +test-vals+))
    (let ((vec (coerce +test-vals+ 'simple-vector)))
      (is (= (length +test-vals+) (cardinality bitmap)))
      (is (subtypep (type-of vec) 'simple-vector))
      (remove-many vec bitmap)
      (is (= 0 (cardinality bitmap)))))
  ;; seq is (array (unsigned-byte 32))
  (with-bitmap (bitmap (create :initial-contents +test-vals+))
    (let ((vec (make-array (length +test-vals+) :element-type '(unsigned-byte 32)
                           :initial-contents +test-vals+)))
      (is (typep vec '(array (unsigned-byte 32))))
      (remove-many vec bitmap)
      (is (= 0 (cardinality bitmap))))))

(test shrink-to-fit
  (with-bitmap (bitmap (create :initial-contents +test-vals+))
    ;; I printed this, it saved 8 bytes, don't want to assert that though.
    (is (typep (shrink-to-fit bitmap) '(unsigned-byte 32)))))

(test equals
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create))
      (is (not (equals bitmap1 bitmap2)))
      (add 1 bitmap2)
      (is (not (equals bitmap1 bitmap2)))
      (add 3 bitmap2)
      (is (not (equals bitmap1 bitmap2)))
      (add 2 bitmap2)
      (is (equals bitmap1 bitmap2)))))

(test subsetp ; & strict
  (with-bitmap (bitmap1 (create))
    (with-bitmap (bitmap2 (create :initial-contents '(1 2 3)))
      (is (subsetp bitmap1 bitmap2))
      (is (strict-subsetp bitmap1 bitmap2))
      (add 1 bitmap1)
      (add 2 bitmap1)
      (is (subsetp bitmap1 bitmap2))
      (is (strict-subsetp bitmap1 bitmap2))
      (add 3 bitmap1)
      (is (subsetp bitmap1 bitmap2))
      (is (not (strict-subsetp bitmap1 bitmap2)))
      (add 4 bitmap1)
      (is (not (subsetp bitmap1 bitmap2)))
      (is (not (strict-subsetp bitmap1 bitmap2)))
      (is (subsetp bitmap2 bitmap1))
      (is (strict-subsetp bitmap2 bitmap1)))))

(test to-vector
  (with-bitmap (bitmap (create))
    (is (equalp #() (to-vector bitmap)))
    (add-many +test-vals+ bitmap)
    ;; to-vector output is ordered.
    (is (equalp (coerce +test-vals+ 'vector) (to-vector bitmap))))
  ;; Try a reverse population to ensure it doesn't affect order of to-vector output.
  (with-bitmap (bitmap (create))
    (add-many (reverse +test-vals+) bitmap)
    (is (equalp (coerce +test-vals+ 'vector) (to-vector bitmap)))))

(test min-max
  (with-bitmap (bitmap (create))
    (is (null (minimum bitmap)))
    (is (null (maximum bitmap)))
    (add-many +test-vals+ bitmap)
    (is (= 0 (minimum bitmap)))
    (is (= (first (last +test-vals+)) (maximum bitmap)))
    (remove (first +test-vals+) bitmap)
    (is (= (second +test-vals+) (minimum bitmap)))
    (remove (car (last +test-vals+)) bitmap)
    (is (= (car (last +test-vals+ 2)) (maximum bitmap)))))

(test intersection-and-intersectp
  (with-bitmap (bitmap1 (create :initial-contents +test-vals+))
    (is (intersectp bitmap1 bitmap1))
    (with-bitmap (bitmap2 (create))
      (is (not (intersectp bitmap1 bitmap2)))
      (with-bitmap (bitmap3 (intersection bitmap1 bitmap2))
        (is (= 0 (cardinality bitmap3))))
      (add (second +test-vals+) bitmap2)
      (is (intersectp bitmap1 bitmap2))
      (with-bitmap (bitmap3 (intersection bitmap1 bitmap2))
        (is (= 1 (cardinality bitmap3)))
        (is (containsp (second +test-vals+) bitmap3)))
      (add-many +test-vals+ bitmap2)
      (with-bitmap (bitmap3 (intersection bitmap1 bitmap2))
        (is (intersectp bitmap1 bitmap3))
        (is (equalp (coerce +test-vals+ 'vector) (to-vector bitmap3)))))))

(test nintersection
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(1 3 4)))
      (is (eq bitmap1 (nintersection bitmap1 bitmap2)))
      (is (equalp #(1 3) (to-vector bitmap1)))
      (is (eq bitmap1 (nintersection bitmap1 bitmap1)))
      (is (equalp #(1 3) (to-vector bitmap1))))))

(test union-nunion
  (with-bitmap (bitmap1 (create :initial-contents '(1 2 3)))
    (with-bitmap (bitmap2 (create :initial-contents '(1 3 4)))
      (with-bitmap (bitmap3 (union bitmap1 bitmap2))
        (is (equalp #(1 2 3 4) (to-vector bitmap3))))
      (is (eq bitmap1 (nunion bitmap1 bitmap2)))
      (is (equalp #(1 2 3 4) (to-vector bitmap1)))
      (is (eq bitmap1 (nunion bitmap1 bitmap2)))
      (is (equalp #(1 2 3 4) (to-vector bitmap1))))))

(defun near (double integer)
  "Return true if double rounds to approximately integer.
  Probaly a stupid way to do this."
  (multiple-value-bind (n rem)
      (round double)
    (and (= integer n)
         (< rem 0.01))))

(test jaccard
  (with-bitmap (bitmap1 (create))
    (with-bitmap (bitmap2 (create))
      (is (null (jaccard-index bitmap1 bitmap2)))
      (add 1 bitmap1)
      (is (zerop (jaccard-index bitmap1 bitmap2)))
      (add 1 bitmap2)
      (is (near (jaccard-index bitmap1 bitmap2) 1))
      (loop for val in (cddr +test-vals+)
            for n-vals from 0
            with last-index = 1.0
            do (add val bitmap1)
               (let ((new-index (jaccard-index bitmap1 bitmap2)))
                 (is (< new-index last-index))
                 (setf last-index new-index))))))

(test map-bitmap
  (let ((result nil))
    (flet ((collect (x) (push x result)))
      (with-bitmap (bitmap (create :initial-contents +test-vals+))
        ;; null starting-with, null ending-with
        (is (null (map-bitmap bitmap #'collect)))
        (is (equalp +test-vals+ (reverse result)))
        (setf result nil)
        (is (null (map-bitmap bitmap #'collect :from-end t)))
        (is (equalp +test-vals+ result))
        (setf result nil)

        ;; starting-with
        (is (null (map-bitmap bitmap #'collect :starting-with 0)))
        (is (equalp +test-vals+ (reverse result)))
        (setf result nil)
        (map-bitmap bitmap #'collect :starting-with 2)
        (is (equalp (cddr +test-vals+) (reverse result)))
        (setf result nil)
        (map-bitmap bitmap #'collect :starting-with 3)
        (is (equalp (cdddr +test-vals+) (reverse result)))
        (setf result nil)
        ;; starting-with is inclusive, regardless of direction
        (map-bitmap bitmap #'collect :starting-with (car (last +test-vals+)) :from-end t)
        (is (equalp +test-vals+ result))
        (setf result nil)
        (map-bitmap bitmap #'collect :starting-with (car (last +test-vals+ 2)) :from-end t)
        (is (equalp (butlast +test-vals+ 1) result))
        (setf result nil)

        ;; ending-with
        (is (null (map-bitmap bitmap #'collect :ending-with +container-size+)))
        (is (equalp (subseq +test-vals+ 0 (position +container-size+ +test-vals+))
                    (reverse result)))
        (setf result nil)
        (map-bitmap bitmap #'collect :ending-with +container-size+ :from-end t)
        (is (equalp (subseq +test-vals+ (1+ (position +container-size+ +test-vals+)))
                    result))
        (setf result nil)

        ;; Both starting-with and ending-with
        (map-bitmap bitmap #'collect 
                    :starting-with 2
                    :ending-with +container-size+)
        (is (equalp (subseq +test-vals+ 2 (position +container-size+ +test-vals+))
                    (reverse result)))
        (setf result nil)

        ;; Range checks
        (signals error (map-bitmap bitmap #'collect :starting-with 2 :ending-with 2))
        (signals error (map-bitmap bitmap #'collect :starting-with 3 :ending-with 2))
        (signals error (map-bitmap bitmap #'collect :starting-with 2 :ending-with 2 :from-end t))
        (signals error (map-bitmap bitmap #'collect :starting-with 1 :ending-with 2 :from-end t))
        (map-bitmap bitmap #'collect :starting-with 3 :ending-with 2 :from-end t);no signal
          
        ))))
  
(defun run-tests ()
  "Run all :cl-roaring tests."
  (explain! (run 'test-suite)))


