(in-package :cl-roaring)

;; See: https://r-libre.teluq.ca/602/1/RoaringBitmap.pdf
;;      https://arxiv.org/abs/1709.07821
;; for Roaring Bitmap concept discussions.

;; The `make install` for croaring put stuff in /usr/local/include/croaring and /usr/local/lib64
(define-foreign-library libroaring
  ;; If you only have it in a private build, maybe something like:
  ;; #p"/home/dave/CRoaring/build/libroaring.so"
  ;; Note that by default CRoaring builds static libraries, I had to edit 
  ;; CRoaring/build/CMakeCache.txt and change the value of ROARING_BUILD_STATIC
  (:unix (:or "libroaring.so"
              #p"/usr/local/lib64/libroaring.so"))
  (t (:default "libroaring")))

(use-foreign-library libroaring)

(defconstant +max-uint32+ (1- (expt 2 32)))
(declaim (type (unsigned-byte 32) +max-uint+))

;;;
;;; With thanks to the paper "Integrating Foreign Libraries in Common Lisp: Best Practices"
;;; by Rudi Schlatte (2006). http://www.european-lisp-workshop.org/archives/06.schlatte.pdf
;;;

(define-condition bitmap-error (error)
  ()
  (:documentation "An error condition signalled when dealing with the Roaring Bitmap C API"))

(define-condition allocation-error (bitmap-error)
  ((object-type :reader object-type-of :initarg :object-type))
  (:documentation "An error arose when attempting to allocate a Roaring Bitmap or related entity.")
  (:report
   (lambda (condition stream)
     (format stream "Failed object allocation for ~A."
             (object-type-of condition)))))

(defun allocation-error (object-type)
  "Signal an ALLOCATION-ERROR condition in dealing with Roaring Bitmap APIs.
OBJECT-TYPE describes the type of object for which the attempted allocation failed."
  (error 'allocation-error :object-type object-type))

(define-condition deallocated-bitmap-error (bitmap-error)
  ((bitmap-object
    :reader bitmap-object :initarg :bitmap-object
    :documentation "The bitmap structure object that held the now-deallocated foreign pointer."))
  (:documentation "An error signalled when attempting to perform a Roaring Bitmap operation
against a bitmap whose foreign memory has been freed.")
  (:report
   (lambda (condition stream)
     (format stream "An attempt was made to perform a bitmap operation on bitmap ~A whose foreign memory was deallocated."
             (bitmap-object condition)))))

(defun deallocated-operation-error (bitmap)
  "Signal DEALLOCATED-BITMAP-ERROR with a bitmap strcuture for which
an operation was requested that requires a valid foreign pointer, but whose foreign
memory has been freed."
  (error 'deallocated-bitmap-error :bitmap-object bitmap))
  
;;;
;;; LOW LEVEL FUNCTIONS for use only in this package.
;;;
;;; Note that the types/functions apparently defined here are not normal lisp types/functions.
;;; They are resolved only with in CFFI contexts within CFFI calls.
;;; Thus a 'create' CFUN does not collide with a create lisp function.
;;;

(defctype roaring-bitmap :pointer)

(defcfun "roaring_bitmap_create_with_capacity" roaring-bitmap
  "Allocate a new bitmap with the indicated 'capacity', which is a hint indicating
the number of 'containers' that the data will need.
Returns null if the bitmap could not be allocated.
Caller must remember to call `bitmap-free` in the returned bitmap."
  (capacity :uint32))

;; The C function `bitmap-create` is `static inline` and basically doesn't exist as a symbol.
;; It calls `roaring_bitmap_create(0)`.

(defcfun "roaring_bitmap_copy" roaring-bitmap
  "Make a copy of a roaring-bitmap.
Return the new bitmap, or null if insufficient memory was available."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_overwrite" :bool
  "Copies a bitmap from src to dest. It is assumed that the pointer dest
is to an already allocated bitmap. The content of the dest bitmap is freed/deleted
as necessary to accommodate new content.

It might be preferable and simpler to call roaring_bitmap_copy except
that roaring_bitmap_overwrite can save on memory allocations.

Returns true if the overwrite was successful, false if the overwrite couldn't be completed
because of allocation failures."
  (dest roaring-bitmap)
  (src  roaring-bitmap))

(defcfun "roaring_bitmap_free" :void
  "Free the memory previously allocated by `bitmap-create`. E.g. (bitmap-free <bitmap>)"
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_equals" :bool
  "Return true if the two bitmaps contain the same elements."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_is_subset" :bool
  "Return true if all the elements of bitmap1 are also in bitmap2."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_is_strict_subset" :bool
  "Return true if all the elements of bitmap1 are also in bitmap2, and bitmap2 is strictly
greater than bitmap1."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_shrink_to_fit" :size
  "If needed, reallocate memory to shrink the memory usage.
Returns the number of bytes saved."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_add" :void
  "Add the unsigned integer N to the bitmap, e.g. (bitmap-add <bitmap> 1000)"
  (bitmap roaring-bitmap)
  (n :uint32))

(defcfun "roaring_bitmap_add_checked" :bool
  "Add value n to bitmap.
Returns true if a new value was added, false if the value already existed."
  (bitmap roaring-bitmap)
  (n :uint32))

(defcfun "roaring_bitmap_remove_checked" :bool
  "Remove value n from bitmap.
Returns true if a new value was removed, false if the value was already absent."
  (bitmap roaring-bitmap)
  (n :uint32))

;; Apparently there's no remove_checked analygous to add_checked.
(defcfun "roaring_bitmap_remove" :void
  "Remove value n"
  (bitmap roaring-bitmap)
  (n :uint32))

(defcfun "roaring_bitmap_add_many" :void
  "Add n_args values from pointer vals, faster than repeatedly calling `roaring_bitmap_add`
(at least if you're calling from C, in lisp you may have to copy a lisp seq
to a C array, slowing things down somewhat)."
  (bitmap roaring-bitmap)
  (n-args :size)
  (vals :pointer))

(defcfun "roaring_bitmap_remove_many" :void
  "Remove n_args values from pointer vals, faster than repeatedly calling `roaring_bitmap_remove`
(at least if you're calling from C, in lisp you may have to copy a lisp seq
to a C array, slowing things down somewhat)."
  (bitmap roaring-bitmap)
  (n-args :size)
  (vals :pointer))

(defcfun "roaring_bitmap_to_uint32_array" :void
  "Convert the bitmap to an array, output in `ans`,
Caller is responsible to ensure that there is enough memory allocated, e.g.
ans = malloc(roaring_bitmap_get_cardinality(bitmap) * sizeof(uint32_t));"
  (bitmap roaring-bitmap)
  (result :pointer))

(defcfun "roaring_bitmap_contains" :boolean
  "Return true if unsigned integer N is present in the bitmap, false (NIL) otherwise."
  (bitmap roaring-bitmap)
  (n :uint32))

(defcfun "roaring_bitmap_and" roaring-bitmap
  "Computes the intersection between two bitmaps and returns new bitmap.
Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_and_inplace" :void
  "Computes the intersection between two bitmaps, modifying the first to hold the result."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_and_cardinality" :uint64
  "Computes the size of the intersection between two bitmaps."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_or" roaring-bitmap
  "Computes the union between two bitmaps and returns new bitmap.
Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_or_inplace" :void
  "Computes the union between two bitmaps and stores the result in the first bitmap."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_or_cardinality" :uint64
  "Computes the size of the union between two bitmaps."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_andnot" roaring-bitmap
  "Computes the difference (andnot) between two bitmaps and returns new bitmap.
Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_andnot_inplace" :void
  "Inplace version of roaring_bitmap_andnot, modifies bitmap1, 
which must not be EQ bitmap2."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_andnot_cardinality" :uint64
  "Computes the size of the difference (andnot) between two bitmaps."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_xor" roaring-bitmap
  "Computes the symmetric difference (xor) between two bitmaps
and returns new bitmap.
Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_xor_inplace" :void
  "Computes the symmetric difference (xor) between two bitmaps and stores the result 
in the first bitmap. Does not support both args being the same bitmap."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_xor_cardinality" :uint64
  "Computes the size of the symmetric difference (xor) between two bitmaps."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_intersect" :bool
  "Check whether two bitmaps intersect."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_jaccard_index" :double
  "Computes the Jaccard index between two bitmaps. (Also known as the Tanimoto
distance, or the Jaccard similarity coefficient).
The Jaccard index is undefined if both bitmaps are empty."
  (bitmap1 roaring-bitmap)
  (bitmap2 roaring-bitmap))

(defcfun "roaring_bitmap_get_cardinality" :uint64
  "Get the cardinality of the bitmap (number of elements)."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_is_empty" :bool
  "Returns true if the bitmap is empty (cardinality is zero).
Guessing this may be faster than calling get_cardinality and checking for zero."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_clear" :void
 "Empties the bitmap.  It will have no auxiliary allocations (so if the bitmap
 was initialized in client memory via roaring_bitmap_init(), then a call to
 roaring_bitmap_clear() would be enough to 'free' it)."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_minimum" :uint32
  "Returns the smallest value in the set, or UINT32_MAX (+max-uint32+) if the set is empty."
  (bitmap roaring-bitmap))

(defcfun "roaring_bitmap_maximum" :uint32
  "Returns the greatest value in the set, or 0 if the set is empty."
  (bitmap roaring-bitmap))

;;;
;;; Iteration (low level)
;;;
;;; The iterate() interface is not presently supported. 
;;; While we could call the roaring_iterate() interface
;;; and arrange for a callback to a lisp function for each member of the bitmap,
;;; I'm thinking bad things would happen if the called-back lisp function signalled a condition
;;; or attempted to return from some block outside the scope of roaring_iterate().
;;; Thus a generic (map-bitmap <bitmap> <fn>) functionality would not be safe for arbitrary <fn>.
;;;
;;; I'm not sure how useful this iteration technique is beyond map/reduce/filter type things.
;;; Either way, we can emulate it more safely with the struct driven iterator mechanisms 
;;; defined below.

#|
(defctype roaring-iterator :pointer
  "`roaring_iterator` is simply a pointer to a function that returns bool
(true means that the iteration should continue while false means that it
should stop), and takes (uint32_t,void*) as inputs.")

(defcfun "roaring_iterate" :bool
  "Iterate over the bitmap elements. The function iterator is called once for
all the values with ptr (can be NULL) as the second parameter of each call.

Iterator is as per `roaring-iterator`.
`pointer` is a utility pointer for use by the interator, or it can be NULL.

Returns true if the roaring_iterator returned true throughout (so that all
data points were necessarily visited).

Iteration is ordered: from the smallest to the largest elements."
  (bitmap roaring-bitmap)
  (iterator roaring-iterator)
  (pointer :pointer))
|#  

;;; cl-raylib was a useful example for me here, though I'm unclear on when to 
;;; use :class with `defcstruct`, and since it isn't documented I haven't done it.
(defcstruct roaring-uint32-iterator-t
  "Structure that holds mutable roaring bitmap iterator state, for use with
roaring.*iterator functions.  The current-value and has-value fields
are examined by lisp, as they contain the information of interest on initialization/iteration."
  (parent :pointer)
  (container-index :int32)
  (in-container-index :int32)
  (run-index :int32)
  (current-value :uint32)
  (has-value :bool)
  ;; We probably don't need the rest of these, but we define them in the unlikly event
  ;; we have arrays of these structs.
  (container :pointer)
  (typecode :uint8)
  (highbits :uint32))

;;; A challenge here is that we must access `current-value` and `has-value` via structure
;;; mechanisms.  If the c-struct changes the location of those slots, our code will break.
;;; We could potentially iterate using `roaring_read_uint32_iterator`, for now that
;;; approach hasn't been explored.  The following github issue was filed to point out
;;; the need for a callable function(s) to access `current-value` and `has-value`
;;; https://github.com/RoaringBitmap/CRoaring/issues/343


(defctype roaring-uint32-iterator-ptr :pointer)

(defcfun "roaring_init_iterator" :void
  "Initialize an iterator object that can be used to iterate through the
values. If there is a  value, then this iterator points to the first value
and `it->has_value` is true. The value is in `it->current_value`."
  (roaring-bitmap roaring-bitmap)
  (iterator-ptr roaring-uint32-iterator-ptr))
  
(defcfun "roaring_init_iterator_last" :void
  "Initialize an iterator object that can be used to iterate through the
values. If there is a value, then this iterator points to the last value
and `it->has_value` is true. The value is in `it->current_value`."
  (roaring-bitmap roaring-bitmap)
  (iterator-ptr roaring-uint32-iterator-ptr))

#+nil ; not using this for now
(defcfun "roaring_create_iterator" roaring-uint32-iterator-ptr
  "Create an iterator object that can be used to iterate through the values.
Basically malloc() + roaring_init_iterator().

Caller is responsible for calling `roaring_free_iterator()`.
The iterator is initialized (this function calls `roaring_init_iterator()`)
If there is a value, then this iterator points to the first value and
`it->has_value` is true.  The value is in `it->current_value`."
  (roaring-bitmap roaring-bitmap))

(defcfun "roaring_advance_uint32_iterator" :bool
  "Advance the iterator. If there is a new value, then `it->has_value` is true.
The new value is in `it->current_value`. Values are traversed in increasing
orders. For convenience, returns `it->has_value`."
  (iterator-ptr roaring-uint32-iterator-ptr))

(defcfun "roaring_previous_uint32_iterator" :bool
  "Decrement the iterator. If there's a new value, then `it->has_value` is true.
The new value is in `it->current_value`. Values are traversed in decreasing
order. For convenience, returns `it->has_value`."
  (iterator-ptr roaring-uint32-iterator-ptr))

(defcfun "roaring_move_uint32_iterator_equalorlarger" :bool
  "Move the iterator to the first value >= `val`. If there is a such a value,
then `it->has_value` is true. The new value is in `it->current_value`.
For convenience, returns `it->has_value`."
  (iterator-ptr roaring-uint32-iterator-ptr)
  (val :uint32))

#+NIL ; not using this either if `with-foreign-object` works for our sole iteration cosntruct
(defcfun "roaring_free_uint32_iterator" :void
  "Free memory following `roaring_create_iterator()`."
  (iterator-ptr roaring-uint32-iterator-ptr))

;; roaring_read_uint32_iterator might be of interest for taking values in chunks,
;; in a way that the 'in_range' functions won't do well, or at all.  Not implemented for now.


;;;
;;; Higher level APIs to be used outside the package
;;;

(deftype uint32 () '(unsigned-byte 32))

(defstruct bitmap
  "Object to package up a Roaring Bitmap foreign pointer."
  (fp nil :type foreign-pointer :read-only t)
  (freed nil :type boolean))

(defun check-deallocation (bitmap)
  "Call `deallocated-operation-error` if a bitmap's foreign pointer has been freed."
  ;; TBD: inline?  macro? Is this even useful? Or would the FFI mechanisms signal some reasonable
  ;; condition for invoking the CFFI declared routine with a freed pointer without
  ;; my extra check here?
  (when (bitmap-freed bitmap)
    (deallocated-operation-error bitmap)))

(defun free (bitmap)
  "Release foreign memory for the bitmap, and clear the finalization that was set up for
the bitmap object.  Return value N/A"
  (declare (type bitmap bitmap))
  (unless (bitmap-freed bitmap)
    (setf (bitmap-freed bitmap) t)
    ;; TBD: portable way to block interrupts here?  Should I even worry about that?
    (tg:cancel-finalization bitmap)
    (roaring-bitmap-free (bitmap-fp bitmap))))

(defun wrap-bitmap (roaring-bitmap)
  "Allocate and initialize a bitmap struct that wraps the indicated roaring-bitmap foreign pointer,
  and arrange for finalization of the foreign pointer.  Return the struct."
  (let ((bitmap (make-bitmap :fp roaring-bitmap)))
    (tg:finalize bitmap #'(lambda () (roaring-bitmap-free roaring-bitmap)))
    bitmap))

(defun create (&key capacity initial-contents)
  "Allocate a new bitmap.

If CAPACITY is sepcified, it must be a non-negative integer that is passed to bitmap
creation logic as a hint about the number of Roaring Bitmap 'containers' that the data
will need. By default this is zero, unless initial-contents are used in which case we default
to some non-zero value.

If INITIAL-CONTENTS is non-nil, it must be a sequence and is used to
to populate the newly created bitmap using ADD-MANY.

Signals an error of type ALLOCATION-ERROR if the bitmap could not be allocated.

Caller may proactively call FREE on the bitmap, or leave it to be freed when garbage
collection occurs. A proactive approach (using FREE, or WITH-BITMAP) is recommended
over leaving it for GC."
  (declare (type (or null uint32) capacity)
           (type (or null sequence) initial-contents))
  (let* ((capacity (or capacity (if initial-contents 1 0)))
         (bp (roaring-bitmap-create-with-capacity capacity)))
    (if (null-pointer-p bp)
        (allocation-error :bitmap)
        (let ((bitmap (wrap-bitmap bp)))
          (when initial-contents
            (add-many initial-contents bitmap))
          bitmap))))

(defun copy (bitmap)
  "Create a new bitmap that is a copy of the input bitap.
Signals ALLOCATION-ERROR if memory for the new bitmap could not be allocated."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (let ((bp (roaring-bitmap-copy (bitmap-fp bitmap))))
    (if (null-pointer-p bp)
      (allocation-error :bitmap)
      (wrap-bitmap bp))))

(defun overwrite (dest-bitmap src-bitmap)
  "Copies a bitmap from src-bitmap to dest-bitmap. The content of the dest bitmap is freed/deleted
as necessary to accommodate new content.

It might be preferable and simpler to call `copy` except
that `overwrite` might save on memory allocations.

Signals ALLOCATION-ERROR if the operation failed due to internall [re-]allocation
failures on the destination bitmap.  TBD: The structure of the destination bitmap is 
internally consistent and can be used for future operations,
however the state and/or element content of the
destination bitmap is UNDEFINED whan an ALLOCATION-ERROR is signaled."
  (declare (bitmap dest-bitmap src-bitmap))
  (check-deallocation dest-bitmap)
  (check-deallocation src-bitmap)
  (if (roaring-bitmap-overwrite (bitmap-fp dest-bitmap) (bitmap-fp src-bitmap))
      t
      (allocation-error :bitmap)))

(defmacro with-bitmap ((bitmap-var bitmap-source) &body body)
  "Evaluates BODY with a BITMAP object specified by BITMAP-SOURCE bound to the symbol specified
by BITMAP-VAR.  Upon normal or abnormal exit from BODY, the foreign object reference
embodied in the BITMAP bound to BITMAP-VAR will freed, and the BITMAP may no longer be used.

BITMAP-SOURCE will typically be a call to CREATE.

Returns the values returned by BODY."
  (check-type bitmap-var symbol)
  `(let ((,bitmap-var ,bitmap-source))
     (unwind-protect 
          (progn ,@body)
       (free ,bitmap-var))))            ;cancels finalization too

(defun shrink-to-fit (bitmap)
  "If needed, reallocate memory to shrink the memory usage.
Returns the number of bytes saved."
  (declare (type bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-shrink-to-fit (bitmap-fp bitmap)))

(defun equals (bitmap1 bitmap2)
  "Return true if the two bitmaps contain the same elements."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-equals (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun subsetp (bitmap1 bitmap2)
  "Return true if all the elements of bitmap1 are also in bitmap2."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-is-subset (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun strict-subsetp (bitmap1 bitmap2)
  "Return true if all the elements of bitmap1 are also in bitmap2, and bitmap2 is strictly
greater than bitmap1."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-is-strict-subset (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun add (n bitmap)
  "Add the unsigned integer N to the bitmap, e.g. (add 1000 <bitmap>)"
  (declare (type uint32 n) (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-add (bitmap-fp bitmap) n))

(defun add-checked (n bitmap)
  "Add value n to bitmap.
Returns true if a new value was added, false if the value already existed."
  (declare (type uint32 n) (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-add-checked (bitmap-fp bitmap) n))

(defun remove-checked (n bitmap)
  "Remove value n from bitmap.
Returns true if a new value was removed, false if the value was already absent."
  (declare (type uint32 n) (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-remove-checked (bitmap-fp bitmap) n))

(defun add-many (seq bitmap)
  "Add elements of seq (which must have elements of type (unsigned-byte 32)) to bitmap."
  (declare (sequence seq) ;would like to declare seq element type, didn't quite manage it
           (bitmap bitmap))
  (check-deallocation bitmap)
  ;; *TBD*: make safer for sequence length and element type compliance?
  (let ((length (length seq)))
    (if (typep seq '(array (unsigned-byte 32)))
        ;; Avoid array copy, array is already specialized to the type needed by the C fun.
        (with-pointer-to-vector-data (ptr-to-lisp-vector seq)
          (roaring-bitmap-add-many (bitmap-fp bitmap) length ptr-to-lisp-vector))
        (with-foreign-object (array :uint32 length)
          (if (listp seq)
              (loop for i from 0 
                    for elt in seq
                    do (setf (mem-aref array :uint32 i) elt))
              (loop for i from 0
                    for elt across seq
                    do (setf (mem-aref array :uint32 i) elt)))
          (roaring-bitmap-add-many (bitmap-fp bitmap) length array)))))

(defun remove-many (seq bitmap)
  "Remove elements of seq (which must have elements of type (unsigned-byte 32)) from bitmap."
  (declare (sequence seq) ;would like to declare seq element type, didn't quite manage it
           (bitmap bitmap))
  (check-deallocation bitmap)
  ;; *TBD*: make safer for sequence length and element type compliance?
  (let ((length (length seq)))
    (if (typep seq '(array (unsigned-byte 32)))
        ;; Avoid array copy, array is already specialized to the type needed by the C fun.
        (with-pointer-to-vector-data (ptr-to-lisp-vector seq)
          (roaring-bitmap-remove-many (bitmap-fp bitmap) length ptr-to-lisp-vector))
        (with-foreign-object (array :uint32 length)
          (if (listp seq)
              (loop for i from 0 
                    for elt in seq
                    do (setf (mem-aref array :uint32 i) elt))
              (loop for i from 0
                    for elt across seq
                    do (setf (mem-aref array :uint32 i) elt)))
          (roaring-bitmap-remove-many (bitmap-fp bitmap) length array)))))

(defun to-vector (bitmap)
  "Produce all elements of the bitmap as a vector.
Elements are returned in ascending order (per author in Google groups)."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (let* ((length (cardinality bitmap))
         (result (make-array length :element-type '(unsigned-byte 32))))
    (declare (type (unsigned-byte 32) length)
             (type (vector (unsigned-byte 32)) result))
    ;; `with-pointer-to-vector-data` isn't in the manual, presumably an oversight
    ;; This avoids the double copy to foreign array then to lisp vector
    (with-pointer-to-vector-data (ptr-to-lisp-vector result)
      (roaring-bitmap-to-uint32-array (bitmap-fp bitmap) ptr-to-lisp-vector))
    result))

(defun remove (n bitmap)
  "Remove the unsigned integer N from the bitmap."
  (declare (type uint32 n) (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-remove (bitmap-fp bitmap) n))

(defun containsp (n bitmap)
  "Return true if unsigned integer N is present in the bitmap, false (NIL) otherwise.
E.g. (containsp 1000 <bitmap>)"
  (declare (type uint32 n) (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-contains (bitmap-fp bitmap) n))

(defun intersection (bitmap1 bitmap2)
  "Computes the intersection of two bitmaps and returns new bitmap.
  Warning: at the time of this writing there appears to be CRoaring allocation failure
  checks, and possible NULL dereferences.  The maintainers have been notified.
  We go through the motions and attempt to signal an ALLOCATION-ERROR if allocation fails."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (let ((bp (roaring-bitmap-and (bitmap-fp bitmap1) (bitmap-fp bitmap2))))
    (if (null-pointer-p bp)
      (allocation-error :bitmap)
      (wrap-bitmap bp))))

(defun nintersection (bitmap1 bitmap2)
  "Computes the intersection of two bitmaps, updating bitmap1 with the result.
  bitmap2 is unaffected. It is okay if (eq bitmap1 bitmap2). Returns bitmap1."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-and-inplace (bitmap-fp bitmap1) (bitmap-fp bitmap2))
  bitmap1)

(defun and-cardinality (bitmap1 bitmap2)
  "Computes the size of the intersection between two bitmaps."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-and-cardinality (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun union (bitmap1 bitmap2)
  "Computes the union of two bitmaps and returns new bitmap.
  Warning: at the time of this writing there appears to be CRoaring allocation failure
  checks, and possible NULL dereferences.  The maintainers have been notified.
  We go through the motions and attempt to signal an ALLOCATION-ERROR if allocation fails."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (let ((bp (roaring-bitmap-or (bitmap-fp bitmap1) (bitmap-fp bitmap2))))
    (if (null-pointer-p bp)
      (allocation-error :bitmap)
      (wrap-bitmap bp))))

(defun nunion (bitmap1 bitmap2)
  "Computes the union of two bitmaps, updating bitmap1 with the result.
  bitmap2 is unaffected. It is okay if (eq bitmap1 bitmap2). Returns bitmap1."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-or-inplace (bitmap-fp bitmap1) (bitmap-fp bitmap2))
  bitmap1)

(defun or-cardinality (bitmap1 bitmap2)
  "Computes the size of the union between two bitmaps."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-or-cardinality (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun and-not (bitmap1 bitmap2)
  "Computes the the difference (and not) between two bitmaps,
i.e. the  elements in bitmap1 that are not also in bitmap2,
and returns a new bitmap.

Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified.
We go through the motions and attempt to signal an ALLOCATION-ERROR if allocation fails."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (let ((bp (roaring-bitmap-andnot (bitmap-fp bitmap1) (bitmap-fp bitmap2))))
    (if (null-pointer-p bp)
      (allocation-error :bitmap)
      (wrap-bitmap bp))))

(defun and-not-in-place (bitmap1 bitmap2)
  "Computes the the difference (and not) between two bitmaps,
i.e. the  elements in bitmap1 that are not also in bitmap2,
and stores the result in bitmap1.

bitmap2 is unaffected. 

Restriction: bitmap1 must not be EQ bitmap2, an error will be signalled.

Returns bitmap1."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (when (eq bitmap1 bitmap2)
    (error "and-not-in-place requires non-eq arguments, got ~s twice." bitmap1))
  (roaring-bitmap-andnot-inplace (bitmap-fp bitmap1) (bitmap-fp bitmap2))
  bitmap1)

(defun and-not-cardinality (bitmap1 bitmap2)
  "Computes the size of the difference (and not) between two bitmaps.
I.e. the number of elements in bitmap1 that are not also in bitmap2."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-andnot-cardinality (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun xor (bitmap1 bitmap2)
  "Computes the symmetric difference (xor) between two bitmaps
and returns new bitmap.
Warning: at the time of this writing there appears to be CRoaring allocation failure
checks, and possible NULL dereferences.  The maintainers have been notified.
We go through the motions and attempt to signal an ALLOCATION-ERROR if allocation fails."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (let ((bp (roaring-bitmap-xor (bitmap-fp bitmap1) (bitmap-fp bitmap2))))
    (if (null-pointer-p bp)
      (allocation-error :bitmap)
      (wrap-bitmap bp))))

(defun xor-in-place (bitmap1 bitmap2)
  "Computes the symmetric difference (xor) between two bitmaps and stores the result in
the first bitmap. Does not support both args being the same bitmap and will signal an error."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (when (eq bitmap1 bitmap2)
    (error "xor-in-place requires non-eq arguments, got ~s twice." bitmap1))
  (roaring-bitmap-xor-inplace (bitmap-fp bitmap1) (bitmap-fp bitmap2))
  bitmap1)

(defun xor-cardinality (bitmap1 bitmap2)
  "Computes the size of the symmetric difference (xor) between two bitmaps."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-xor-cardinality (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun intersectp (bitmap1 bitmap2)
  "Return true if the bitmaps intersect, nil if they do not.
More efficient than checking the results of INTERSECTION."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (roaring-bitmap-intersect (bitmap-fp bitmap1) (bitmap-fp bitmap2)))

(defun cardinality (bitmap)
  "Returns the number of integers represented by the bitmap.
This may be an O(n) function, depending on how the C library implements it."
  ;; *TBD*: if interoperability with externally provided roarinb bitmaps is not required
  ;; we could cache the count in our bitmap wrapper so that this is O(1).
  (check-deallocation bitmap)
  (roaring-bitmap-get-cardinality (bitmap-fp bitmap)))

(defun jaccard-index (bitmap1 bitmap2)
  "Computes the Jaccard index between two bitmaps. (Also known as the Tanimoto
distance, or the Jaccard similarity coefficient), a double that is zero
if there are no members in common, and 1 if all members are common, otherwise
something in-between.

The Jaccard index is undefined if both bitmaps are empty, in such cases
we return nil."
  (declare (bitmap bitmap1 bitmap2))
  (check-deallocation bitmap1)
  (check-deallocation bitmap2)
  (let* ((c1 (cardinality bitmap1))
         (c2 (cardinality bitmap2))
         (sum (+ c1 c2)))
    (declare (uint32 c1 c2) (type (integer 0) sum))
    (if (zerop sum)
        nil
        (let ((ac (and-cardinality bitmap1 bitmap2)))
          (declare (uint32 ac))
          (/ ac (- (+ c1 c2) ac))))))

(defun emptyp (bitmap)
  "Return true if the bitmap has no elements (bits set). 
It's possible this is faster than calling `cardinality` and checking for zero."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-is-empty (bitmap-fp bitmap)))

(defun clear (bitmap)
  "Empties the bitmap so that it has no auxiliary allocations and no members/bits-set."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (roaring-bitmap-clear (bitmap-fp bitmap)))

(defun minimum (bitmap)
  "Returns the smallest value in the set, or nil if the set is empty."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (let ((result (roaring-bitmap-minimum (bitmap-fp bitmap))))
    (if (and (= result +max-uint32+)
             (roaring-bitmap-is-empty (bitmap-fp bitmap)))
        nil
        result)))

(defun maximum (bitmap)
  "Returns the greatest value in the set, or nil if the set is empty."
  (declare (bitmap bitmap))
  (check-deallocation bitmap)
  (let ((result (roaring-bitmap-maximum (bitmap-fp bitmap))))
    (if (and (= result 0)
             (roaring-bitmap-is-empty (bitmap-fp bitmap)))
        nil
        result)))

(defun NOTroaring-move-uint32-iterator-equalorsmaller (iterator-ptr val)
  "Helper routing taking an iterator FP and a uing32 value and searching for 
  a value  that is <= val.  This is not efficient, it is O(n).

  Leaves iterator positioned at the desired value <= VAL, with appropriate iterator->has_value
  status."
  (declare (type foreign-pointer iterator-ptr)
           (type (unsigned-byte 32) val))
  (with-foreign-slots ((has-value current-value) iterator-ptr (:struct roaring-uint32-iterator-t))
    (loop while has-value
          while (> current-value val)
          do    (roaring-previous-uint32-iterator iterator-ptr))))

(defun map-bitmap (bitmap f &key starting-with ending-with from-end)
  "Iterate over values of a bitmap, calling (f <val>) for each value in the bitmap.

By default, iteration proceeds with all values in ascending order.

If FROM-END is true, iterataion proceeds in descending order. As explained below,
FROM-END affects comparison of STARTING-WITH and ENDING-WITH values.

STARTING-WITH, if specified, is a (unsigned-byte 32) inclusive value limiting the mapped range
start to the first value equal to or (> if from-end, < if not from-end) the value.

CAUTION: Specifying STARTING-WITH and FROM-END T may result in an O(n) iterator driven search
for the starting value because CRoaring does not have an efficient descending flavor of
`roaring_move_uint32_iterator_equalorlarger`.

ENDING-WITH, is specified, is a (unsigned-byte 32) exclusive value limiting the mapped
range end to the first value (> if from-end, < if not frome-end) the value.

If both STARTING-WITH and ENDING-WITH are supplied and overlap in semantically incompatible
ways an error will be signalled, e.g.

  `(map-bitmap b f :starting-with 1 :ending-with 1)` => ERROR

Returns nil."
  (declare (bitmap bitmap) 
           (type (or null (unsigned-byte 32)) starting-with)
           (type (or null (unsigned-byte 32)) ending-with))
  (when (and starting-with ending-with
             (or (and from-end (>= ending-with starting-with))
                 (and (not from-end) (>= starting-with ending-with))))
    (error "STARTING-WITH value ~d and ENDING-WITH value ~d are incompatible with FROM-END ~a."
           starting-with ending-with from-end))
  (with-foreign-object (iterator-ptr '(:struct roaring-uint32-iterator-t))
    ;; There isn't an API to find the last  value constrained by ENDING-WITH
    ;; There is    an API to find the first value constrained by STARTING-WITH
    ;; So for :FROM-END with :STARTING-WITH we'll have to seek it out some harder way.
    (if from-end
        (progn (roaring-init-iterator-last (bitmap-fp bitmap) iterator-ptr)
               (when starting-with
                 (NOTroaring-move-uint32-iterator-equalorsmaller iterator-ptr starting-with)))
        (progn (roaring-init-iterator (bitmap-fp bitmap) iterator-ptr)
               (when starting-with
                 (roaring-move-uint32-iterator-equalorlarger iterator-ptr starting-with))))
    (with-foreign-slots ((has-value current-value) 
                         iterator-ptr (:struct roaring-uint32-iterator-t))
      ;; Eliminate unnecessary tests in the loop.  Is there a better way?
      ;; Starting-with has already been factored in, the iterator is positioned to correct start.
      (cond ((and (null ending-with) (null from-end))
             (loop while has-value
                   do (funcall f current-value)
                      (roaring-advance-uint32-iterator iterator-ptr)))
            ((null ending-with)         ;from-end t
             (loop while has-value
                   do (funcall f current-value)
                      (roaring-previous-uint32-iterator iterator-ptr)))
            ((null from-end)            ;ending-with applies
             (loop while has-value
                   while (< current-value ending-with)
                   do (funcall f current-value)
                      (roaring-advance-uint32-iterator iterator-ptr)))
            (t                          ;ending-with and from-end
             (loop while has-value
                   while (> current-value ending-with)
                   do (funcall f current-value)
                      (roaring-previous-uint32-iterator iterator-ptr)))))))

;;; TBD: Whether or not to expose a proper iterator object and semantics.
;;; Would want to wrap the iterator foreign memory like we do the bitmap FP though.
;;; Would want create/free/with-iterator type constructs, and prev/next methods on it.
;;; The bitmap would probably be part of the create call and implicit thereafter, like the
;;; C iterator semantics. 
