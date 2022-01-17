# Purpose

Provide a Common Lisp tool kit for managing sets of non-negative integers spanning the range of
`(unsigned-byte 32)` by calling out to the popular C implementation of 
[Roaring Bitmaps](https://github.com/RoaringBitmap/CRoaring).

There are two papers you may wish to read on Roaring Bitmaps:
1. [Better bitmap performance with Roaring bitmaps](https://r-libre.teluq.ca/602/1/RoaringBitmap.pdf)
2. [Roaring Bitmaps: Implementation of an Optimized Software Library](https://arxiv.org/pdf/1709.07821.pdf)

# Motivation

I wanted "integer set" capabilities that could represent large numbers of
large valued integers with resonable efficiency. As a secondary goal, it was
also desirable that I be able to efficiently examine the set membership in
numberic order.

However in 2021 the availability of Common Lisp tools suitable for
this task was nil, unless I missed something, perhaps I did.
A simple lisp bit-vector (or lisp bignums in lieu of bit vectors such as
[cl-intset](https://github.com/tkych/cl-intset)) is not sufficient when the keys
members are large numbers.

Take an example use case of managing sets of integer primary keys in a database.
Regardless of the current cardinality of the table, your keys have a good chance
of having very large values for an active table, i.e. in the hundreds of millions or greater.
Using a simple bit-vector to represent values in that range would require prohibitive
amounts of memory.

As I was contemplating and/or working on some pure lisp solutions for this,
there wasn't even anything I could use for comparison, benchmarking-wise.
Enter Roaring Bitmaps, which has a strong multi-language code base and an
apparently active maintainer (for CRoaring at least), and so here we are,
though note that you're limited to `(unsigned-byte 32)` values.

# Status - *EXPERIMENTAL/EVALUATION RELEASE*

This is a first draft implementation for evaluation purposes.  There is a
distinct likelihood I'll change some of the function names (see the section on
feedback) but hopefully I won't change arguments/semantics beyond that, and
actual semantic versioning semantics will be applied ON A FUTURE RELEASE.

# Goals

1. Enable access to a reasonabe set of functions dealing with sets of integers.
2. Ensure that the foreign function nature of this implementation is _safe_ so that foriegn memory is not leaked and the risk of FFI-induced segmentation faults is minimal.

While I do care about performance, I care about FFI robustness more, but see the
solicited feedback section on this topic, input is welcome.  I may have erred on the side of
excessive caution, and my lack of experience with CFFI may mean I've doubled
up on checks that don't need to be done, I don't know.

# Caution: Known memory limitations in CRoaring.

At the time of this writing, January 2022, there were a number of memory bugs
that might be exihibited in the CRoaring library if `malloc` should fail to
obtain memory. (The C code fails to check memory allocation results and would dereference
null pointers).  

Rather than create a new issue for this on the CRoaring github project,
I added to one that already points at the problem from a test standpoint:
https://github.com/RoaringBitmap/CRoaring/issues/182

While this would occur only in low-memory situations I personally 
consider this a potential showstopper for production use. Points of potential failure
are noted in the lisp docstrings of the affected functions, perhaps they'll be fixed
by the time you read this.

# Installation

I tested this on Fedora 33. There was no repository that had pre-built
CRoaring libraries, so I downloaded the CRoaring stuff from github and built
it locally.  Note that to get a shared, instead of static, library, I had to
edit the `CMakeCache.txt` in my `build` directory and change
`ROARING_BUILD_STATIC` to `OFF`.

A `make install` put the headers and shared libraries in `/usr/local/...` I'm
not sure what best practies are for Common Lisp CFFI in this regard, but for
now I have `#p"/usr/local/lib64/libroaring.so"` in my call to
`define-foreign-library`.  Hopefully that works for you.

The installation should otherwise be painless, though without a build-time groveling
of header files this issue could potentially break things some day:
https://github.com/RoaringBitmap/CRoaring/issues/343

# Feedback sought from experienced lispers.

1. Whether or not to overload CL symbols such as UNION, NUNION when semantics match, or 
use the CRoaring pattern of naming.
2. Whether subsetp should overload the CL symbol with same semantics, or
follow the stylistic approach presently embodied in #2.
3. Whether "strict-subsetp" should follow "p" name of "subsetp" for limited consistency,
or "-p" convention of multi-token identifiers.
4. If there are other CRoaring I didn't implement you feel are really crucial.
5. Have I added too much overhead for safety with all my `check-allocation` and `check-deallocation` calls?  See section titled "Goals"
6. Changes necessary to my `define-foreign-library` or other tips on making it easy to use the CRoaring external dependencies?  How does that play with QUICKLISP? The code isn't going to get past the `use-foreign-library` call without CRoaring installed.

# Differences from using CRoaring directly.

Many CRoaring functions are provided via some exported symbol that wraps the foreign function
calls.  However the following CRoaring functions were not implemented out of lazyness,
personal lack of need, or because other "lisp-ier" interfaces were created in their place.

(In the order they appeared in the header file, for better or worse)

- roaring_bitmap_create (static inline)
- roaring_bitmap_init_cleared
- roaring_bitmap_from_range
- roaring_bitmap_of_ptr (see `add-many`)
- roaring_bitmap_get_copy_on_write (static inline)
- roaring_bitmap_set_copy_on_write (static inline)
- roaring_bitmap_printf_describe (writing to stdout from lisp is not particularly useful)
- roaring_bitmap_of
- roaring_bitmap_printf (more stdout dysfunction)
- roaring_bitmap_intersect_with_range
- roaring_bitmap_or_many
- roaring_bitmap_or_many_heap
- roaring_bitmap_xor_many
- roaring_bitmap_add_range_closed
- roaring_bitmap_add_range (static inline)
- roaring_bitmap_remove_range_closed
- roaring_bitmap_remove_range (static inline)
- roaring_bitmap_contains_range
- roaring_bitmap_range_cardinality
- roaring_bitmap_range_uint32_array
- roaring_bitmap_remove_run_compression
- roaring_bitmap_run_optimize
- roaring_bitmap_serialize
- roaring_bitmap_deserialize
- roaring_bitmap_size_in_bytes
- roaring_bitmap_portable_deserialize
- roaring_bitmap_portable_deserialize_safe
- roaring_bitmap_portable_deserialize_size
- roaring_bitmap_portable_size_in_bytes
- roaring_bitmap_portable_serialize
- roaring_bitmap_frozen_size_in_bytes
- roaring_bitmap_frozen_serialize
- roaring_bitmap_frozen_view
- roaring_iterate (problematic to implement safely as a callback is required - see code comments)
                  (probably safer to use the _iterator() APIs)
- roaring_iterate64
- roaring_bitmap_lazy_or
- roaring_bitmap_lazy_or_inplace
- roaring_bitmap_repair_after_lazy
- roaring_bitmap_lazy_xor
- roaring_bitmap_lazy_xor_inplace
- roaring_bitmap_flip
- roaring_bitmap_flip_inplace
- roaring_bitmap_select
- roaring_bitmap_rank
- roaring_bitmap_statistics
- roaring_copy_uint32_iterator
- roaring_read_uint32_iterator

On the `{and,or,xor}_many` variants, I suppose supporting those might be much more efficient
than REDUCE on pairs of bitmaps. In the meantime you can workaround with REDUCE and other
variants.

Other differences are as follows:

0. Where CRoaring functions might return NULL when memory could not be allocated,
cl-roaring will signal ALLOCATION-ERROR conditions.
1. The exported functions wrapping `roaring_bitmap_and` and
`roaring_bitmap_and_inplace` are named INTERSECTION and NINTERSECTION,
respectively, and behave similarly to the CL functions except that they take bitmaps as arguments.
2. The exported functions wrapping `roaring_bitmap_or` and `roaring_bitmap_or_inplace` are
named UNION and NUNION, respectively, and behave similarly to the CL functions except that they
take bitmaps as arguments.
4. Where `roaring_bitmap_jaccard_index` is undefined if both bitmaps are empty, cl-roaring's `jaccard-index` function returns nil.

I considered using AND and OR instead of INTERSECTION and UNION, but I'd still be
in conflict with common lisp symbols, and in a much less obvious way. See the section on feedback.

# Big questions

What about `(unsigned-byte 64)` values?  For now CRoaring is limited to uint32 
elements, with presently indicated plans to fix this.  The author suggests partitioning at the
application domain level.
