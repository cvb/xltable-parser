(defpackage :xltable-ptr-parser
  (:use :cl :cffi :cl-cont)
  (:export get-function-to-read-xltable))

(in-package :xltable-ptr-parser)

(defconstant word 2) ;bytes

;; size of types
(defconstant tdt-table 4) ;bytes, int
(defconstant tdt-float 8) ;bytes, float
(defconstant tdt-string nil) ;first byte contains length, not null terminated
(defconstant tdt-bool 2) ;bytes
(defconstant tdt-error 2) ;0|7|15|23|29|36|42
(defconstant tdt-blank 2) ;number of blank cells
(defconstant tdt-int 2) 
(defconstant tdt-skip 2) ;number of cells to skip

;; Actual data type values
(defconstant tdtTable #x0010)
(defconstant tdtFloat #x0001)
(defconstant tdtString #x0002)
(defconstant tdtBool #x0003)
(defconstant tdtError #x0004)
(defconstant tdtBlank #x0005)
(defconstant tdtInt #x0006)
(defconstant tdtSkip #x0007)

(defvar types '(
(#x0010 . tdtTable)
(#x0001 . tdtFloat)
(#x0002 . tdtString)
(#x0003 . tdtBool)
(#x0004 . tdtError)
(#x0005 . tdtBlank)
(#x0006 . tdtInt)
(#x0007 . tdtSkip)))

(defmacro with-ptr-inc (form)
  `(multiple-value-bind (val new-ptr)
       ,form
;     (format t "~&val: ~a new-ptr: ~a~%" val new-ptr)
     (setf ptr new-ptr)
     val))

(defun read-data-type (ptr)
  (values (mem-ref ptr :uint16)
	  (inc-pointer ptr 2)))

(defun read-data-size (ptr)
  (values (mem-ref ptr :uint16)
	  (inc-pointer ptr 2)))

(defun read-tdt-table (ptr)
  ;; return (rows . columns)
  (values (list :rows    (mem-aref ptr :uint16 0)
		:columns (mem-aref ptr :uint16 1))
	  (inc-pointer ptr (+ 2 2))))

;(binary-types:define-unsigned u64 8)

(defun read-tdt-float (ptr)
  (values (mem-ref ptr :double)
	  (inc-pointer ptr 8)))

(defun read-tdt-string (ptr)
  (let* ((len (mem-ref ptr :uint8))
	 (string-raw (make-array len :element-type '(unsigned-byte 8))))
    (values (foreign-string-to-lisp ptr :offset 1 :count len :encoding :cp1251)
	    (inc-pointer ptr (+ 1 len)))
					;(octets-to-string string-raw :encoding :cp1251)
    ))
    
(defun find-data-size (type &optional len)
  (cond 
    ((= type tdttable) tdt-table)
    ((= type tdtfloat) tdt-float)
    ((= type tdtstring) (+ 1 len))))

(define-condition find-reader-error (error)
  ((message :initarg :message
	    :accessor message-of
	    :initform nil
	    :documentation "Text that indicate what was wrong.")
   (value :initarg :value
	  :accessor value-of
	  :initform nil
	  :documentation "The type that is not handled.")))

(defun find-reader-error (message &key value)
  (error 'find-reader-error
         :message message
         :value value))


(defun find-reader (type)
  "Return reader for type"
  (cond 
    ((eql type tdttable) 'read-tdt-table)
    ((eql type tdtfloat) 'read-tdt-float)
    ((eql type tdtstring) 'read-tdt-string)
    (t (find-reader-error "unhandled type" :value type))))

(defmacro read-data (data-type size data ptr)
  "Actual data read is here, looks a bit ugly, but simple,
I use macros here to change actual value binded to size and
data when macros will be called."
  `(progn (setf ,data (with-ptr-inc (funcall (find-reader ,data-type) ,ptr))
		,size (- ,size
		  (if (stringp ,data) 
		      (find-data-size ,data-type (length ,data))
		      (find-data-size ,data-type))))
	  ,data))

(defun get-function-to-read-xltable (ptr len)
  "Return continuation, which read data from stream."
  (let ((cc nil)
	(ptr ptr)
	(len len)
	(offset 0))
    (lambda ()
      (if cc  (funcall cc)
	  (with-call/cc
	    (loop while (< offset len)
	       do (let* ((data-type (with-ptr-inc (read-data-type ptr)))
			 (data-size (with-ptr-inc (read-data-size ptr)))
			 (data nil))
		    ;(format t "~& ptr: ~a offset: ~a~%" ptr offset)
		    (setf offset (+ offset 2 2 data-size))
		    (loop while (not (= 0 data-size))
		       do (progn (read-data data-type data-size data ptr)
				 (call/cc 
				  (lambda (k)
				    (setf cc k)
				    data)))))))))))

