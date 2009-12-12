(defpackage :xltable-parser
  (:use :cl :binary-types :babel :cl-cont :ieee-floats))

(in-package :xltable-parser)

(defconstant word 2) ;bytes

;; size of types
(defconstant tdt-table 4) ;bytes, int
(defconstant tdt-float 8) ;bytes, float
(defconstant tdt-string nil) ;first byte contains length, not null terminated
(defconstant tdt-bool 2) ;byte, why use word for these?
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


(defun read-data-type (stream)
  (read-binary 'u16 stream))

(defun read-data-size (stream)
  (read-binary 'u16 stream))

(defun read-tdt-table (stream)
  ;; return (rows columns)
  (list (read-binary 'u16 stream)
	(read-binary 'u16 stream)))

(binary-types:define-unsigned u64 8)

(defun read-tdt-float (stream)
  (decode-float64 (read-binary 'u64 stream)))

(defun read-tdt-string (stream)
  (let* ((len (read-binary 'u8 stream))
	 (string-raw (make-array len :element-type '(unsigned-byte 8))))
    (format t "~&read-tdt-string len: ~a" len) 
    (read-sequence string-raw stream)
    (format t "~&read-tdt-string seq: ~a~%" string-raw)
    (octets-to-string string-raw :encoding :cp1251)))
    
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

(defmethod print-object ((object find-reader-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~& error: ~a ~a"
	    (message-of object)
	    (value-of object))))

(defun find-reader-error (message &key value)
  (error 'find-reader-error
         :message message
         :value value))


(defun find-reader (type)
  (cond 
    ((eql type tdttable) 'read-tdt-table)
    ((eql type tdtfloat) 'read-tdt-float)
    ((eql type tdtstring) 'read-tdt-string)
    (t (find-reader-error "unhandled type" :value type))))

(defmacro read-data (data-type size data stream)
  `(progn (setf ,data (funcall (find-reader ,data-type) ,stream)
		,size (- ,size
		  (if (stringp ,data) 
		      (find-data-size ,data-type (length ,data))
		      (find-data-size ,data-type))))
	  ,data))

(defun get-function-to-read-xltable (stream)
  (let ((cc nil)
	(stream stream))
    (lambda ()
      (if cc  (funcall cc))
      (with-call/cc
	(loop do (let* ((data-type (read-data-type stream))
			(data-size (read-data-size stream))
			(data nil))
		   (loop while (not (= 0 data-size))
		      do (progn (read-data data-type data-size data stream)
				(call/cc 
				 (lambda (k)
				   (setf cc k)
				   data))))))))))