(defpackage #:xltable-parser-asd
  (:use :cl :asdf))

(in-package #:xltable-parser-asd)

(defsystem xltable-parser
  :name "xltable-parser"
  :version "0.0.1"
  :depends-on (:binary-types :cl-utilities :cl-cont :ieee-floats :babel :cffi)
  :components ((:file "xltable-parser")
	       (:file "xltable-ptr-parser")))