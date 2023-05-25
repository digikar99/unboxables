(defsystem "unboxables"
  :description "A simple wrapper around CFFI to enable contiguously allocated arrays of structures in Common Lisp."
  :author "Shubhamkar Ayare \"digikar\" (shubhamayare@yahoo.co.in)"
  :license "MIT"
  :depends-on ("cffi"
               "alexandria"
               "trivial-garbage")
  :serial t
  :components ((:file "package")
               (:file "unboxable-metadata")
               (:file "unboxables")
               (:file "row-major-aref")
               (:file "do-unboxables")))
