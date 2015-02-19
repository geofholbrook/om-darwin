(in-package om)

(unless (find-package "OM-DARWIN") 
  (defpackage "OM-DARWIN" 
        (:nicknames "DWN" "D")
        (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))