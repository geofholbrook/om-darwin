
(unless (find-package "OM-DARWIN") 
  (defpackage "OM-DARWIN" 
        (:nicknames "DWN" "D")
        (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))


(in-package om)

(export '(om::rrnd om::mki) "OM") 

;(require-library "om-geof")
;(require-library "om-enved")

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *lib-folder* nil)
(setf *lib-folder* (or *load-pathname* (om-choose-directory-dialog :prompt "om-darwin directory")))

(defvar *res-dir* nil)
(setf *res-dir* (append (pathname-directory *lib-folder*) (list "resources")))
                           
(defvar *source-files* nil)
(setf *source-files* 
      '( "dwn.utils"
         "dwn.basic"
         "dwn.arrange2"

         "dwn.defspecies"
         "dwn.multi-cell"
         "dwn.defspecies-builtin"   ;; rename
         "dwn.spec.trees"

         ;reorganize these two files
         "dwn.engine"
         "dwn.om2"

         "dwn.criterion2"
         "dwn.criterion-utils" 

         "dwn.tests"

         
        
         ))

;--------------------------------------------------
;Create globals needed for compile 
;--------------------------------------------------


;--------------------------------------------------
;Load files 
;--------------------------------------------------

(mapc #'(lambda (file) (let ((path
                              (make-pathname :directory (append (pathname-directory *lib-folder*) (list "darwin-sources")) 
                                             :name file)))
                         (load path)
                         (compile&load path)))
      *source-files*) 

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *darwin-subpackages* nil)
(setf *darwin-subpackages*

      '( ("species" nil nil (make-even-melody make-grid-melody make-ga-tree) nil) 
         ("criteria" nil nil (c-list c-pitch c-rate))
         ("engine" nil (d::ga-engine) (d::start d::stop d::set-fitness-function) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(fill-library *darwin-subpackages*)

