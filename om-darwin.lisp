
(unless (find-package "OM-DARWIN") 
  (defpackage "OM-DARWIN" 
        (:nicknames "DWN" "D")
        (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))


(in-package om)

(require-library "om-geof")
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
         "dwn.criterion-utils"
         "dwn.arrange2"

         "dwn.defspecies"
         "dwn.multi-cell"

         "dwn.defspecies-builtin"
         ; "dwn.defspecies-custom"    ;;; in user folder

         "dwn.engine"

         "dwn.om2"
        
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
                         (compile-file path)))
      *source-files*) 

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------


(defvar *darwin-subpackages* nil)
(setf *darwin-subpackages*
      '(("criteria" (("custom" nil nil (c-fun c-note c-chord c-melody c-block
                                              c-subchord c-subblock) nil)
                     ("special" nil nil (c-pset c-diatonish) nil))
                    nil
                    (c-list)
                    nil)

        ("specimens" (("standard" nil nil (s-melody s-chords om-spec) nil)
                      ("mapping" nil nil (s-mapmelody s-mapchords) nil))
                     nil
                     nil
                     nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------


(fill-library *darwin-subpackages*)

