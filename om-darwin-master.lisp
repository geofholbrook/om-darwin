
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

         "dwn.criterion3"
         "dwn.criterion-utils" 

         ;reorganize these two files
         "dwn.engine"
         "dwn.om2"

         "dwn.maquette"
         


         "dwn.tests"
         
         ))


;--------------------------------------------------
;Load files 
;--------------------------------------------------

(mapc #'(lambda (file)

          ;; to populate dwn::*species-info-alist*
          (when (member file '("dwn.defspecies-builtin" "dwn.multi-cell" "dwn.om2") :test #'string-equal)
            (load (make-pathname :directory (append (pathname-directory *lib-folder*) 
                                                    (list "darwin-sources")) 
                                 :name file
                                 :type "lisp")))

          (compile&load (make-pathname :directory (append (pathname-directory *lib-folder*) 
                                                          (list "darwin-sources")) 
                                       :name file)))

      *source-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *darwin-subpackages* nil)
(setf *darwin-subpackages*

      '( ("species" nil nil (make-ga-chord
                             make-ga-chord-seq
                             make-even-melody 
                             make-grid-melody 
                             make-ga-tree
                             make-multi
                             make-stack
                             make-arrangement) nil) 
         ("criteria" nil nil (c-list 
                              c-pitches 
                              c-melodic
                              c-harmonic
                              c-oblique
                              c-regions
                              c-chords
                              c-voices
                              c-operons
                              c-block
                              c-pc
                              c-print
                              ))
         ("engine" nil (d::ga-engine) (evolute phenotype evaluate d::result d::start d::stop d::set-fitness-function) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(fill-library *darwin-subpackages*)

