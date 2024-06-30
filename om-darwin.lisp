
 (unless (find-package "OM-DARWIN") 
   (defpackage "OM-DARWIN" 
         (:nicknames "DWN" "D")
         (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))

 (in-package om)
 (export '(om::rrnd om::mki) "OM")

 (defvar *lib-folder* nil)
 (setf *lib-folder* (or *load-pathname* (om-choose-directory-dialog :prompt "om-darwin directory")))

 (defvar *res-dir* nil)
 (setf *res-dir* (append (pathname-directory *lib-folder*) (list "resources")))

 (defvar *source-files* nil)
 (setf *source-files* 
       '( "http"
          "index"
          "tests" ))

 (mapc #'(lambda (file)
       (compile&load (make-pathname :directory (append (pathname-directory *lib-folder*) 
                                                           (list "src")) 
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

