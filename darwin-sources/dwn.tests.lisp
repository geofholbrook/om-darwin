(in-package dwn)

(defvar macro-test nil)

;(setf macro-test (macroexpand-1 (quote
(defspecies walker

            :prefix w-

            :species-slots        ;;;; things that cannot be mutated. with defaults.

            (num-operons 10)       ;special ... all species with any operon slots needs this.

            (note-value 1/16) 
            (channel 1)


            :operon-slots         ;;;; properties of a single operon

            (starting-note :range (28 50)) 
            (intervals :range (1 3) :cardinality (4 15))
            (fifths-length :range (0 6))
            
            :phenotyper

            (make-even-melody (loop for unit in (w-operons self)
                                    append
                                    (let ((scale (dx->x (w-starting-note unit)
                                                        (w-intervals unit))))
                                      (append scale (loop repeat (w-fifths-length unit)
                                                          for k from 0
                                                          with last-note = (om::last-elem scale)
                                                          collect (incf last-note (if (evenp k) -7 5))))))
                              (w-note-value self)
                              :channel (w-channel self)))
;)))
