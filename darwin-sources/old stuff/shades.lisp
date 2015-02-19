(in-package darwin)

(defun shades (lis &optional (min 0) (max 10))
  (let ((w (om-make-window 'om-window :size (om-make-point 
                                             (max 10 (* (length lis) 10))
                                             100))))
    (loop for num in lis
          for index from 0
          for grayscale = (- 1 (om-scale num 0 1 min max))
          do
          (om-with-focused-view w
            (om-with-fg-color nil (om-make-color grayscale grayscale grayscale)
              (om-fill-rect (* index 10) 0 10 100))))))

(defun 2D-shades (lis &optional (min 0) (max 10))
  (let ((w (om-make-window 'om-window :size (om-make-point 
                                             (max 100 (* (length (car lis)) 10))
                                             (max 100 (* (length lis) 30))))))
    (loop for sublis in lis
          for index1 from 0
          for y = (* index1 30)
          do
          (loop for num in sublis
                for index2 from 0
                for x = (* index2 10)
                for grayscale = (- 1 (om-scale num 0 1 min max))
                do
                (om-with-focused-view w
                  (om-with-fg-color nil (om-make-color grayscale grayscale grayscale)
                    (om-fill-rect x y 9 20)))))))



 
(defvar *shades-window* nil)

(defun x-from-index (index)
  (+ 10
     (* index 10)
     (* (floor index 3) 2)
     (* (floor index 9) 3)
     (* (floor index 27) 7)))

(defun y-from-num (num)
  (+ 150
     (* (- 1 num) 100)))

(defun rb-shades (lis &optional (min 0) (max 10) lines)
  (let ((w (or (and *shades-window*
                    (om-window-open-p *shades-window*)
                    *shades-window*)
               (setf *shades-window*
                     (om-make-window 'om-window :size (om-make-point 
                                                       (max 10 (* (length lis) 12))
                                                       280)))))
        prev-red prev-blue)

     (om-with-focused-view w
       (om-erase-rect 0 150 (om-point-h (om-view-size w)) 250))

    (loop for num in lis
          for index from 0
          for red = (- 1 (om-scale (first num) 0 1 min max))
          for blue = (- 1 (om-scale (second num) 0 1 min max))
          do
          
          (om-with-focused-view w
            
            

            ;color bars
            (om-with-fg-color nil (om-make-color red 0 blue)
              (om-fill-rect (x-from-index index) 
                            25 10 90))

            ;line graph
            (when (and prev-red lines)
              (om-with-fg-color nil *om-red-color*
                (om-draw-line (x-from-index (1- index))
                              (y-from-num prev-red)
                              (x-from-index index)
                              (y-from-num red)))
              (om-with-fg-color nil *om-blue-color*
                (om-draw-line (x-from-index (1- index))
                              (y-from-num prev-blue)
                              (x-from-index index)
                              (y-from-num blue))))
            )
            (setf prev-red red)
            (setf prev-blue blue))))





                             
          