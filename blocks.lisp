(in-package :tetris)

(defun [] (elements)
  "Quick 2D matrix function. Note: args must be of consistent size."
  (let ((h (length elements))
        (w (length (nth 0 elements))))
    (make-array (list h w) :initial-contents (reverse elements))))

(defstruct block-rotations
  r-0
  r-90
  r-180
  r-270)



(defparameter rotation-tests-others
  (list
   '(:0   . ('(:90  . '(0 0  -1 0  -1  1   0 -2  -1 -2))
             '(:270 . '(0 0   1 0   1  1   0 -2   1 -2))))
   '(:90  . ('(:0   . '(0 0   1 0   1 -1   0  2   1  2))
             '(:180 . '(0 0   1 0   1 -1   0  2   1  2))))
   '(:180 . ('(:90  . '(0 0  -1 0  -1  1   0 -2  -1 -2))
             '(:270 . '(0 0   1 0   1  1   0 -2   1 -2))))
   '(:270 . ('(:180 . '(0 0  -1 0  -1 -1   0  2  -1  2))
             '(:0   . '(0 0  -1 0  -1 -1   0  2  -1  2))))))

(defparameter rotation-tests-i
  (list
   '(:0   . ('(:90  . '(0 0  -2 0   1  0  -2 -1   1  2))
             '(:270 . '(0 0  -1 0   2  0  -1  2   2 -1))))
   '(:90  . ('(:0   . '(0 0   2 0  -1  0   2  1  -1 -2))
             '(:180 . '(0 0  -1 0   2  0  -1  2   2 -1))))
   '(:180 . ('(:90  . '(0 0   1 0  -2  0   1 -2  -2  1))
             '(:270 . '(0 0   2 0  -1  0   2  1  -1 -2))))
   '(:270 . ('(:180 . '(0 0  -2 0   1  0  -2 -1   1  2))
             '(:0   . '(0 0  -1 0  -2  0   1 -2  -2  1))))))


(defvar i-0 ([] '((0 0 0 0)
                  (1 1 1 1)
                  (0 0 0 0)
                  (0 0 0 0))))
(defvar i-90 ([] '((0 0 1 0)
                   (0 0 1 0)
                   (0 0 1 0)
                   (0 0 1 0))))
(defvar i-180 ([] '((0 0 0 0)
                    (0 0 0 0)
                    (1 1 1 1)
                    (0 0 0 0))))
(defvar i-270 ([] '((0 1 0 0)
                    (0 1 0 0)
                    (0 1 0 0)
                    (0 1 0 0))))
(defvar i-colour +cyan+)
(defvar i-rotations (make-block-rotations
                     :r-0 i-0
                     :r-90 i-90
                     :r-180 i-180
                     :r-270 i-270))

(defvar j-0 ([] '((1 0 0)
                  (1 1 1)
                  (0 0 0))))
(defvar j-90 ([] '((0 1 1)
                   (0 1 0)
                   (0 1 0))))
(defvar j-180 ([] '((0 0 0)
                    (1 1 1)
                    (0 0 1))))
(defvar j-270 ([] '((0 1 0)
                    (0 1 0)
                    (1 1 0))))
(defvar j-colour +orange+)
(defvar j-rotations (make-block-rotations
                     :r-0 j-0
                     :r-90 j-90
                     :r-180 j-180
                     :r-270 j-270))

(defvar l-0 ([] '((0 0 1)
                  (1 1 1)
                  (0 0 0))))
(defvar l-90 ([] '((0 1 0)
                   (0 1 0)
                   (0 1 1))))
(defvar l-180 ([] '((0 0 0)
                    (1 1 1)
                    (1 0 0))))
(defvar l-270 ([] '((1 1 0)
                    (0 1 0)
                    (0 1 0))))
(defvar l-colour +blue+)
(defvar l-rotations (make-block-rotations
                     :r-0 l-0
                     :r-90 l-90
                     :r-180 l-180
                     :r-270 l-270))

(defvar o-0 ([] '((0 1 1 0)
                  (0 1 1 0)
                  (0 0 0 0))))
(defvar o-90 ([] '((0 1 1 0)
                   (0 1 1 0)
                   (0 0 0 0))))
(defvar o-180 ([] '((0 1 1 0)
                    (0 1 1 0)
                    (0 0 0 0))))
(defvar o-270 ([] '((0 1 1 0)
                    (0 1 1 0)
                    (0 0 0 0))))
(defvar o-colour +yellow+)
(defvar o-rotations (make-block-rotations
                     :r-0 o-0
                     :r-90 o-90
                     :r-180 o-180
                     :r-270 o-270))

(defvar s-0 ([] '((0 1 1)
                  (1 1 0)
                  (0 0 0))))
(defvar s-90 ([] '((0 1 0)
                   (0 1 1)
                   (0 0 1))))
(defvar s-180 ([] '((0 0 0)
                    (0 1 1)
                    (1 1 0))))
(defvar s-270 ([] '((1 0 0)
                    (1 1 0)
                    (0 1 0))))
(defvar s-colour +green+)
(defvar s-rotations (make-block-rotations
                     :r-0 s-0
                     :r-90 s-90
                     :r-180 s-180
                     :r-270 s-270))

(defvar z-0 ([] '((1 1 0)
                  (0 1 1)
                  (0 0 0))))
(defvar z-90 ([] '((0 0 1)
                   (0 1 1)
                   (0 1 0))))
(defvar z-180 ([] '((0 0 0)
                    (1 1 0)
                    (0 1 1))))
(defvar z-270 ([] '((0 1 0)
                    (1 1 0)
                    (1 0 0))))
(defvar z-colour +red+)
(defvar z-rotations (make-block-rotations
                     :r-0 z-0
                     :r-90 z-90
                     :r-180 z-180
                     :r-270 z-270))

(defvar t-0 ([] '((0 1 0)
                  (1 1 1)
                  (0 0 0))))
(defvar t-90 ([] '((0 1 0)
                   (0 1 1)
                   (0 1 0))))
(defvar t-180 ([] '((0 0 0)
                    (1 1 1)
                    (0 1 0))))
(defvar t-270 ([] '((0 1 0)
                    (1 1 0)
                    (0 1 0))))
(defvar t-colour +purple+)
(defvar t-rotations (make-block-rotations
                     :r-0 t-0
                     :r-90 t-90
                     :r-180 t-180
                     :r-270 t-270))
