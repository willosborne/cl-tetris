(in-package :tetris)

(defun generate-new-bag ()
  (alexandria:shuffle (list :l :j :t :o :s :z :i)))

(defvar *piece-queue* (generate-new-bag))
(defvar *piece-bag* nil)

(defun reset-piece-queue ()
  (setf *piece-bag* nil
        *piece-queue* (generate-new-bag)))

(defun get-next-bag-piece ()
  (if *piece-bag*
      (destructuring-bind (next . remaining) *piece-bag*
        (setf *piece-bag* remaining)
        next)
      (progn
        (setf *piece-bag* (generate-new-bag))
        (get-next-bag-piece))))

(defun enqueue-next-piece ()
  (nconc *piece-queue* (list (get-next-bag-piece))))

(defun get-next-queue-piece ()
  (enqueue-next-piece)
  (pop *piece-queue*))

