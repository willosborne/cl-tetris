;;;; tetris.lisp

(in-package #:tetris)


(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Swank.Live: Continue")))

(defmacro nlet (name let-vars &body body)
  `(labels ((,name ,(mapcar #'car let-vars)
              ,@body))
     (,name ,@(mapcar #'cadr let-vars))))

(defvar *origin* (gamekit:vec2 0 0))

(defparameter *block-size* 15)

(defparameter *grid-width* 10)
(defparameter *grid-height* 20)

(defparameter *game-width* (* *grid-width* *block-size*))
(defparameter *game-height* (* *grid-height* *block-size*))

(defparameter *window-width* (* (+ 5 *grid-width*) *block-size*))
(defparameter *window-height* (* *grid-height* *block-size*))

(defparameter *spawn-x* 5)
(defparameter *spawn-y* 20)

(defparameter *score* 0)

(defparameter *level* 0)

(defparameter *paused* nil)

(defparameter *hold-piece* nil)

(defun update-score (num-lines level)
  (incf *score* (ecase num-lines
                  (0 0)
                  (1 (* 100 (1+ level)))
                  (2 (* 300 (1+ level)))
                  (3 (* 300 (1+ level)))
                  (4 (* 1200 (1+ level))))))

(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defparameter *last-update* (real-time-seconds))
(defparameter *update-delay* 0.5)


(gamekit:defgame tetris-game () ()
  (:viewport-width  *window-width*)
  (:viewport-height *window-height*))

(defparameter *grid* (make-array (list *grid-height*
                                       *grid-width*)
                                 :initial-element nil))

(defvar *current-piece* nil)
(defvar *current-piece-x* nil)
(defvar *current-piece-y* nil)
(defvar *current-piece-rotation* nil)


(defmacro loop-for-2d (i j cell-value grid &body body)
  (alexandria:with-gensyms (x y)
    `(destructuring-bind (,y ,x) (array-dimensions ,grid)
       (loop for ,j from 0 below ,y do
         (loop for ,i from 0 below ,x do
           (let ((,cell-value (aref ,grid ,j ,i)))
             ,@body))))))

(defun spawn-piece (x y type)
  (setf *current-piece* type
        *current-piece-x* x
        *current-piece-y* y
        *current-piece-rotation* :0)
  nil)

(defun spawn-next-piece (x y)
  (let ((piece (get-next-queue-piece)))
    (spawn-piece x y piece)))

(defun get-piece-rotation (type rotation)
  (let* ((piece-rotations (ecase type
                            (:l l-rotations)
                            (:j j-rotations)
                            (:o o-rotations)
                            (:s s-rotations)
                            (:z z-rotations)
                            (:i i-rotations)
                            (:t t-rotations))))
    (ecase rotation
      (:0 (block-rotations-r-0 piece-rotations))
      (:90 (block-rotations-r-90 piece-rotations))
      (:180 (block-rotations-r-180 piece-rotations))
      (:270 (block-rotations-r-270 piece-rotations)))))

(defun in-range (x y)
  (and (>= x 0)
       (< x *grid-width*)
       (>= y 0)
       ;; (< y *grid-height*)
       ))

(defun piece-fits-p (x y type rotation grid)
  (let ((piece (get-piece-rotation type rotation)))
    (loop-for-2d i j cell piece
      (let ((px (+ x i))
            (py (+ y j)))
        (when (and cell
                   (not (= cell 0))
                   (or
                    (not (in-range px py))
                    (aref grid py px)))
          (return-from piece-fits-p nil))))
    t))

(defun lock-in-piece (x y type rotation)
  (let ((piece (get-piece-rotation type rotation)))
    (when (piece-fits-p x y type rotation *grid*)
      (loop-for-2d piece-i piece-j value piece
        (when (and value (not (= value 0)))
          (setf (aref *grid* (+ y piece-j) (+ x piece-i)) type)))
      t)))

(defun draw-cell (x y type)
  (let ((colour (ecase type
                  (:l l-colour)
                  (:j j-colour)
                  (:o o-colour)
                  (:s s-colour)
                  (:z z-colour)
                  (:i i-colour)
                  (:t t-colour))))
    (gamekit:draw-rect (gamekit:vec2 (* x *block-size*)
                                     (* y *block-size*))
                       *block-size*
                       *block-size*
                       :fill-paint colour)))

(defun draw-piece (x y type rotation)
  (let ((piece (get-piece-rotation type rotation)))
    (loop-for-2d piece-i piece-j value piece
      (when (and value (not (= value 0)))
        (draw-cell (+ x piece-i)
                   (+ y piece-j)
                   type)))))

(defun draw-grid (grid)
  (loop-for-2d i j cell grid
    (when cell
      (draw-cell i j cell))))

(defun reset ()
  (setf *grid* (make-array '(40 10) :initial-element nil)
        *last-update* (real-time-seconds)
        *score* 0
        *hold-piece* nil)
  (reset-piece-queue)
  t)

(defun draw-queue ()
  (nlet recur ((queue *piece-queue*)
               (y (- *grid-height* 4))
               (count 5))
    (when (> count 0)
      (draw-piece (+ *grid-width* 1) y
                  (car queue) :0)
      (recur (cdr queue)
             (- y
                (array-dimension (get-piece-rotation (car queue) :0) 0))
             (1- count)))))

(defmethod gamekit:draw ((app tetris-game))
  (gamekit:draw-rect *origin* *window-width* *window-height*
                     :fill-paint (gamekit:vec4 0 0 0 1))
  (gamekit:draw-rect *origin*
                     (* *block-size* 10)
                     (* *block-size* 40)
                     :fill-paint (gamekit:vec4 0.6 0.6 0.6 1))
  (draw-grid *grid*)
  (when (and *current-piece* *current-piece-x* *current-piece-y*)
    (draw-piece *current-piece-x* *current-piece-y* *current-piece* *current-piece-rotation*))
  (continuable (draw-queue)))

(defmethod gamekit:act ((app tetris-game))
  (let ((time (real-time-seconds)))
    (when (>= (- time *last-update*) *update-delay*)
      (when (not *paused*)
        (soft-drop))
      (setf *last-update* time))))

(defun try-move-piece (dx dy)
  (when *current-piece*
    (if (piece-fits-p (+ *current-piece-x* dx) (+ *current-piece-y* dy)
                      *current-piece* *current-piece-rotation* *grid*)
        (progn
          (setf *current-piece-x* (+ *current-piece-x* dx)
                *current-piece-y* (+ *current-piece-y* dy))
          t)
        nil)))

(defun try-hard-drop ()
  (when *current-piece*
    (loop for y downfrom *current-piece-y*
          until (not
                 (piece-fits-p
                  *current-piece-x* (1- y)
                  *current-piece*
                  *current-piece-rotation*
                  *grid*))
          finally (lock-in-piece *current-piece-x* y
                                 *current-piece*
                                 *current-piece-rotation*)
                  (clear-lines)
                  (spawn-next-piece *spawn-x* *spawn-y*))))

(defun soft-drop ()
  (when *current-piece*
    (unless (try-move-piece 0 -1)
      ;; piece was blocked
      (lock-in-piece *current-piece-x* *current-piece-y*
                     *current-piece*
                     *current-piece-rotation*)
      (clear-lines)
      (spawn-next-piece *spawn-x* *spawn-y*))))

(defun clear-single-line (cleared-y)
  (loop for y from cleared-y below (1- *grid-height*) do
    (loop for x from 0 below *grid-width* do
      (setf (aref *grid* y x) (aref *grid* (1+ y) x))))
  (loop for x from 0 below *grid-width* do
        (setf (aref *grid* (1- *grid-height*) x) nil)))

(defun line-filled-p (y)
  (loop for x from 0 below *grid-width*
        always (aref *grid* y x)))

(defun clear-lines ()
  (let ((lines-cleared 0))
    (loop for y from 0 below *grid-height* do
      (loop while (line-filled-p y) do
        (clear-single-line y)
        (incf lines-cleared)))
    (when (> lines-cleared 0)
      (format t "Cleared ~a lines.~%" lines-cleared))
    (update-score lines-cleared *level*)))

(defun get-new-rotation (old-rot direction)
  (ecase direction
    (:clockwise (ecase old-rot
                  (:0 :90)
                  (:90 :180)
                  (:180 :270)
                  (:270 :0)))
    (:anticlockwise (ecase old-rot
                      (:0 :270)
                      (:90 :0)
                      (:180 :90)
                      (:270 :180)))))

(defun try-rotate-piece (direction)
  (when *current-piece*
    (let* ((new-rot (get-new-rotation *current-piece-rotation* direction))
           ;; (piece (get-piece-rotation *current-piece* new-rot))
           (kicks-tree (if (eq *current-piece* :i)
                           rotation-tests-i
                           rotation-tests-others))
           (kicks (get-kicks kicks-tree *current-piece-rotation* new-rot)))
      (loop for (dx . dy) in (mapcar #'cons kicks (cdr kicks)) by #'cddr
            do
               (when (piece-fits-p (+ *current-piece-x* dx)
                                   (+ *current-piece-y* dy)
                                   *current-piece* new-rot *grid*)
                 (setf *current-piece-rotation* new-rot
                       *current-piece-x* (+ *current-piece-x* dx)
                       *current-piece-y* (+ *current-piece-y* dy))
                 (return t))))))

(defun swap-hold-piece ()
  (let ((current *current-piece*))
    (if *hold-piece*
        (setf *current-piece* *hold-piece*
              *hold-piece* current
              *current-piece-x* *spawn-x*
              *current-piece-y* *spawn-y*)
        (progn
          (setf *hold-piece* *current-piece*)
          (spawn-next-piece *spawn-x* *spawn-y*)))))

(defun start ()
  (gamekit:start 'tetris-game)
  (flet ((move-left ()
           (try-move-piece -1 0))
         (move-right ()
           (try-move-piece 1 0))
         (rotate-cw ()
           (try-rotate-piece :clockwise))
         (rotate-ccw ()
           (try-rotate-piece :anticlockwise))
         )
    (gamekit:bind-button
     :left :repeating
     #'move-left)
    (gamekit:bind-button
     :left :pressed
     #'move-left)

    (gamekit:bind-button
     :right :repeating
     #'move-right)
    (gamekit:bind-button
     :right :pressed
     #'move-right)


    (gamekit:bind-button
     :z :pressed
     #'rotate-ccw)

    (gamekit:bind-button
     :x :pressed
     #'rotate-cw)

    (gamekit:bind-button
     :r :pressed
     #'reset)


    (gamekit:bind-button
     :space :pressed
     #'swap-hold-piece)

    (gamekit:bind-button
     :down :repeating
     #'soft-drop)
    (gamekit:bind-button
     :down :pressed
     #'soft-drop)
    (gamekit:bind-button
     :up :pressed
     #'try-hard-drop)))

(defun start-game ()
  (reset)
  (start))
