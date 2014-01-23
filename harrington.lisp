(defmacro defrule (name &body body)
  `(defparameter ,name
     (expand-rule-orientations (quote ,@body))))


(defrule place-carrier
  (((- * * *)
    (- - - -)
    (- * * *))
   ((S = = =)
    (S S S S)
    (S = = =))))

(defrule place-hovercraft
  (((- - *)
    (* - -)
    (- - *))
   ((S S =)
    (= S S)
    (S S =))))

(defrule place-battleship
  (((- - - -))
   ((S S S S))))

(defrule place-cruiser
  (((- - -))
   ((S S S))))

(defrule place-destroyer
  (((- -))
   ((S S))))

(defparameter placement-rules-touching
  (list place-carrier
        place-hovercraft
        place-battleship
        place-cruiser
        place-destroyer))

(defparameter board
  (make-array '(12 12) :initial-element '-))

(defun north (s) s)
(defun east (s) (rot s))
(defun west (s) (rot (flip s)))
(defun south (s) (flip (rot (flip (rot s)))))

(defun rot (s)
  (apply #'mapcar #'list s))

(defun flip (s)
  (mapcar #'reverse s))

(defun many (s n)
  (loop for i from 1 to n
       collecting s))

(defun random-choice (choices)
  (elt choices (random (length choices))))

(defun expand-rule-orientations (rule)
  (mapcar
   (lambda (f) (list (funcall f (car rule))
                     (funcall f (cadr rule))))
   '(north south east west)))

(defun apply-placement-rules (rules board)
  (loop for ship in rules
     do (apply-one-rule-randomly ship board))
  board)

(defun apply-one-rule-randomly (rules board)
  (let ((matchrules (loop for rule in rules appending
                         (many rule (length (match (car rule) board))))))
    (apply-rule-randomly (random-choice matchrules) board)
    board))

(defun apply-some-rules-randomly (rules board)
  (loop for rule in rules do
       (apply-rule-randomly rule board))
  board)

(defun apply-rule-randomly (rule board)
  (let ((pos (random-match (car rule) board)))
    (when pos
      (destructuring-bind (r c) (random-match (car rule) board)
        (apply-action (cadr rule) board r c))
      board)))

(defun apply-rule-globally (rule board)
  (let ((matches (match (car rule) board)))
    (when matches
      (loop for (r c) in matches
         do (apply-action (cadr rule) board r c))
      board)))

(defun apply-action (action board start-row start-col)
  (loop
     for row in action
     for ri from start-row
     do (loop
           for point in row
           for ci from start-col
           do (apply-point point board ri ci)))
  board)

(defun apply-point (point board row col)
  (case point
    ((=) t)
    ((S) (setf (aref board row col) 'S))))

(defun random-match (pattern board)
  (let ((matches (match pattern board)))
    (when matches
      (random-choice matches))))

(defun match (pattern board)
  (loop for ri from 0 below (car (array-dimensions board))
     appending
       (loop for ci from 0 below (cadr (array-dimensions board))
             when
               (match-here pattern board ri ci)
             collect (list ri ci))))

(defun match-here (pattern board start-row start-col)
  (loop
     for row in pattern
     for ri from start-row
     always (loop
               for point in row
               for ci from start-col
               always (and (inside-board board ri ci)
                           (match-point point board ri ci)))))

(defun match-point (point board row col)
  (case point
    ((*) t)
    ((-) (eq (aref board row col)
             '-))))

(defun outside-board (board row col)
  (not (inside-board board row col)))

(defun inside-board (board row col)
  (and (< row (nrows board))
       (< col (nrows board)))))))

(defun nrows (board)
  (car (array-dimensions board)))

(defun ncols (board)
  (cadr (array-dimensions board)))
