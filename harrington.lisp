;;; a blottleships AI

;;; some definitions

(defstruct board
  (state (make-array '(12 12) :initial-element '?))
  (weights (make-array '(12 12) :initial-element 0)))

(defstruct rule
  pattern
  action)

(defstruct strategy
  weight
  rules)


;;; a few macros to make it easy to introduce named rules and strategies

(defmacro defrule (name &body body)
  `(defparameter ,name
     (expand-rule-orientations (make-rule :pattern (quote ,(car body)) :action (quote ,(cadr body))))))

(defmacro defstrategy (name weight &body rules)
  `(defparameter ,name
     (make-strategy :weight ,weight
                    :rules (append ,@rules))))


;;; we expand rules into their four orientations at declaration time

(defun rot (s) (apply #'mapcar #'list s))
(defun flip (s) (mapcar #'reverse s))

(defun north (s) s)
(defun east (s) (rot s))
(defun west (s) (rot (flip s)))
(defun south (s) (flip (rot (flip (rot s)))))

(defun expand-rule-orientations (rule)
  (mapcar
   (lambda (f) (make-rule :pattern (funcall f (rule-pattern rule))
                          :action (funcall f (rule-action rule))))
   '(north south east west)))


;;; next, we have a number of rules. A rule consists of a pattern and
;;; an action, each of which is a list of lists representing a
;;; two-dimensional pattern of matchers or action instructions

;;; supported matchers
;;; '*' : match anything
;;; '-' : match only an empty cell
;;; '×' : match only a cell whose value is outside the usable board
;;; '?' : match only a cell whose value is unknown
;;; 'X' : match only a cell in which a hit has been registered
;;; '/' : match either an unknown cell or a hit cell (effectively ?|X)

;;; supported actions
;;; '=' : leave the cell unchanged
;;; 'S' : place part of a ship in the cell
;;; '↑' : increment the weight associated with the cell
;;; '↓' : decrement the weight associated with the cell


;;; placement rules---these are used to generate boards

(defrule place-carrier
  ((- * * *)
   (- - - -)
   (- * * *))
  ((S = = =)
   (S S S S)
   (S = = =)))

(defrule place-hovercraft
  ((- - *)
   (* - -)
   (- - *))
  ((S S =)
   (= S S)
   (S S =)))

(defrule place-battleship
  ((- - - -))
  ((S S S S)))

(defrule place-cruiser
  ((- - -))
  ((S S S)))

(defrule place-destroyer
  ((- -))
  ((S S)))


;;; Exploration rules

;;; Exploration rules attempt to place ships across the whole board
;;; and increase the weighting of a square according to the
;;; number of possible ship placements to which it contributes

(defrule explore-carrier
  ((/ * * *)
   (/ / / /)
   (/ * * *))
  ((↑ = = =)
   (↑ ↑ ↑ ↑)
   (↑ = = =)))

(defrule explore-hovercraft
  ((/ / *)
   (* / /)
   (/ / *))
  ((↑ ↑ =)
   (= ↑ ↑)
   (↑ ↑ =)))

(defrule explore-battleship
  ((/ / / /))
  ((↑ ↑ ↑ ↑)))

(defrule explore-cruiser
  ((/ / /))
  ((↑ ↑ ↑)))

(defrule explore-destroyer
  ((/ /))
  ((↑ ↑)))

;;; Recognition rules

;;; Recognition rules find particular patterns of hits that suggest
;;; incomplete ships, and attempt to complete them

(defrule recognise-neighbour
  ((X ?))
  ((= ↑)))

(defrule recognise-line
  ((X ? X))
  ((= ↑ =)))

(defrule recognise-corner
  ((? X)
   (X *))
  ((↑ =)
   (= =)))

(defrule recognise-tee
  ((X ? X)
   (* X *))
  ((= ↑ =)
   (= = =)))

(defrule recognise-destroyer
  ((* - - *)
   (- X ? -)
   (* - - *))
  ((= = = =)
   (= = ↑ =)
   (= = = =)))

(defrule recognise-cruiser-centre
  ((* * - * *)
   (- X ? X -)
   (* * - * *))
  ((= = = = =)
   (= = ↑ = =)
   (= = = = =)))

(defrule recognise-cruiser-end
  ((* * - * *)
   (- X X ? -)
   (* * - * *))
  ((= = = = =)
   (= = = ↑ =)
   (= = = = =)))

(defrule recognise-battleship-end
  ((* * - - * *)
   (- X X X ? -)
   (* * - - * *))
  ((= = = = = =)
   (= = = = ↑ =)
   (= = = = = =)))

(defrule recognise-battleship-centre
  ((* * - - * *)
   (- X X ? X -)
   (* * - - * *))
  ((= = = = = =)
   (= = = ↑ = =)
   (= = = = = =)))

;;; Elimination rules

;;; Elimination rules recognise patterns of unknown cells, and attempt
;;; to explore them in the way which gives the most information in the
;;; fewest shots (by attempting to create 'orphaned' cells which could
;;; never contribute to a ship).

(defrule eliminate-cruciform
  ((* ? *)
   (? ? ?)
   (* ? *))
  ((= = =)
   (= ↑ =)
   (= = =)))

(defrule eliminate-line
  ((? ? ?))
  ((= ↑ =)))

;;; Ignore rules

;;; Ignore rules recognise cells which are obviously not worth
;;; pursuing (such as cells which have already been shot at). This
;;; acts similarly to a 'stop list', and allows the other patterns to
;;; be written in a much more concise form.

(defrule ignore-hits
  ((X))
  ((↓)))

(defrule ignore-misses
  ((-))
  ((↓)))

(defrule ignore-offboard
  ((×))
  ((↓)))

(defrule ignore-checkerboard
  ((* - *)
   (- * -)
   (* - *))
  ((= = =)
   (= ↓ =)
   (= = =)))


;;; Strategies

;;; A strategy is a collection of rules with an associated weighting.

(defstrategy exploration
    1
  explore-destroyer
  explore-cruiser
  explore-battleship
  explore-hovercraft
  explore-carrier)

(defstrategy recognition
    10
  recognise-neighbour
  recognise-line
  recognise-corner
  recognise-tee
  recognise-destroyer
  recognise-cruiser-end
  recognise-cruiser-centre
  recognise-battleship-end
  recognise-battleship-centre)

(defstrategy norepeat    
    10000
  ignore-misses
  ignore-hits
  ignore-checkerboard
  ignore-offboard)

(defstrategy elimination
    2
  eliminate-line
  eliminate-cruciform)



(defparameter placement-rules-touching
  (list place-carrier
        place-hovercraft
        place-battleship
        place-cruiser
        place-destroyer))

(defparameter the-board
  (make-board))

(defvar *weight-increment*
  1)

(defun many (s n)
  (loop for i from 1 to n
     collecting s))

(defun random-choice (choices)
  (elt choices (random (length choices))))

(defun apply-strategies (strategies board)
  (loop for strategy in strategies do
       (apply-strategy strategy board))
  board)

(defun apply-strategy (strategy board)
  (let ((*weight-increment* (strategy-weight strategy)))
    (apply-all-rules (strategy-rules strategy) board))
  board)

(defun apply-placement-rules (rules board)
  (loop for ship in rules
     do (apply-one-rule-randomly ship board))
  board)

(defun apply-one-rule-randomly (rules board)
  (let ((matchrules (loop for rule in rules appending
                         (many rule (length (match (rule-pattern rule) board))))))
    (apply-rule-randomly (random-choice matchrules) board)
    board))

(defun apply-all-rules (rules board)
  (loop for rule in rules do
       (apply-rule-globally rule board))
  board)

(defun apply-some-rules-randomly (rules board)
  (loop for rule in rules do
       (apply-rule-randomly rule board))
  board)

(defun apply-rule-randomly (rule board)
  (let ((pos (random-match (rule-pattern rule) board)))
    (when pos
      (destructuring-bind (r c) (random-match (rule-pattern rule) board)
        (apply-action (rule-action rule) board r c))
      board)))

(defun apply-rule-globally (rule board)
  (let ((matches (match (rule-pattern rule) board)))
    (when matches
      (loop for (r c) in matches
         do (apply-action (rule-action rule) board r c))
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
    ((S) (setf (aref (board-state board) row col) 'S))
    ((↑) (incf (aref (board-weights board) row col) *weight-increment*))
    ((↓) (decf (aref (board-weights board) row col) *weight-increment*))))

(defun random-match (pattern board)
  (let ((matches (match pattern board)))
    (when matches
      (random-choice matches))))

(defun match (pattern board)
  (loop for ri from 0 below (nrows board)
     appending
       (loop for ci from 0 below (ncols board)
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
  (let ((state-at-point (aref (board-state board) row col)))
    (case point
      ((*) t)
      ((×) (eq state-at-point '×))
      ((-) (eq state-at-point '-))
      ((?) (eq state-at-point '?))
      ((X) (eq state-at-point 'X))
      ((/) (member state-at-point '(? X))))))

(defun outside-board (board row col)
  (not (inside-board board row col)))

(defun inside-board (board row col)
  (and (< row (nrows board))
       (< col (nrows board))))

(defun nrows (board)
  (car (array-dimensions (board-state board))))

(defun ncols (board)
  (cadr (array-dimensions (board-state board))))

(defun max-weight (board)
  (loop for r from 0 below (nrows board) maximizing
       (loop for c from 0 below (ncols board) maximizing
            (aref (board-weights board) r c))))

(defun best-moves (board)
  (let ((max-weight (max-weight board)))
    (loop for r from 0 below (nrows board) appending
         (loop for c from 0 below (ncols board)
            when (= (aref (board-weights board) r c) max-weight)
            collect (list r c)))))

(defun choose-move (board)
  (random-choice (best-moves board)))

(defun count-hits (board)
  (loop for r below (nrows board) summing
       (loop for c below (ncols board) counting
            (eq (aref (board-state board) r c) 'X))))

(defun setup-unreachable (board)
  (loop for r from 0 below 6 do
       (loop for c from 0 below 6 do
            (setf (aref (board-state board) r c) '×))))

(defun play ()
  (let ((target-board (make-board))
        (player-board (make-board))
        (move-counter 0))
    (setf (board-state target-board)
          (make-array '(12 12) :initial-element '-))
    (setup-unreachable target-board)
    (setup-unreachable player-board)
    (apply-placement-rules placement-rules-touching target-board)
    (loop
       until (or (= (count-hits player-board) 21)
                 (> move-counter 200))
       do
         (setf (board-weights player-board)
               (make-array '(12 12) :initial-element 0))
       ;; (preseed-checkerboard player-board)
         (apply-strategies (list exploration recognition norepeat) player-board)
         (flatten-weights player-board)
         (destructuring-bind (r c) (choose-move player-board)
           (setf (aref (board-state player-board) r c)
                 (if (eq (aref (board-state target-board) r c) 'S)
                     'X
                     '-)))
         (incf move-counter))
    (list target-board player-board move-counter)))

(defun preseed-checkerboard (board)
  (loop for r from 0 below (nrows board) by 2 do
       (loop for c from 0 below (ncols board) by 2 do
            (incf (aref (board-weights board) r c) 1))))

(defun flatten-weights (board)
  (loop for r from 0 below (nrows board) do
       (loop for c from 0 below (ncols board) do
            (when (> 0 (aref (board-weights board) r c))
              (setf (aref (board-weights board) r c) 0)))))

(defun play-many-with-averages (n)
  (float (let ((move-counts (loop for i from 1 to n
                               collecting (caddr (play)))))
           (/ (reduce #'+ move-counts) (length move-counts)))))
