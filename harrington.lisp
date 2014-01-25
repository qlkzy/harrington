;;; a blottleships AI

;;; We define a few structures to represent important concepts:

(defstruct board
  (state (make-array '(12 12) :initial-element '?))
  (weights (make-array '(12 12) :initial-element 0)))

(defstruct rule
  pattern
  action)

(defstruct strategy
  weight
  rules)

;;; Rules and strategies come up often, so we define a couple of
;;; macros to make it easy to introduce named rules and strategies:

(defmacro defrule (name &body body)
  `(defparameter ,name
     (expand-rule-orientations (make-rule :pattern (quote ,(car body)) :action (quote ,(cadr body))))))

(defmacro defstrategy (name weight &body rules)
  `(defparameter ,name
     (make-strategy :weight ,weight
                    :rules (append ,@rules))))

;;; When we declare a rule, we expand it into a list of for rules: one
;;; for each orientation (referred to here as north, south, east, and
;;; west). The two-dimensional list representing each part of the rule
;;; is massaged into each of these orientations using a combination of
;;; rotations (transforming row-major to column-major ordering, and
;;; vice-versa) and flips (reversing the minor lists).

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

;;; Boards are arrays; dealing with their dimensions is moderately
;;; verbose, so we introduce a few utility functions for the two
;;; dimensions we care about.

(defun nrows (board)
  (car (array-dimensions (board-state board))))

(defun ncols (board)
  (cadr (array-dimensions (board-state board))))

(defun inside-board (board row col)
  (and (>= row 0)
       (>= col 0)
       (< row (nrows board))
       (< col (nrows board))))

(defun outside-board (board row col)
  (not (inside-board board row col)))

;;; A few miscellaneous utility functions, to replicate a list, and to
;;; make a random choice from a list.

(defun many (s n)
  (loop for i from 1 to n
     collecting s))

(defun random-choice (choices)
  (elt choices (random (length choices))))



;;; The whole program basically consists of repeatedly applying rules
;;; which change the state and weights of the board.

;;; A rule consists of a pattern and an action, each of which is a
;;; list of lists representing a two-dimensional pattern of 'matchers'
;;; or 'actions'.

;;; supported matchers
;;; '*' : match anything
;;; '-' : match only an empty cell
;;; '×' : match only a cell whose value is outside the usable board
;;; '?' : match only a cell whose value is unknown
;;; 'X' : match only a cell in which a hit has been registered
;;; '/' : match either an unknown cell or a hit cell (effectively ?|X)
;;; '÷' : match any cell where a hit *cannot* be (effectively -|×)

;;; supported actions
;;; '=' : leave the cell unchanged
;;; 'S' : place part of a ship in the cell
;;; '~' : set the cell to 'empty'
;;; '↑' : increment the weight associated with the cell
;;; '↓' : decrement the weight associated with the cell



;;; Placment rules

;;; Placement rules are used to construct a board at the start of the
;;; game.

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

;;; Non-touching placement rules

;;; These variants of the placement rules require that the ships not
;;; be touching.


(defrule place-carrier-no-touching
  ((÷ ÷ ÷ * * *)
   (÷ - ÷ ÷ ÷ ÷)
   (÷ - - - - ÷)
   (÷ - ÷ ÷ ÷ ÷)
   (÷ ÷ ÷ * * *))
  ((= = = = = =)
   (= S = = = =)
   (= S S S S =)
   (= S = = = =)
   (= = = = = =)))

(defrule place-hovercraft-no-touching
  ((÷ ÷ ÷ ÷ *)
   (÷ - - ÷ ÷)
   (* ÷ - - ÷)
   (÷ - - ÷ ÷)
   (÷ ÷ ÷ ÷ *))
  ((= = = = =)
   (= S S = =)
   (= = S S =)
   (= S S = =)
   (= = = = =)))

(defrule place-battleship-no-touching
  ((÷ ÷ ÷ ÷ ÷ ÷)
   (÷ - - - - ÷)
   (÷ ÷ ÷ ÷ ÷ ÷))
  ((= = = = = =)
   (= S S S S =)
   (= = = = = =)))

(defrule place-cruiser-no-touching
  ((÷ ÷ ÷ ÷ ÷)
   (÷ - - - ÷)
   (÷ ÷ ÷ ÷ ÷))
  ((= = = = =)
   (= S S S =)
   (= = = = =)))

(defrule place-destroyer-no-touching
  ((÷ ÷ ÷ ÷)
   (÷ - - ÷)
   (÷ ÷ ÷ ÷))
  ((= = = =)
   (= S S =)
   (= = = =)))



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
  ((* ÷ ÷ *)
   (÷ X ? ÷)
   (* ÷ ÷ *))
  ((= = = =)
   (= = ↑ =)
   (= = = =)))

(defrule recognise-cruiser-centre
  ((* * ÷ * *)
   (÷ X ? X ÷)
   (* * ÷ * *))
  ((= = = = =)
   (= = ↑ = =)
   (= = = = =)))

(defrule recognise-cruiser-end
  ((* * ÷ * *)
   (÷ X X ? ÷)
   (* * ÷ * *))
  ((= = = = =)
   (= = = ↑ =)
   (= = = = =)))

(defrule recognise-battleship-end
  ((* * ÷ ÷ * *)
   (÷ X X X ? ÷)
   (* * ÷ ÷ * *))
  ((= = = = = =)
   (= = = = ↑ =)
   (= = = = = =)))

(defrule recognise-battleship-centre
  ((* * ÷ ÷ * *)
   (÷ X X ? X ÷)
   (* * ÷ ÷ * *))
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

(defrule eliminate-corner
  ((* ÷ *)
   (÷ ? ?)
   (* ? *))
  ((= = =)
   (= ↓ ↑)
   (= ↑ =)))

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
  ((* ÷ *)
   (÷ ? ÷)
   (* ÷ *))
  ((= = =)
   (= ↓ =)
   (= = =)))


;;; Inference Rules

;;; Inference rules change the state of the board to make implicit
;;; knowledge (such as that from the ignore-checkerboard rule)
;;; explicit in the board. This information should generally be
;;; superfluous if other rules have been written correctly, but these
;;; rules provide a little extra assurance.

(defrule infer-checkerboard
  ((* ÷ *)
   (÷ ? ÷)
   (* ÷ *))
  ((= = =)
   (= ~ =)
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
    40
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
  eliminate-cruciform
  eliminate-corner)


(defstrategy inference
    0
  infer-checkerboard)

;;; a few global-type things

(defparameter placement-rules-touching
  (list place-carrier
        place-hovercraft
        place-battleship
        place-cruiser
        place-destroyer))

(defparameter placement-rules-no-touching
  (list place-carrier-no-touching
        place-hovercraft-no-touching
        place-battleship-no-touching
        place-cruiser-no-touching
        place-destroyer-no-touching))

(defparameter the-board
  (make-board))

(defvar *weight-increment*
  1)


;;; Rule Matching

;;; First, we must define what it means for a matcher to match. For
;;; most matchers, this just means checking whether the matcher
;;; corresponds to the value in the state. Wildcards and the
;;; ship-not-allowed marker are a little more nuanced, but still quite
;;; simple.

(defun match-point (point board row col)
  (or
   ;; wildcard matches anywhere
   (eq point '*)
   ;; state matchers just lookup the value
   (and (inside-board board row col)
        (let ((state-at-point (aref (board-state board) row col)))
          (case point
            ((×) (eq state-at-point '×))
            ((-) (eq state-at-point '-))
            ((?) (eq state-at-point '?))
            ((X) (eq state-at-point 'X))
            ((÷) (member state-at-point '(× -)))
            ((/) (member state-at-point '(? X))))))
   ;; ship-not-allowed matcher can also match outside the board
   (and (outside-board board row col)
        (eq point '÷))))


;;; For a pattern to match at a location, we simply require that each
;;; of its individual matchers match at the corresponding offset

(defun match-here (pattern board start-row start-col)
  (loop
     for row in pattern
     for ri from start-row
     always (loop
               for point in row
               for ci from start-col
               always (match-point point board ri ci))))

;;; To match a pattern against the board, we attempt to match it
;;; across the entire board, and collect a list of those points where
;;; the pattern matched the pattern m

(defun match (pattern board)
  (loop for ri from -1 to (nrows board)
     appending
       (loop for ci from -1 to (ncols board)
          when
            (match-here pattern board ri ci)
          collect (list ri ci))))


;;; Rule actions

;;; Applying a single point of an action is a simple case analysis:

(defun apply-point (point board row col)
  (case point
    ;; unchanged
    ((=) t)
    ;; adjust state
    ((S) (setf (aref (board-state board) row col) 'S))
    ((~) (setf (aref (board-state board) row col) '-))
    ;; adjust weights
    ((↑) (incf (aref (board-weights board) row col) *weight-increment*))
    ((↓) (decf (aref (board-weights board) row col) *weight-increment*))))

;;; Applying an action is simply a matter of applying all of its
;;; points in turn

(defun apply-action (action board start-row start-col)
  (loop
     for row in action
     for ri from start-row
     do (loop
           for point in row
           for ci from start-col
           do (apply-point point board ri ci))))


;;; Ship placement

;;; Ship placement is a special case of rule application; whereas with
;;; almost all rules, we want to apply them as often as possible
;;; across the board, ship placement rules should each be applied
;;; exactly once.

;;; As we want to apply a rule at random, we clearly need to be able
;;; to choose a random position from those at which the rule matches:

(defun random-match (pattern board)
  (let ((matches (match pattern board)))
    (when matches
      (random-choice matches))))

;;; And then take that random position and apply the action of the
;;; rule to it

(defun apply-rule-randomly (rule board)
  "Apply RULE to BOARD at a random choice from the locations at which
it matches."
  (let ((pos (random-match (rule-pattern rule) board)))
    (when pos
      (destructuring-bind (r c) (random-match (rule-pattern rule) board)
        (apply-action (rule-action rule) board r c)))))

;;; As each ship has an number of placement rules (one for each
;;; orientation, and possibly more for touching/not touching other
;;; ships), we must choose randomly from the possible rules. We do
;;; this by counting the number of times each rule matches, and
;;; replicating it that number of times. We can then make a
;;; straightforward random choice between each of these rules, and
;;; apply it.

(defun apply-one-rule-randomly (rules board)
  "Apply one of RULES to BOARD at random; the probability that each
rule will be chosen is a function of how many times it matches BOARD."
  (let ((matchrules (loop for rule in rules appending
                         (many rule (length (match (rule-pattern rule) board))))))
    (apply-rule-randomly (random-choice matchrules) board)))


;;; With our random-rule-application functions now defined, all we
;;; need to do is use them once for each ship.

(defun apply-placement-rules (ship-rules board)
  (loop for ship in ship-rules
     do (apply-one-rule-randomly ship board)))


;;; General rule application

(defun apply-rule-globally (rule board)
  (let ((matches (match (rule-pattern rule) board)))
    (when matches
      (loop for (r c) in matches
         do (apply-action (rule-action rule) board r c)))))

(defun apply-all-rules (rules board)
  (loop for rule in rules do
       (apply-rule-globally rule board)))


;;; Strategies

(defun apply-strategy (strategy board)
  (let ((*weight-increment* (strategy-weight strategy)))
    (apply-all-rules (strategy-rules strategy) board)))


(defun apply-strategies (board &rest strategies)
  (loop for strategy in strategies do
       (apply-strategy strategy board)))




;;; Choosing moves

;;; Enormous negative numbers make the weights data structure hard to
;;; read; they carry no more information than zero, so we can just
;;; throw them away.

(defun flatten-weights (board)
  (loop for r from 0 below (nrows board) do
       (loop for c from 0 below (ncols board) do
            (when (> 0 (aref (board-weights board) r c))
              (setf (aref (board-weights board) r c) 0)))))

;;; Some strategies can be improved by overlaying a global
;;; checkerboard pattern over the game board: this can help avoid
;;; wasteful miss checks

(defun preseed-checkerboard (board weight)
  (loop for r from 0 below (nrows board) by 2 do
       (loop for c from 0 below (ncols board) by 2 do
            (incf (aref (board-weights board) r c) weight))))

;;; The 'best' moves at each point are those which hit the
;;; highest-weighted cell. We do this in two passes: one pass over the
;;; weights finds the maximum, and a second pass finds all coordinates
;;; at which that maximum appears.

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

;;; Once we have a list of the "best" moves, we can just pick one at
;;; random.

(defun choose-move (board)
  (random-choice (best-moves board)))



;;; Boards need a little more initialisation than just creating a
;;; 12x12 array, because of the cutout:

(defun setup-unreachable (board)
  (loop for r from 0 below 6 do
       (loop for c from 0 below 6 do
            (setf (aref (board-state board) r c) '×))))

;;; In order to determine when the game has finished (and possibly as
;;; input to certain kinds of phased strategy), we need to know how
;;; many hits a player has scored:

(defun count-hits (board)
  (loop for r below (nrows board) summing
       (loop for c below (ncols board) counting
            (eq (aref (board-state board) r c) 'X))))


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
         (if (< move-counter 7)
             (progn
               ;; (preseed-checkerboard player-board 3)
               (apply-strategies player-board
                                 inference
                                 exploration
                                 norepeat))
             (apply-strategies player-board
                               inference
                               exploration
                               recognition
                               norepeat))
         (flatten-weights player-board)
         (destructuring-bind (r c) (choose-move player-board)
           (setf (aref (board-state player-board) r c)
                 (if (eq (aref (board-state target-board) r c) 'S)
                     'X
                     '-)))
         (incf move-counter))
    (list target-board player-board move-counter)))


;;; Statistics and analysis

(defun mean (s)
  (/ (reduce #'+ s) (length s)))

(defun median (s)
  (elt (sort (copy-list s) #'>) (floor (/ (length s) 2))))

(defun play-many (n)
  (let ((results (loop for i from 1 to n
                    collecting (caddr (play)))))
    (format t "Mean: ~A~%Median: ~A~%"
            (float (mean results))
            (float (median results)))
    (sort results #'<)))
