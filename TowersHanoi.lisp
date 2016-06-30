(defun can-move-pegs (p1 p2)
    (if (null p1) (not 1) (or (null p2) (< (car p1) (car p2))))
)

(defun get-successors (p1 p2 p3)
    (setq states ())
    (if (can-move-pegs p1 p2) (push (list (cdr p1) (cons (car p1) p2) p3) states) nil)
    (if (can-move-pegs p1 p3) (push (list (cdr p1) p2 (cons (car p1) p3)) states) nil)
    (if (can-move-pegs p2 p3) (push (list p1 (cdr p2) (cons (car p2) p3)) states) nil)
    (if (can-move-pegs p2 p1) (push (list (cons (car p2) p1) (cdr p2) p3) states) nil)
    (if (can-move-pegs p3 p1) (push (list (cons (car p3) p1) p2 (cdr p3)) states) nil)
    (if (can-move-pegs p3 p2) (push (list p1 (cons (car p3) p2) (cdr p3)) states) nil)
    states
)

(defun goal-state-met (pegs goal-state)
    ;(format t "~A = ~A? ~%" pegs goal-state)
    (equal goal-state pegs)
)

(defun generate-ordered-stack (n)
    (setq discs ())
    (do ((i n (- i 1)))
        ((= i 0))
        (push i discs))
    discs
)

(defstruct problem
  initial-state
  goal-state
  trans-function
)

(defstruct node
  state
  parent
)

(defun solution (goal-node explored)
  (setq trace ())
  (setf node goal-node)
  (push (node-state node) trace)
  (loop
    (if (null (node-parent node)) (return trace))
    (setf parent (gethash (node-state (node-parent node)) explored))
    (setf node parent)
    (push (node-state node) trace)
  )
)

(defun print-solution (trace)
  (setf num-moves (- (length trace) 1))
  (loop
    (setf node-state (pop trace))
    (if (null node-state) (return (format nil "! NUMBER OF MOVES: ~A" num-moves)))
    (print node-state)
  )
)

(defun make-queue ()
  ()
)

(defun enqueue (node q)
  (push node q)
  q
)

(defun dequeue (q)
  (setf rq (reverse q))
  (setf first (pop rq))
  (setf q (reverse rq))
)

(defun compare-nodes (n1 n2)
  (equal (node-state n1) (node-state n2))
)

(defun member-in-frontier (frontier node)
  (setf member nil)
  (dolist (n frontier)
    (if (compare-nodes n node) (setf member n) nil)
  )
  (not (null member))
)

(defun bfs (p)
  ;create node from initial state
  (setf node (make-node :state (problem-initial-state p)))

  ;create frontier
  (setf frontier (make-queue))

  ;create explored state
  (setf explored (make-hash-table :test 'equal))

  (block search
    ;check node for goal state
    (if (goal-state-met (node-state node) (problem-goal-state p))
      (return-from search (node-state node))
    )

    ;add node to frontier
    (setf frontier (enqueue node frontier))

    ;start loop
    (loop
      ;if frontier empty, return NIL
      (if (= (length frontier) 0) (return-from search ()))

      ;choose shallowest node in frontier
      (setf node (car (last frontier)))

      (setf frontier (dequeue frontier))

      ;add node to explored
      (setf (gethash (node-state node) explored) node)

      ;get children of node
      (setq children (apply (problem-trans-function p) (node-state node)))

      (setf num-children (length children))

      (do
        ((i 1 (+ i 1)))
        ((> i num-children))

        ;create child node
        (setq child-state (pop children))
        (setf child-node (make-node :state child-state :parent node))

        ;is child already in explored?
        (setf child-explored (gethash child-state explored))

        ;is child already in frontier
        (setf in-frontier (member-in-frontier frontier child-node))

        ;if not in explored or frontier
        (if (and (null child-explored) (not in-frontier))
            (if (goal-state-met child-state (problem-goal-state p)) 
              (return-from search (solution child-node explored)) 
              (setf frontier (enqueue child-node frontier))
            ) ;if goal-state met
        ) ;if not in explored
      ) ; loop through children
    ) ; infinite search loop 
  ) ; search block
)

(defun solve (n)
  ; create definition of perfectly ordered peg
  (setq ordered-stack (generate-ordered-stack n))

  ; create initial state
  (setq initial-state (list ordered-stack () ()))

  ; create goal state
  (setq goal-state (list () () ordered-stack))

  ; create problem with initial state, goal state, successor function
  (setf hanoi-problem (make-problem :initial-state initial-state :goal-state goal-state :trans-function #'GET-SUCCESSORS))

  ; call search algorithm
  (setf solution (bfs hanoi-problem))

  ; print solution
  (print-solution solution)

)

(defun print-author ()
  (setf author "William Jeffries")
  author
)

