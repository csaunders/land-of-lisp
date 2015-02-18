(load "dice-of-doom")
(load "lazy-evaluation")

(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree
                         (add-new-dice
                           board
                           player
                           (1- spare-dice))
                         (mod (1+ player) *num-players*)
                         0
                         t))
                  moves)))

(defun attacking-moves (board cur-player sparce-dice)
  (labels ((player (pos)
            (car (aref board pos)))
           (dice (pos)
            (cadr (aref board pos))))
    (lazy-mapcan
      (lambda (src)
        (if (eq (player src) cur-player)
            (lazy-mapcan
              (lambda (dst)
                (if (and (not (eq (player dst)
                                  cur-player))
                         (> (dice src) (dice dst)))
                    (make-lazy
                      (list (list (list src dst)
                                  (game-tree (board-attack board
                                                           cur-player
                                                           src
                                                           dst
                                                           (dice src))
                                              cur-player
                                              (+ sparce-dice (dice dst))
                                              nil))))
                    (lazy-nil)))
              (make-lazy (neighbors src)))
            (lazy-nil)))
      (make-lazy (loop for n below *board-hexnum*
                       collect n)))))

(defun limit-tree-depth (tree depth)
  (list
    (car tree)
    (cadr tree)
    (if (zerop depth)
      (lazy-nil)
      (lazy-mapcar
        (lambda (move)
          (list (car move)
            (limit-tree-depth (cadr move) (1- depth))))
        (caddr tree)))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
                (unless (lazy-null moves)
                  (let* ((move (lazy-car moves))
                         (action (car move)))
                    (fresh-line)
                    (format t "~a. " n)
                    (if action
                      (format t "~a -> ~a" (car action) (cadr action))
                      (princ "end turn")))
                    (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings
                    (limit-tree-depth tree *ai-level*)
                    (car tree))))
  (cadr (lazy-nth (position (apply #'max ratings) ratings)
                  (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond
    ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
    ((zerop (car tree)) (play-vs-computer (handle-human tree)))
    (t (play-vs-computer (handle-computer tree)))))

