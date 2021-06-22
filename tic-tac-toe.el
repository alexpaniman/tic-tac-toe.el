;;; tic-tac-toe.el --- Tic-Tac-Toe implementation in emacs-lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Alexander Paniman

;; Author: Alexander Paniman <https://github.com/alexpaniman>
;; Keywords: tic-tac-toe, games

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; RULES:
;;
;; Tic-Tac-Toe is a game played between two players on a rectangular 3x3 board. Each
;; player, in turn, marks a free square of its choice. The winner is the first
;; one to mark three contiguous squares in any direction (horizontally,
;; vertically or diagonally).

;; HOW TO USE:
;;
;; The command "M-x tic-tac-toe" displays a board.

;; ALGORITHM:
;;
;; It solves tic-tac-toe with minimax algorithm.

;;; Code:

(define-derived-mode tic-tac-toe--mode special-mode
  "tic-tac-toe"
  "Major mode for tic-tac-toe games.")

(define-key tic-tac-toe--mode-map (kbd "C-c C-c")
  'tic-tac-toe--human-make-move)


(defun tic-tac-toe ()
  "Start a Gomoku game between you and Emacs."

  (interactive)

  (switch-to-buffer "*tic-tac-toe*")
  (tic-tac-toe--init)
  (tic-tac-toe--mode))


(defvar tic-tac-toe--side-length 3)

(defvar tic-tac-toe--board (make-vector 9 ?\*))

(defvar tic-tac-toe--current-player ?\X)


(defun tic-tac-toe--init ()
  "Initiate a new tic-tac-toe game."

  (setq tic-tac-toe--side-length 3
        tic-tac-toe--board (make-vector 9 ?\*)
        tic-tac-toe--current-player ?\X)

  (tic-tac-toe--draw-board tic-tac-toe--board)

  (if (y-or-n-p "Can I play first? ")
      (tic-tac-toe--ai-make-move)))


(defun tic-tac-toe--get-cell-index (row column)
  "Return index of cell that's in ROW and COLUMN."

  (+ (* tic-tac-toe--side-length row) column))


(defun tic-tac-toe--get-cell (board row column)
  "Return cell that's on BOARD in ROW and COLUMN."

  (elt board (tic-tac-toe--get-cell-index row column)))


(defun tic-tac-toe--draw-row-skip ()
  "Draw BOARD's row skip."

  (dotimes (_ tic-tac-toe--side-length)
    (insert "+---"))
  (insert "+\n"))


(defun tic-tac-toe--draw-row (board row)
  "Draw an BOARD's ROW. A board has NUM cells."

  (dotimes (column tic-tac-toe--side-length)
    (insert "| " (tic-tac-toe--get-cell board row column) " "))
  (insert "|\n"))


(defun tic-tac-toe--draw-board (board)
  "Draw BOARD with NUM nuber of cells."

  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row tic-tac-toe--side-length)
      (tic-tac-toe--draw-row-skip)
      (tic-tac-toe--draw-row board row))
    (tic-tac-toe--draw-row-skip)))


(defun tic-tac-toe--check-cells (board &rest points)
  "Check if all the POINTS on a BOARD contain same player."

  (let ((i 0) (is-equal t) (last-cell))
    (while (< i (length points))
      (let ((cell (tic-tac-toe--get-cell
                   board (elt points i) (elt points (1+ i)))))
        (if (null last-cell)
            (setq last-cell cell))
        (setq is-equal
              (and is-equal (char-equal cell last-cell)))
        (setq last-cell cell)
        (setq i (+ i 2))))
    (if is-equal last-cell)))


(defun tic-tac-toe--check-win (board)
  "Return player's symbol if he won on BOARD and nil otherwise."

  (let ((winner
         (or (tic-tac-toe--check-cells board 0 0  0 1  0 2)
             (tic-tac-toe--check-cells board 1 0  1 1  1 2)
             (tic-tac-toe--check-cells board 2 0  2 1  2 2)

             (tic-tac-toe--check-cells board 0 0  1 0  2 0)
             (tic-tac-toe--check-cells board 0 1  1 1  2 1)
             (tic-tac-toe--check-cells board 0 2  1 2  2 2)

             (tic-tac-toe--check-cells board 0 0  1 1  2 2)
             (tic-tac-toe--check-cells board 0 2  1 1  2 0))))

  (unless (and winner (char-equal winner ?\*))
    winner)))


(defun tic-tac-toe--check-is-board-filled (board)
  "Check if all the cells in the BOARD has been filled."

  (let ((is-filled t))
    (dotimes (i (expt tic-tac-toe--side-length 2))
      (setq is-filled
            (and is-filled
                 (not (char-equal (elt board i) ?\*)))))
    is-filled))


(defun tic-tac-toe--get-status (board)
  "Return a game status for BOARD.
nil as retun value means that game is still in progress.
?x or ?o as return value means x or o has won.
'tie as return value means tie."

  (or (tic-tac-toe--check-win board)
      (if (tic-tac-toe--check-is-board-filled board) 'tie)))


(defun tic-tac-toe--next-player (player)
  "Return the next player to play after PLAYER."

  (if (char-equal player ?\X) ?\O ?\X))


(defun tic-tac-toe--solve (board player)
  "Solve the next move for PLAYER on BOARD.
Return list that contains the best score
and list possible steps to achive it."

  (let* ((total-score) (moves) (index)
         (number-of-cells (expt tic-tac-toe--side-length 2))
         (indices (make-vector number-of-cells 0)))

    (dotimes (i number-of-cells) (aset indices i i))

    (while (> (length indices) 0)
      (setq index (aref indices (random (length indices)))
            indices (delete index indices))

      (if (and (not (and total-score (= total-score 1)))
               (char-equal (elt board index) ?\*))

          (let ((new-board (copy-sequence board)))

            (aset new-board index player)

            (let* ((status (tic-tac-toe--get-status new-board))
                   (result (if status (if (eq status 'tie) +0 +1)
                             (- (car (tic-tac-toe--solve
                                      new-board
                                      (tic-tac-toe--next-player player)))))))

              (setq total-score result)

              (setq moves
                    (cond ((or (null total-score)
                               (> result total-score)) (list index))

                          ((or (= result total-score)) (cons index moves))))))))

    (list total-score moves)))


(defun tic-tac-toe--get-current-row ()
  "Get tic-tac-toe row number at current cursor position.
nil returned when cursor isn't in the row"

  (let ((index
         (let ((line (line-number-at-pos)))
           (if (/= (% line 2) 1)
               (1- (/ line 2))))))

    (if (and index (<= index tic-tac-toe--side-length))
        index)))


(defun tic-tac-toe--get-current-column ()
  "Get tic-tac-toe column number at current cursor position.
nil returned when cursor isn't in the column"

  (let ((index
         (let ((column (current-column)))
           (if (/= (% column 4) 0)
               (1- (ceiling (/ column 4.0)))))))

    (if (and index (<= index tic-tac-toe--side-length))
        index)))


(defun tic-tac-toe--notify-about-game-status ()
  "Notify player about current game status. Return t if game isn't finished."

  (let ((status (tic-tac-toe--get-status tic-tac-toe--board)))
    (if status
        (let ((inhibit-read-only t))
          (goto-char (point-max))

          (cond ((eq status 'tie) (insert "It's a tie!"))

                ((char-equal status ?\O) (insert "O has won!"))
                ((char-equal status ?\X) (insert "X has won!")))

          (if (y-or-n-p "Do you want another game? ")
              (tic-tac-toe--init)
            (kill-current-buffer)))
      t)))


(defun tic-tac-toe--swap-players ()
  "Swap tic-tac-toe players."

  (setq tic-tac-toe--current-player
        (tic-tac-toe--next-player tic-tac-toe--current-player)))


(defun tic-tac-toe--human-make-move ()
  "Mark current cell in tic-tac-toe according to the player."

  (interactive)

  (let ((row (tic-tac-toe--get-current-row))
        (column (tic-tac-toe--get-current-column)))

    (if (and row column)
        (if (char-equal (tic-tac-toe--get-cell tic-tac-toe--board
                                               row column) ?\*)
        (let ((point-in-buffer (point)))

          (aset tic-tac-toe--board
                (tic-tac-toe--get-cell-index row column)
                tic-tac-toe--current-player)

          (tic-tac-toe--draw-board tic-tac-toe--board)

          (goto-char point-in-buffer)

          (tic-tac-toe--swap-players)

          (if (tic-tac-toe--notify-about-game-status)
            (tic-tac-toe--ai-make-move)))

        (message "You cannot overwrite cells that has been already set!"))

      (message "Your cursor is not in any the cell!"))))




(defun tic-tac-toe--ai-make-move ()
  "Solve next game move with ai and make it."

  (interactive)

  (aset tic-tac-toe--board
        (let ((moves (elt (tic-tac-toe--solve tic-tac-toe--board tic-tac-toe--current-player) 1)))

          (elt moves (random (length moves))))
        tic-tac-toe--current-player)

  (tic-tac-toe--swap-players)
  (tic-tac-toe--draw-board tic-tac-toe--board)
  (tic-tac-toe--notify-about-game-status))

(provide 'tic-tac-toe)
;;; tic-tac-toe.el ends here
