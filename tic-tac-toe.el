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



(provide 'tic-tac-toe)
;;; tic-tac-toe.el ends here
