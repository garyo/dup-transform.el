;;; dup-transform.el --- RGB/XY graphics code helpers  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Gary Oberbrunner

;; Author: Gary Oberbrunner <garyo@darkstarsystems.com>
;; Keywords: lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "27"))
;; Keywords: graphics, programming

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

;; A package for graphics coders, to duplicate a line or region, handling rgb(a), xyz(w), [0]-[2] etc.
;; To use, place point on a line to be duplicated & transformed. That line should have a pattern
;; like the following:
;; * .x (will be transformed to .y .z .w)
;; * ->x (similar)
;; * .r (will be transformed to .g .b .a)
;; * ->r (similar)
;; * red (to green blue alpha)
;; * [0] (to [1] [2] [3])
;; Use C-u (repeat count) to set total number of repeats. Defaults to 3 if the line/region contains rgb-type text,
;; otherwise 2.

;;; Code:

(defgroup dup-tranform nil
  "Customizations for 'dup-transform'."
  :group 'applications
  :prefix "dup-transform-")

(defcustom dup-transform-rgb-default-n 3
  "Default for `dup-transform' when the text looks like RGB."
  :type 'integer
  :group 'dup-transform)

(defcustom dup-transform-xy-default-n 2
  "Default for `dup-transform' when the text doesn't look like RGB (e.g. xy(z)(w))."
  :type 'integer
  :group 'dup-transform)

(defun dup-transform (n)
  "Duplicate the current region or line N times, replacing rgb/xyz instances.
Will replace `.r' with `.g', then `.b', `.a'. Same for red/green/blue/alpha.
Also replaces `[0]' with `[1]', `[2]', `[3]'.
Prefix arg sets number of repeats; default is 3 if the line looks like rgb,
else 2 (assuming xy).
Point should be on a `red' line to be repeated."
  (interactive "P") ;; prefix arg sets total number of lines (3 for rgb, 4 for rgba)
  (let* ((use-region (use-region-p)) ;; Check if the region is active
         (start (if use-region (region-beginning) (line-beginning-position)))
         (end (if use-region (region-end) (line-end-position)))
         (text (buffer-substring-no-properties start end)) ;; Get the text of the region or line
         (has-rgb (or (string-search ".r" text)
                      (string-search "->r" text)
                      (string-search "red" text)))
         (default-n (if has-rgb dup-transform-rgb-default-n dup-transform-xy-default-n))
         (n (or n default-n))
         (n (prefix-numeric-value n))
         (n (- (min n 4) 1)) ; clamp to 4x max and subtract 1 for the original line
         (xyz-patterns1 '(".x" ".y" ".z" ".w"))
         (xyz-patterns2 '("->x" "->y" "->z" "->w"))
         (color-patterns1 '(".r" ".g" ".b" ".a"))
         (color-patterns2 '("->r" "->g" "->b" "->a"))
         (color-name-patterns '("red" "green" "blue" "alpha"))
         (text-ends-with-newline (string-suffix-p "\n" text))
         )
    (dotimes (i n)
      (let ((modified-text text))
        ;; Replace patterns
        (dolist (patterns
                 (list xyz-patterns1 xyz-patterns2 color-patterns1 color-patterns2 color-name-patterns))
          (setq modified-text (replace-regexp-in-string (regexp-quote (nth 0 patterns))
                                                        (nth (+ i 1) patterns)
                                                        modified-text)))
        ;; Replace index pattern
        (setq modified-text (replace-regexp-in-string (regexp-quote "[0]")
                                                      (format "[%d]" (+ i 1))
                                                      modified-text))
        ;; Insert the modified text
        (goto-char end)
        (if text-ends-with-newline
            (insert modified-text)
          (insert "\n" modified-text))
        (setq end (point))
        ))
    ;; move to start of next line
    (unless text-ends-with-newline
      (forward-char 1))
    ))

;;; Test code; try on these lines/regions
;; (set foo.x a[0]) ;; xy only
;; (set foo->r a[0]) ;; set red
;; (unless foo
;;   (set foo.r a[0]) ;; red, from array.x
;;   )

(provide 'dup-transform)
;;; dup-transform.el ends here
