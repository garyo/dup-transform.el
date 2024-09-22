;;; dup-transform.el --- RGB/XY graphics code helpers  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Gary Oberbrunner

;; Author: Gary Oberbrunner <garyo@darkstarsystems.com>
;; Maintainer: Gary Oberbrunner <garyo@darkstarsystems.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/garyo/dup-transform.el
;; Keywords: graphics, tools, convenience, c++, 3d, video, rgb

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

;; A package for graphics coders, to duplicate a line or region,
;; handling rgb(a), xyz(w), [0]-[2] etc.
;; To use, place point on a line to be duplicated & transformed.
;; That line should have a pattern
;; like the following:
;; * .x (will be transformed to .y .z .w)
;; * ->x (similar)
;; * .r (will be transformed to .g .b .a)
;; * ->r (similar)
;; * red (to green blue alpha)
;; * [0] (to [1] [2] [3])
;; Use C-u (repeat count) to set total number of repeats.
;; Defaults to 3 if the line/region contains rgb-type text,
;; otherwise 2.

;;; Code:

(require 'seq)

(defgroup dup-transform nil
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

(defcustom dup-transform-cycle-keybinding "C-c <up>"
  "Key to bind `dup-transform-cycle-word' to."
  :type 'key
  :group 'dup-transform
  :set (lambda (symbol value)
         ;; Unset the previous keybinding, if set
         (when (boundp symbol)
           (let ((old-key (symbol-value symbol)))
             (when old-key
               (keymap-global-unset old-key))))
         (set-default symbol value)
         (keymap-global-set value #'dup-transform-cycle-word))
  :initialize 'custom-initialize-default
  :require 'dup-transform)

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
         (text-ends-with-newline (string-suffix-p "\n" text)))
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
        (setq end (point))))
    ;; move to start of next line
    (unless text-ends-with-newline
      (forward-char 1))))

;;; Test code; try on these lines/regions
;; (set foo.x a[0]) ;; xy only
;; (set foo->r a[0]) ;; set red
;; (unless foo
;;   (set foo.r a[0]) ;; red, from array.x
;;   )

(defun dup-transform--get-word-at-point ()
  "Get the word at point, along with its bounds."
  (let ((word-bounds (bounds-of-thing-at-point 'word)))
    (when word-bounds
      (list (buffer-substring-no-properties (car word-bounds) (cdr word-bounds))
            (car word-bounds)
            (cdr word-bounds)))))

(defun dup-transform--cycle-word (word-data n)
  "Cycle the word given by WORD-DATA by N elements."
  (let* ((word (car word-data))
         (word-start (cadr word-data))
         (word-end (nth 2 word-data))
         (string-lists '(("r" "g" "b" "a")
                         ("red" "green" "blue" "alpha")
                         ("x" "y" "z"))))
    (catch 'found
      (dolist (strings string-lists)
        (let ((index (seq-position strings word #'equal)))
          (when index
            (let* ((next-index (mod (+ index n) (length strings)))
                   (next-word (nth next-index strings)))
              (save-excursion
                (delete-region word-start word-end)
                (insert next-word)))
            (throw 'found t))))
      nil)))

(defun dup-transform-cycle-word (n)
  "Cycle the word at point to its Nth next value.
Words should be r/g/b, red/green/blue, or x/y/z."
  (interactive "p")
  (let ((word-at-point (dup-transform--get-word-at-point)))
    (when word-at-point
      (dup-transform--cycle-word word-at-point n))))

(defun dup-transform-cycle-word-back (n)
  "Cycle the word at point N elements `backwards'; see `dup-transform-cycle-word'."
  (interactive "p")
  (dup-transform-cycle-word (- n)))

;;; Test code: try C-c <up> on these, after setting up the bindings
;; (set x green)
;; (set y green)
;; (set z blue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up auto-repeat for repeat-mode

(defvar dup-transform--repeat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'dup-transform-cycle-word)
    (define-key map (kbd "<down>") #'dup-transform-cycle-word-back)
    map)
  "Keymap for repeatable `dup-transform' commands.")

;; Make these commands repeatable
(put #'dup-transform-cycle-word 'repeat-map 'dup-transform--repeat-keymap)
(put #'dup-transform-cycle-word-back 'repeat-map 'dup-transform--repeat-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up minor mode

(defvar dup-transform-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd dup-transform-cycle-keybinding) #'dup-transform-cycle-word)
    map)
  "Keymap for `dup-transform-mode`.")

;;;###autoload
(define-minor-mode dup-transform-mode
  "A minor mode to cycle through word transformations."
  :lighter " RGB"
  :keymap dup-transform-mode-map
  :global nil)

;;;###autoload
(defun dup-transform-mode-enable ()
  "Enable `dup-transform-mode` in the current buffer."
  (dup-transform-mode 1))

;;;###autoload
(defun dup-transform-mode-disable ()
  "Disable `dup-transform-mode` in the current buffer."
  (dup-transform-mode -1))

;;;###autoload
(define-globalized-minor-mode global-dup-transform-mode
  dup-transform-mode dup-transform-mode-enable)


(provide 'dup-transform)
;;; dup-transform.el ends here
