;;; numbex.el --- Manage numbered examples -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/numbex
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; numbex.el provides a minor mode that manages numbered examples.
;; Its primary function is to take care for the user of the correct
;; sequential numbering of example in the buffer.  It also takes care
;; that references to examples are updated with the change in the
;; numbering.  The way it does so is by defining a class of
;; expressions, numbex items, which are buffer substrings that match
;; the following regexp:
;;
;;     "{\\[[pnr]?ex:\\([^\]]*\\)\\]}"  -- regexp for numbex items
;;
;; That is, the following expressions are all well formed numbex
;; items:
;;
;; - {[ex:]}
;; - {[ex:label]}
;; - {[rex:]}
;; - {[nex:label]}
;; - {[pex:label]}
;;
;; The text between ":" and "]" (that is, capture group 1 in the
;; regexp above) is the LABEL of the item.  Thust, every well formed
;; numbex item is made up by a PREFIX ("ex:", "rex:", "nex:" or
;; "pex:") and a LABEL surrounded by square brackets and curly braces.
;;
;; A LABEL can be any string that does not contain backslashes, square
;; brackets or curly braces.  Numbex converts all blank labels
;; (strings that only contains white spaces) to the empty label.  The
;; PREFIX determines the type of item.  The fundamental ones are
;; "ex:", the EXAMPLE item, and "rex:" the REFERENCE item.
;;
;; For explanation as to how to use numbex, check the readme.org file
;; at <https://github.com/enricoflor/numbex>.

;;; Code:

(require 'cl-lib)
(require 'cl-seq)
(require 'subr-x)

(defgroup numbex nil
  "Automatically number examples and references to them."
  :prefix "numbex-"
  :link '(url-link :tag "Website for numbex"
                   "https://github.com/enricoflor/numbex")
  :group 'convenience)

(defvar-local numbex--total-number-of-items 0
  "Total number of numbex-items in the buffer.")

(defvar-local numbex--hidden-labels t
  "If t, numbex items have a numerical overlay.")

(defvar-local numbex--key-number-pairs nil
  "Alist mapping labels of examples to the number assigned.")

(defvar-local numbex--example-lines nil
  "Alist mapping a label to the line of the corresponding example.")

(defvar-local numbex--duplicates nil
  "A list of non-empty labels that are not unique in the buffer.")

(defcustom numbex-delimiters '("(" . ")")
  "Opening and closing characters used around numbers.
Set two empty strings if you just want the number."
  :type 'cons
  :group 'numbex)

(defcustom numbex-highlight-unlabeled t
  "If t, items that are missing a label are highlighted.
Blank strings (containing only white space) count as no label.
If 'font-lock-mode' is enabled, unlabeled items have the appearance
specified by 'font-lock-comment-face', otherwise, they are just
gray."
  :type 'boolean
  :group 'numbex)

(defcustom numbex-highlight-duplicates t
  "If t, items that have an ambiguous label are highlighted.
A label is ambiguous if it not a blank string (it doesn't just
contain white space) and is used as a label of more than one
example item.  If 'font-lock-mode' is enabled, unlabeled items
have the appearance specified by 'font-lock-warning-face',
otherwise, they are just red."
  :type 'boolean
  :group 'numbex)

(defvar numbex--idle-timer nil)

(defvar numbex--item-re "{\\[[pnr]?ex:\\([^\]]*\\)\\]}")
(defvar numbex--ex-re "{\\[ex:\\([^\]]*\\)\\]}")
(defvar numbex--rex-re "{\\[rex:\\([^\]]*\\)\\]}")
(defvar numbex--r-ex-re "{\\[r?ex:\\([^\]]*\\)\\]}")
(defvar numbex--pnrex-re "{\\[[pnr]+ex:\\([^\]]*\\)\\]}")
(defvar numbex--pnex-re "{\\[[pn]+ex:\\([^\]]*\\)\\]}")

(defun numbex--item-at-point ()
  "Return the position of the label of the item point is on.
If point is on a numbex item, this function returns a cons cell
whose car and cdr are the buffer positions of the first and last
character of the label respectively.  Return nil if point is not
on a numbex item."
  (let ((position (point)))
    (save-excursion
      (beginning-of-line)
      (catch 'found
        (while (re-search-forward numbex--item-re (line-end-position) t)
          (when (<= (match-beginning 0) position (match-end 0))
            (throw 'found
                   (cons (match-beginning 1)
                         (match-end 1))))
          nil)))))

(defun numbex--get-duplicates (list)
  "Return a list of strings that occur more than once in LIST."
  (let ((no-dupl
         (cl-remove-duplicates list
                               :test #'equal)))
    (cl-remove-duplicates (cl-set-difference list no-dupl)
                          :test #'equal)))

(defmacro numbex--with-widened-indirect-buffer (&rest body)
  "Execute BODY in a widened indirect buffer of the current one.
Kill the indirect buffer when done with BODY."
  `(with-current-buffer (clone-indirect-buffer nil nil t)
     (widen)
     (unwind-protect
         ,body
       (kill-buffer (current-buffer)))))

(defun numbex--scan-buffer ()
  "Collect information relevant for numbex from the buffer.
Remove all whitespace from items.  Reset the values for the
buffer-local variables.  The information is collected in a
widened indirect buffer, so that even if the current buffer is
narrowed, numbex will behave taking into consideration the entire
buffer.  This reduces the risk of working on a narrowed buffer
and ending up with many duplicate labels or mistaken references
once the buffer is widened again."
  (let ((labels '())
        (labels-lines '())
        (labels-positions '())
        (number 1))
    (numbex--with-widened-indirect-buffer
     (lambda ()
       (goto-char (point-min))
       (while (re-search-forward numbex--ex-re nil t)
        (let ((lab
                (buffer-substring-no-properties (match-beginning 1)
                                                (match-end 1))))
           (push lab labels)
           (push (cons lab
                       (concat (car numbex-delimiters)
                               (number-to-string number)
                               (cdr numbex-delimiters)))
                 labels-positions)
           (push (cons lab
                       (buffer-substring-no-properties
                        (match-end 0)
                        (line-end-position)))
                 labels-lines))
         (setq number (1+ number)))))
    (setq numbex--key-number-pairs (nreverse labels-positions))
    (setq numbex--example-lines (nreverse labels-lines))
    (setq numbex--duplicates (numbex--get-duplicates labels))))

(defun numbex--remove-numbering ()
  "Remove all numbex text properties from the buffer.
Set 'numbex--hidden-labels' to nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward numbex--item-re nil t)
      (set-text-properties (match-beginning 0)
                           (match-end 0)
                           nil)))
  (setq numbex--hidden-labels nil))

(defun numbex--number-closest-example (&optional next)
  "Return the number assigned to the first example before point.
If NEXT is non-nil, return the number assigned to the first
example after point."
  (let* ((m (if next 1 0))
         (n
          (+ m
             (how-many numbex--ex-re
                       (point-min) (point)))))
  (if (= n 0)
      (concat (car numbex-delimiters)
              "0"
              (cdr numbex-delimiters))
    (concat (car numbex-delimiters)
            (number-to-string n)
            (cdr numbex-delimiters)))))

(defun numbex--label-closest-example (&optional next)
  "Return pair of label and number of closest examples.
By default, check the first example before point; if NEXT is
non-nil, check the first example after point."
  (let ((number (numbex--number-closest-example next))
        (direction (if next 1 -1)))
    (save-excursion
      (if (re-search-forward numbex--ex-re nil t direction)
          (cons (buffer-substring-no-properties (match-beginning 1)
                                                (match-end 1))
                number)
        (cons "" number)))))

(defun numbex--highlight (label b e)
  "Highlight items without labels or with non-unique ones.
LABEL is the label of the item, B and E the position of its first
and last character in the buffer."
  (when (and numbex-highlight-duplicates
             (member label numbex--duplicates))
    (if font-lock-mode
        (add-text-properties b e
                             '(font-lock-face 'font-lock-warning-face))
      (add-text-properties b e
                           '(face (:foreground "red"))))
    (put-text-property b e 'rear-nonsticky t))
  (when (and numbex-highlight-unlabeled
             (string-blank-p label))
    (if font-lock-mode
        (add-text-properties b e
                             '(font-lock-face 'font-lock-comment-face))
      (add-text-properties b e
                           '(face (:foreground "gray"))))
    (put-text-property b e 'rear-nonsticky t)))

(defun numbex--add-numbering ()
  "Number items in the buffer as text properties.
Set 'numbex-hidden-labels' to t."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward numbex--r-ex-re nil t)
      (let ((label (match-string 1))
            (item (match-string 0))
            (b (match-beginning 0))
            (e (match-end 0)))
        (set-text-properties b e nil)
        (cond ((string-prefix-p "{[e" item)
               (put-text-property b e
                                  'display
                                  (cdr (assoc label
                                              numbex--key-number-pairs))))
              ((string-prefix-p "{[r" item)
               (when (or (numbex--label-exists-p label)
                         (string-blank-p label))
                 (let ((number
                        (if (string-blank-p label)
                            (numbex--number-closest-example)
                          (cdr (assoc label
                                      numbex--key-number-pairs)))))
                   (put-text-property b e
                                      'display number)))))
        (numbex--highlight label b e)))
    (goto-char (point-min))
    (while (re-search-forward numbex--pnex-re nil t)
      (let* ((b (match-beginning 0))
             (old-item (match-string 0))
             (next (string-prefix-p "{[n" old-item))
             (pre (if next "{[nex:"  "{[pex:")))
        (replace-match "")
        (let* ((closest (numbex--label-closest-example next))
               (new-label (car closest))
               (new-number (cdr closest)))
          (goto-char b)
          (insert (concat pre new-label "]}"))
          (re-search-backward numbex--pnex-re nil t)
          (put-text-property (match-beginning 0)
                             (match-end 0)
                             'display new-number)
           (numbex--highlight new-label
                             (match-beginning 0)
                             (match-end 0))
           (goto-char (match-end 0)))))
    (goto-char (point-min))
    (setq numbex--total-number-of-items 0)
    (while (re-search-forward numbex--item-re nil t)
      (setq numbex--total-number-of-items
            (1+ numbex--total-number-of-items))
      (when (or (numbex--label-exists-p (match-string 1))
                (string-blank-p (match-string 1)))
        (add-text-properties (match-beginning 0)
                             (match-end 0)
                             '(invisible t))))
    (setq numbex--hidden-labels t)))

;;;###autoload
(defun numbex-toggle-display ()
  "Remove numbers if they are present, add them otherwise."
  (interactive)
  (numbex--refresh)
  (if numbex--hidden-labels
      (numbex--remove-numbering)
    (numbex--add-numbering)))

(defun numbex--label-exists-p (s)
  "Return t if S is already being used as a label in the buffer."
  (member s
          (cl-remove-if #'string-blank-p
                        (mapcar #'car
                                numbex--key-number-pairs))))

(defun numbex--edit (old-label-pos)
  "Edit label starting at OLD-LABEL-POS.  Insert the new label."
  (save-excursion
    (let* ((old-label
            (buffer-substring-no-properties (car old-label-pos)
                                            (cdr old-label-pos)))
           (new-label
            (read-string (format
                          "New label [default \"%s\"]: "
                          old-label)
                         old-label nil
                         old-label t)))
      (if (and (numbex--label-exists-p new-label)
               (not (equal new-label old-label)))
          (if (not (yes-or-no-p (concat new-label
                                        " is already a label, are you sure?")))
              (numbex--edit old-label-pos)
            (goto-char (car old-label-pos))
            (delete-region (car old-label-pos)
                           (cdr old-label-pos))
            (insert new-label))
        (goto-char (car old-label-pos))
        (delete-region (car old-label-pos)
                       (cdr old-label-pos))
        (insert new-label))
      ;; If item at point is nex: or pex:, make it into a rex:,
      ;; otherwise the new label will be wiped out automatically.  It
      ;; does not make sense to edit pex: and nex: items manually.
      (when (re-search-backward "{\\[[pn]+ex" (- (car old-label-pos) 6) t)
        (replace-match "{[rex" t)))))

(defun numbex--example ()
  "Insert a new example item."
  (let ((label
         (read-string "Label: "
                      nil nil
                      nil t))
        (existing
         (cl-remove-if #'string-blank-p
                       (mapcar #'car
                               numbex--key-number-pairs))))
    (if (member label existing)
        (if (yes-or-no-p (format
                          "\"%s\" is already a label, are you sure?"
                          label))
            (insert (concat "{[ex:" label "]}"))
          (numbex--example))
      (insert (concat "{[ex:" label "]}")))))

(defun numbex--reference ()
  "Insert a new reference item."
  (insert
   (concat "{[rex:"
           (car (list
                 (completing-read
                  "Label [default empty]: "
                  (cl-remove-if #'string-blank-p
                                (mapcar #'car
                                        numbex--key-number-pairs))
                  nil nil
                  nil nil
                  "" t)))
           "]}")))

(defun numbex--new ()
  "Insert a new item."
  (let ((choice (read-multiple-choice "Insert new:"
                                      '((?e "example")
                                        (?r "reference")
                                        (?n "ref to next")
                                        (?p "ref to previous")))))
    (cond ((equal choice '(?e "example"))
           (numbex--example))
          ((equal choice '(?r "reference"))
           (numbex--reference))
          ((equal choice '(?n "ref to next"))
           (insert "{[nex:]}"))
          ((equal choice '(?p "ref to previous"))
           (insert "{[pex:]}")))
    (insert " ")))

;;;###autoload
(defun numbex-do ()
  "Insert a new item or edit the existing one at point."
  (interactive)
  (let ((hidden numbex--hidden-labels))
    (when hidden
      (numbex--remove-numbering))
    (if (numbex--item-at-point)
        (numbex--edit (numbex--item-at-point))
      (numbex--new))
    (numbex--refresh t)
    (if hidden
        (numbex--add-numbering)
      (numbex--remove-numbering))))

(defun numbex--search-not-at-point ()
  "Let user choose a regexp and grep for it."
  (let ((choice (read-multiple-choice "Look for:"
                                      '((?a "all items")
                                        (?e "examples")
                                        (?r "references")))))
    (cond ((equal choice '(?a "all items"))
           (occur numbex--item-re))
          ((equal choice '(?e "examples"))
           (occur numbex--ex-re))
          ((equal choice '(?r "references"))
           (occur numbex--pnrex-re)))))

(defun numbex--search-at-point ()
  "Let user choose a regexp and grep for it."
  (let* ((positions (numbex--item-at-point))
         (label (buffer-substring-no-properties (car positions)
                                                (cdr positions)))
         (reg (if (string-blank-p label)
                  numbex--item-re
                (concat "{\\[[pnr]?ex:" label "\\]}")))
         (choice (read-multiple-choice "Look for:"
                                       '((?a "all items")
                                         (?e "examples")
                                         (?r "references")
                                         (?l "label at point")))))
    (cond ((equal choice '(?a "all items"))
           (occur numbex--item-re))
          ((equal choice '(?e "examples"))
           (occur numbex--ex-re))
          ((equal choice '(?r "references"))
           (occur numbex--pnrex-re))
          ((equal choice '(?l "label at point"))
           (occur reg)))))

;;;###autoload
(defun numbex-search ()
  "Grep the buffer for items."
  (interactive)
  (numbex--remove-numbering)
  (numbex--refresh t)
  (if (numbex--item-at-point)
      (numbex--search-at-point)
    (numbex--search-not-at-point))
  (numbex--add-numbering))

;;;###autoload
(defun numbex-convert-to-latex ()
  "Replace all numbex items into corresponding LaTeX macros."
  (interactive)
  (numbex--scan-buffer)
  (numbex--add-numbering)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward numbex--ex-re nil t)
      (let ((label (match-string-no-properties 1)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "\\label{ex:" label "}"))))
    (goto-char (point-min))
    (while (re-search-forward numbex--pnrex-re nil t)
      (let ((label (match-string-no-properties 1)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "(\\ref{ex:" label "})"))))))

;;;###autoload
(defun numbex-convert-from-latex ()
  "Replace relevant LaTeX macros with corresponding numbex items."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\label{ex:\\([^\\]*\\)}" nil t)
      (let ((label (match-string-no-properties 1)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "{[ex:" label "]}"))))
    (goto-char (point-min))
    (while (re-search-forward "(\\\\ref{ex:\\([^\\]*\\)})" nil t)
      (let ((label (match-string-no-properties 1)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "{[rex:" label "]}")))))
  (numbex--refresh))

(defun numbex--get-number (p)
  "Return the number assigned to item at position P."
  (let* ((raw-properties (format "%s"
                                 (text-properties-at p)))
         (number (with-temp-buffer
                   (insert raw-properties)
                   (re-search-backward "display \\(([[:digit:]]+)\\)")
                   (match-string 1))))
    number))

;;;###autoload
(defun numbex-write-out-numbers ()
  "Write buffer to new file with numbers instead of numbex items."
  (interactive)
  (numbex--scan-buffer)
  (numbex--add-numbering)
  (let* ((original-buffer (current-buffer))
         (new-buffer-name (concat "nb-"
                                  (format "%s" original-buffer)))
         (unique-new-name (if (file-exists-p new-buffer-name)
                              (generate-new-buffer-name new-buffer-name)
                            new-buffer-name)))
    (with-current-buffer (get-buffer-create unique-new-name)
      (insert-buffer-substring-as-yank original-buffer)
      (goto-char (point-min))
      (while (re-search-forward numbex--item-re nil t)
        (let* ((b (match-beginning 0))
               (e (match-end 0))
               (number (numbex--get-number b)))
          (goto-char b)
          (delete-region b e)
          (insert number)))
      (write-file unique-new-name)
      (kill-current-buffer))))

(defun numbex--echo-duplicates ()
  "Display existing non-unique labels in the echo area."
  (when numbex--duplicates
    (if (= (length numbex--duplicates) 1)
        (message (concat ""
                         "1 duplicate label found: "
                         (car numbex--duplicates)))
      (message (concat (format  "%s duplicate labels found: "
                                (length numbex--duplicates))
                       (mapconcat #'identity
                                  numbex--duplicates
                                  "  "))))))

(defun numbex--refresh (&optional no-echo)
  "Scan the buffer and assign numbers.
If NO-ECHO is non-nil, do not warn about duplicates.  This is to
be added to 'numbex-mode-hook', 'auto-save-hook' and
'before-save-hook'."
  (let ((hidden numbex--hidden-labels))
    ;; First of all, remove whitespace from items.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward numbex--item-re nil t)
        (replace-match
         (replace-regexp-in-string "\s"
                                   ""
                                   (match-string-no-properties 0))
         t)))
    (numbex--scan-buffer)
    (numbex--add-numbering)
    (unless hidden
      (numbex--remove-numbering))
    (unless no-echo
      (numbex--echo-duplicates))))

(defun numbex--timed ()
  "Housekeeping function to be evaluated on numbex--idle-timer.
If not in 'numbex-mode' or 'numbex--hidden-labels' is nil, do
nothing.  Otherwise, if point is on an item, display its label
and, if appropriate, the context of the referenced example in the
echo area.  If point is not on an item, evaluate
'numbex--scan-buffer' and 'numbex--add-numbering' so that the
appearance of the buffer is kept up to date as far as numbex is
concerned."
  (when (and numbex-mode
             numbex--hidden-labels)
    (let ((positions (numbex--item-at-point)))
      (if positions
          ;; Point is on an item: show the underlying label.  If the
          ;; item is a reference, show the context of the
          ;; corresponding example as well.
          (let* ((label
                  (buffer-substring-no-properties (car positions)
                                                  (cdr positions)))
                 (rex (not
                       (equal
                        (buffer-substring-no-properties (- (car positions)
                                                           5)
                                                        (car positions))
                        "{[ex:"))))
            (if rex
                (message (concat label
                                 ": "
                                 (cdr (assoc label
                                             numbex--example-lines))))
              (message label)))
        ;; Point is not on an item, just rescan the buffer and
        ;; renumber the items.  Do it only if this wouldn't cripple
        ;; everything.
        (unless (> numbex--total-number-of-items 400)
          (numbex--scan-buffer)
          (numbex--add-numbering))))))

(defun numbex--hook-functions ()
  "Initialize numbex-mode.  To be added to 'numbex-mode-hook'.
Evaluates 'numbex--refresh' and adds the same function to
'auto-save-hook' and 'before-save-hook'."
  (numbex--refresh)
  (add-hook 'auto-save-hook
            #'numbex--refresh
            nil t)
  (add-hook 'before-save-hook
            #'numbex--refresh
            nil t))

(add-hook 'numbex-mode-hook #'numbex--hook-functions)

;;;###autoload
(define-minor-mode numbex-mode
  "Automatically number examples and references to them."
  :init-value nil
  :global nil
  :lighter " nbex"
  :group 'convenience
  (if numbex-mode
      (unless numbex--idle-timer
        (setq numbex--idle-timer
              (run-with-idle-timer 0.5 t #'numbex--timed)))
    (cancel-timer numbex--idle-timer)
    (setq numbex--idle-timer nil)))

(provide 'numbex)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; numbex.el ends here
