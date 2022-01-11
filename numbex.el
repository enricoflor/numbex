;;; numbex.el --- Manage numbered examples -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/numbex
;; Version: 0.2.0
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

(require 'subr-x)

(defvar numbex-mode)

(defgroup numbex nil
  "Automatically number examples and references to them."
  :prefix "numbex-"
  :link '(url-link :tag "Website for numbex"
                   "https://github.com/enricoflor/numbex")
  :group 'convenience)

(defvar-local numbex--total-number-of-items 0
  "Total number of numbex-items in the buffer.")

(defvar-local numbex--automatic-refresh t
  "If t, evaluate 'numbex-refresh' on idle timer and with other commands.
Specifically, with 'numbex-toggle-display' and 'numbex-do'.  Even
if nil, 'numbex-refresh will be added to 'auto-save-hook' and
'before-save-hook'.")

(defvar-local numbex--hidden-labels t
  "If t, numbex items have a numerical overlay.")

(defcustom numbex-delimiters '("(" . ")")
  "Opening and closing characters used around numbers.
Set two empty strings if you just want the number."
  :type 'cons
  :group 'numbex)

(defcustom numbex-highlight-unlabeled t
  "If t, items that are missing a label are highlighted.
Blank strings (containing only white space) count as no label.
Unlabeled items have the appearance specified by
'font-lock-comment-face'."
  :type 'boolean
  :group 'numbex)

(defcustom numbex-highlight-duplicates t
  "If t, items that have an ambiguous label are highlighted.
A label is ambiguous if it not a blank string (it doesn't just
contain white space) and is used as a label of more than one
example item.  Items with non-unique labels have the appearance
specified by 'font-lock-warning-face'."
  :type 'boolean
  :group 'numbex)

(defvar numbex--idle-timer nil)

;; There is some redundancy in some of these regexp but it is done for
;; consistency: the first capture group is always the type, the second
;; is always the label of the item.
(defvar numbex--item-re "{\\[\\([pnr]?ex\\):\\([^\]]*\\)\\]}")
(defvar numbex--ex-re "{\\[\\(ex\\):\\([^\]]*\\)\\]}")
(defvar numbex--rex-re "{\\[\\(rex\\):\\([^\]]*\\)\\]}")
(defvar numbex--r-ex-re "{\\[\\(r?ex\\):\\([^\]]*\\)\\]}")
(defvar numbex--pnrex-re "{\\[\\([pnr]+ex\\):\\([^\]]*\\)\\]}")
(defvar numbex--pnex-re "{\\[\\([pn]+ex\\):\\([^\]]*\\)\\]}")

(defun numbex--item-at-point ()
  "Return position of the label of the item point is on and its type.
If point is on a numbex item, this function returns a cons cell
whose car is a cons cell with the buffer positions of the first
and last character of the label respectively, and its cdr is a
string corresponding to the type of the item.  Return nil if
point is not on a numbex item.

Thus, when point is on an item:
 - (caar (numbex--item-at-point)) is the beginning of the label
 - (cdar (numbex--item-at-point)) is the end of the label
 - (cdr (numbex--item-at-point)) is the type of the label"
  (let ((position (point)))
    (save-excursion
      (beginning-of-line)
      (catch 'found
        (while (re-search-forward numbex--item-re (line-end-position) t)
          (when (<= (match-beginning 0) position (match-end 0))
            (throw 'found
                   (cons (cons (match-beginning 2)
                               (match-end 2))
                         (buffer-substring-no-properties (match-beginning 1)
                                                         (match-end 1)))))
          nil)))))

(defmacro numbex--with-widened-indirect-buffer (&rest body)
  "Execute BODY in a widened indirect buffer of the current one.
Kill the indirect buffer when done with BODY."
  `(with-current-buffer (clone-indirect-buffer nil nil t)
     (widen)
     (goto-char (point-min))
     (unwind-protect
         ,body
       (kill-buffer (current-buffer)))))

;; This regexp matches either an example item of a form-feed character
(defvar numbex--ex-form-feed-re "{\\[\\(ex\\):\\([^\]]*\\)\\]}\\|")

;; These are the hash tables and list that are reset at every
;; evaluation of 'numbex--scan-buffer': they contain all the
;; information pertinent to the numbering and the referencing of examples.
(defvar-local numbex--label-number (make-hash-table :test 'equal)
  "Hash table mapping labels of examples to the number assigned.")

(defvar-local numbex--label-line (make-hash-table :test 'equal)
  "Hash table a label to the line of the corresponding example.")

(defvar-local numbex--duplicates nil
  "A list of non-empty labels that are not unique in the buffer.")

(defvar-local numbex--existing-labels nil
  "A list of non-empty labels that are used in the buffer.")

;; An example is the best way to explain what this variable is for.
;; Suppose there are five examples in the buffer: three in the first
;; page, two in the second.  The value of this variable will be:
;; ("1" "2" "3" "1" "2")

;; When 'numbex--add-numbering' is evaluated, if
;; 'numbex--relative-numbering' is t, the examples will be numbered in
;; loop by popping values out of this list.
(defvar-local numbex--relative-number-list nil
  "A list of strings with to the relative numbering in the buffer.")

(defvar-local numbex-relative-numbering nil
  "If nil, the numbering doesn't restart ever.
If t, the numbering restarts at every form-feed character, if the
buffer is not narrowed, or at the beginning of the narrowing, if
it is narrowed.)")

(defun numbex--scan-buffer ()
  "Collect information relevant for numbex from the buffer.
Remove all whitespace from items.  Reset the values for the
buffer-local variables.  The information is collected in a
widened indirect buffer, so that even if the current buffer is
narrowed, numbex will behave taking into consideration the entire
buffer.  This reduces the risk of working on a narrowed buffer
and ending up with many duplicate labels or mistaken references
once the buffer is widened again."
  ;; First of all, clear the two hash tables
  (clrhash numbex--label-number)
  (clrhash numbex--label-line)
  (let ((labels '())
        (duplicates '())
        (relative-numbers-list '())
        (absolute-counter 1)
        (relative-counter 1))
    (numbex--with-widened-indirect-buffer
     (lambda ()
       (while (re-search-forward numbex--ex-form-feed-re nil t)
         (if (equal "" (match-string-no-properties 0))
             ;; We hit a form-feed character: all there is to do is to
             ;; reset the relative counter to 1, so that the next
             ;; example item will be assigned "(1)".
             (setq relative-counter 1)
           ;; We hit an example item: we have to fill up our hash
           ;; tables and lists.
           (let* ((lab (buffer-substring-no-properties (match-beginning 2)
                                                       (match-end 2)))
                  ;; Here the choice of relative vs absolute numbering
                  ;; has an effect: this value will end up in the hash
                  ;; table that associates labels with numbers (that
                  ;; is, the one that is accessed when it's time to
                  ;; assign numbers to reference items).
                  (number (if numbex-relative-numbering
                              relative-counter
                            absolute-counter))
                  ;; For convenience, let's store strings of the form
                  ;; "(1)" instead of numbers like 1 in the lists, so
                  ;; we won't have to bother convert it later when
                  ;; it's time to put them as display text properties.
                  (n-string (concat (car numbex-delimiters)
                                    (number-to-string number)
                                    (cdr numbex-delimiters))))
             (push (concat (car numbex-delimiters)
                           (number-to-string relative-counter)
                           (cdr numbex-delimiters))
                   relative-numbers-list)
             (unless (string-blank-p lab)
               (when (and (member lab labels)
                          (not (member lab duplicates)))
                 ;; keep only one token for each type of duplicate
                 ;; label
                 (push lab duplicates))
               (push lab labels))      ; only non-empty labels matter
             (puthash lab n-string numbex--label-number)
             (puthash lab
                      (buffer-substring-no-properties
                       (match-end 0)
                       (line-end-position))
                      numbex--label-line))
           ;; Before the next iteration, increment both counters
           (setq absolute-counter (1+ absolute-counter))
           (setq relative-counter (1+ relative-counter))))))
    ;; Now we are out of the indirect buffer, and we can set the rest
    ;; of the buffer local variables with the values we obtained from
    ;; the loop.
    (setq numbex--relative-number-list (nreverse relative-numbers-list))
    (setq numbex--duplicates duplicates)
    (setq numbex--existing-labels labels)))

(defun numbex--remove-numbering ()
  "Remove all numbex text properties from the buffer.
Set 'numbex--hidden-labels' to nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward numbex--item-re nil t)
      (set-text-properties (match-beginning 0)
                           (match-end 0)
                           nil)))
  (numbex--toggle-font-lock-keyword)
  (numbex--highlight)
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
  (let ((direction (if next 1 -1)))
    (save-excursion
      (if (re-search-forward numbex--ex-re nil t direction)
          (cons (buffer-substring-no-properties (match-beginning 2)
                                                (match-end 2))
                (plist-get (text-properties-at (match-beginning 2))
                           'display))
        (cons "" "(--)")))))

(defun numbex--highlight ()
  "Highlight items without labels or with non-unique ones."
  (when (or numbex-highlight-duplicates
            numbex-highlight-unlabeled)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward numbex--r-ex-re nil t)
        (let ((b (match-beginning 0))
              (e (match-end 0))
              (type (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1)))
              (lab (buffer-substring-no-properties (match-beginning 2)
                                                   (match-end 2))))
          (cond ((and (equal type "ex")
                      (member lab numbex--duplicates)
                      numbex-highlight-duplicates)
                 (add-text-properties
                  b e
                  '(font-lock-face font-lock-warning-face))
                 (put-text-property b e 'rear-nonsticky t))
                ((and numbex-highlight-unlabeled
                      (string-blank-p lab))
                 (add-text-properties
                  b e
                  '(font-lock-face font-lock-comment-face))
                 (put-text-property b e 'rear-nonsticky t))
                (t nil)))))))

;; This function adds the numbers as display text property.  The first
;; pass is to number examples and simple references.  If
;; 'numbex-relative-numbering' it t, the numbers of the examples are
;; taken from 'numbex--relative-number-list', otherwise, the numbers
;; are just incremented from 1.  If the buffer is narrowed, however,
;; and 'numbex-relative-numbering' is t, we need to start from (1) in
;; the narrowed portion of the buffer no matter what (and number
;; references accordingly).
(defun numbex--add-numbering ()
  "Number items in the buffer as text properties.
Set 'numbex-hidden-labels' to t."
  (setq numbex--total-number-of-items 0)
  ;; First, let's number the examples.  If the buffer is narrowed
  ;; and 'numbex-relative-numbering' is t, we just need to number
  ;; the examples in the buffer.
  (if (and (buffer-narrowed-p)
           numbex-relative-numbering)
      (save-excursion
        (goto-char (point-min))
        (let ((counter 1))
          (while (re-search-forward numbex--ex-form-feed-re nil t)
            (if (equal "" (match-string-no-properties 0))
                (setq counter 1)
              ;; We hit an example: add to the total number first
              (setq numbex--total-number-of-items
                    (1+ numbex--total-number-of-items))
              (let ((n (concat (car numbex-delimiters)
                               (number-to-string counter)
                               (cdr numbex-delimiters)))
                    (label (buffer-substring-no-properties
                            (match-beginning 2)
                            (match-end 2)))
                    (b (match-beginning 0))
                    (e (match-end 0)))
                (set-text-properties b e nil)
                ;; Just put the number
                (put-text-property b e 'display n)
                ;; Change the entry in the hash table so that
                ;; reference to this example will pick up the number
                ;; relative to the narrowing instead fo the one
                ;; relative to the widened buffer.  Do this only if
                ;; the label is non-empty, of course.
                (unless (string-blank-p label)
                  (puthash label n numbex--label-number))
                (setq counter (1+ counter)))))))
    ;; Buffer is either not narrowed or 'numbex-relative-numbering'
    ;; is nil.  Either way, it's better if we do what we need to do
    ;; in an inderect buffer, using the same macro used in
    ;; 'numbex--scan-buffer'
    (numbex--with-widened-indirect-buffer
     (lambda ()
       (let ((abs-n 1))
         (while (re-search-forward numbex--ex-re nil t)
           ;; Add to the total number first
           (setq numbex--total-number-of-items
                 (1+ numbex--total-number-of-items))
           (let ((b (match-beginning 0))
                 (e (match-end 0))
                 ;; If we want relative-numbering, pop from the
                 ;; approriate list, otherwise just count:
                 (n (if numbex-relative-numbering
                        (pop numbex--relative-number-list)
                      (concat (car numbex-delimiters)
                              (number-to-string abs-n)
                              (cdr numbex-delimiters)))))
             (set-text-properties b e nil)
             (put-text-property b e 'display n)
             (setq abs-n (1+ abs-n))))))))
  ;; Now we can take care of the references, in a single loop:
  (numbex--with-widened-indirect-buffer
   (lambda ()
     (while (re-search-forward numbex--pnrex-re nil t)
       ;; Add to the total numer first.
       (setq numbex--total-number-of-items
             (1+ numbex--total-number-of-items))
       (let ((label (buffer-substring-no-properties (match-beginning 2)
                                                    (match-end 2)))
             (type (buffer-substring-no-properties (match-beginning 1)
                                                   (match-end 1)))
             (b (match-beginning 0))
             (e (match-end 0)))
         (set-text-properties b e nil)
         ;; Now, depending whether it's a rex:, pex: or nex: we must
         ;; do different things.
         (if (equal type "rex")
             ;; If the item is {[rex:]}, without a label, give it
             ;; the number of the closes example.  Otherwise, look
             ;; up the hash table to find the number to give.  If
             ;; no key corresponding to the label is found it means
             ;; that the non-empty label is not being used by any
             ;; example (yet), so the reference will appear as
             ;; "(--)":
             (let ((n (if (string-blank-p label)
                          (numbex--number-closest-example)
                        (gethash label
                                 numbex--label-number
                                 "(--)"))))
               (put-text-property b e 'display n))
           ;; We hit a special reference: first thing to do is to
           ;; remove it:
           (replace-match "")
           ;; Now we need to get the label and number of the closes
           ;; example: the function 'numbex--label-closest-example'
           ;; fetches this information about the example immediately
           ;; following of preceding point, depending if its argument
           ;; is t or nil.  Thus, we must feed it the correct argument
           ;; according to whether we deal with a pex: or nex: item.
           (let* ((closest
                   (numbex--label-closest-example (equal "nex" type)))
                  (new-label (car closest))
                  (new-number (cdr closest)))
             ;; Let's go to where the item was and insert the new one.
             (goto-char b)
             (insert (concat "{[" type ":" new-label "]}"))
             ;; Now let's search for it backwards to know where to put
             ;; the text property:
             (re-search-backward numbex--pnex-re nil t)
             (put-text-property (match-beginning 0)
                                (match-end 0)
                                'display new-number)
             (goto-char (match-end 0))))))))
  (numbex--toggle-font-lock-keyword t)
  (setq numbex--hidden-labels t))

(defun numbex-toggle-relative-numbering ()
  "Toggle value of 'numbex-relative-numbering' (buffer-local)."
  (interactive)
  (if numbex-relative-numbering
      (progn
        (setq numbex-relative-numbering nil)
        (message "Relative numbering deactivated"))
    (setq numbex-relative-numbering t)
    (message "Relative numbering activated")))

;;;###autoload
(defun numbex-toggle-display ()
  "Remove numbers if they are present, add them otherwise."
  (interactive)
  (when numbex--automatic-refresh
    (numbex-refresh))
  (if numbex--hidden-labels
      (numbex--remove-numbering)
    (numbex--add-numbering)))

(defun numbex--edit (item)
  "With ITEM being the output of 'numbex--item-at-point, insert new label."
  (save-excursion
    (let* ((old-label
            (buffer-substring-no-properties (car (car item))
                                            (cdr (car item))))
           (type (cdr item))
           (new-label
            (if (equal type "ex")
                (read-string (format
                              "New label [default \"%s\"]: "
                              old-label)
                             old-label nil
                             old-label t)
              ;; If the item is a reference, provide completion with
              ;; the existing labels.
              (car (list
                 (completing-read
                  (format "Label [default \"%s\"]: " old-label)
                  numbex--existing-labels
                  nil nil
                  old-label nil
                  old-label t)))))
           (novel (not (member new-label numbex--existing-labels))))
      (if (and (equal type "ex")
               (not novel)
               (not (equal new-label old-label)))
          (if (not (yes-or-no-p (concat new-label
                                        " is already a label, are you sure?")))
              (numbex--edit item)
            (goto-char (car (car item)))
            (delete-region (car (car item))
                           (cdr (car item)))
            (insert new-label))
        (goto-char (car (car item)))
        (delete-region (car (car item))
                       (cdr (car item)))
        (insert new-label))
      (when (and (equal type "ex") novel)
        (let ((rename (yes-or-no-p
                       "Relabel all corresponding references?"))
              (target (concat "{[rex:" old-label "]}"))
              (rep (concat "{[rex:" new-label "]}")))
          (when rename
            (goto-char (point-min))
            (while (search-forward target nil t)
              (delete-region (match-beginning 0)
                             (match-end 0))
              (insert rep)))))
      ;; If item at point is nex: or pex:, make it into a rex:,
      ;; otherwise the new label will be wiped out automatically.  It
      ;; does not make sense to edit pex: and nex: items manually.
      (when (or (equal type "nex")
                (equal type "pex"))
        (re-search-backward "{\\[[pn]+ex")
        (replace-match "{[rex" t)))))

(defun numbex--example ()
  "Insert a new example item."
  (let ((label
         (read-string "Label: "
                      nil nil
                      nil t)))
    (if (member label numbex--existing-labels)
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
                  numbex--existing-labels
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
    (when numbex--automatic-refresh
      (numbex-refresh t))
    (if hidden
        (numbex--add-numbering)
      (numbex--remove-numbering))))

(defun numbex--search-not-at-point ()
  "Let user choose a regexp and grep for it."
  (let ((choice (read-multiple-choice "Look for:"
                                      '((?a "all")
                                        (?e "examples")
                                        (?r "references")
                                        (?d "duplicates")
                                        (?u "unlabeled")))))
    (cond ((equal choice '(?a "all"))
           (occur numbex--item-re))
          ((equal choice '(?e "examples"))
           (occur numbex--ex-re))
          ((equal choice '(?r "references"))
           (occur numbex--pnrex-re))
          ((equal choice '(?d "duplicates"))
           (occur (regexp-opt
                   (nconc (mapcar (lambda (x) (concat "{[ex:"
                                                      x
                                                      "]}"))
                                  numbex--duplicates)
                          (mapcar (lambda (x) (concat "{[rex:"
                                                      x
                                                      "]}"))
                                  numbex--duplicates)))))
          ((equal choice '(?u "unlabeled"))
           (occur "{\\[[pnr]?ex:\s*\\]}")))))

(defun numbex--search-at-point ()
  "Let user choose a regexp and grep for it."
  (let* ((positions (car (numbex--item-at-point)))
         (label (buffer-substring-no-properties (car positions)
                                                (cdr positions)))
         (reg (if (string-blank-p label)
                  numbex--item-re
                (concat "{\\[[pnr]?ex:" label "\\]}")))
         (choice (read-multiple-choice "Look for:"
                                       '((?a "all")
                                         (?e "examples")
                                         (?r "references")
                                         (?l "label at point")
                                         (?d "duplicates")
                                         (?u "unlabeled")))))
    (cond ((equal choice '(?a "all"))
           (occur numbex--item-re))
          ((equal choice '(?e "examples"))
           (occur numbex--ex-re))
          ((equal choice '(?r "references"))
           (occur numbex--pnrex-re))
          ((equal choice '(?l "label at point"))
           (occur reg))
          ((equal choice '(?d "duplicates"))
           (occur (regexp-opt
                   (nconc (mapcar (lambda (x) (concat "{[ex:"
                                                      x
                                                      "]}"))
                                  numbex--duplicates)
                          (mapcar (lambda (x) (concat "{[rex:"
                                                      x
                                                      "]}"))
                                  numbex--duplicates)))))
          ((equal choice '(?u "unlabeled"))
           (occur "{\\[[pnr]?ex:\s*\\]}")))))

;;;###autoload
(defun numbex-search ()
  "Grep the buffer for items."
  (interactive)
  (numbex--remove-numbering)
  (numbex-refresh t)
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
      (let ((label (match-string-no-properties 2)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "\\label{ex:" label "}"))))
    (goto-char (point-min))
    (while (re-search-forward numbex--pnrex-re nil t)
      (let ((label (match-string-no-properties 2)))
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
      (let ((label (match-string-no-properties 2)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "{[ex:" label "]}"))))
    (goto-char (point-min))
    (while (re-search-forward "(\\\\ref{ex:\\([^\\]*\\)})" nil t)
      (let ((label (match-string-no-properties 2)))
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (insert (concat "{[rex:" label "]}")))))
  (when numbex--automatic-refresh
    (numbex-refresh)))

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
               (number (plist-get (text-properties-at (match-beginning 2))
                                  'display)))
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


(defun numbex-refresh (&optional no-echo)
  "Scan the buffer and assign numbers.
If NO-ECHO is non-nil, do not warn about duplicates.  This is to
be added to 'numbex-mode-hook', 'auto-save-hook' and
'before-save-hook'."
  (interactive)
  (let ((hidden numbex--hidden-labels))
    ;; First of all, remove whitespace from items.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward numbex--item-re nil t)
        (replace-match
         (replace-regexp-in-string
          "\s"
          ""
          (buffer-substring-no-properties (match-beginning 0)
                                          (match-end 0)))
         t)))
    (numbex--scan-buffer)
    (numbex--add-numbering)
    (unless hidden
      (numbex--remove-numbering))
    (unless no-echo
      (numbex--echo-duplicates))))

(defun numbex--timed ()
  "Housekeeping function to be evaluated on 'numbex--idle-timer'.
If not in 'numbex-mode' or 'numbex--hidden-labels' is nil, do
nothing.  Otherwise, if point is on an item, display its label
and, if appropriate, the context of the referenced example in the
echo area.  If point is not on an item, evaluate
'numbex--scan-buffer' and 'numbex--add-numbering' so that the
appearance of the buffer is kept up to date as far as numbex is
concerned."
  (when (and numbex-mode
             numbex--hidden-labels)
    (let ((item (numbex--item-at-point)))
      (if item
          ;; Point is on an item: show the underlying label.  If the
          ;; item is a reference, show the context of the
          ;; corresponding example as well.
          (let* ((type (cdr item))
                 (label
                  (buffer-substring-no-properties (car (car item))
                                                  (cdr (car item)))))
            (if (not (equal type "ex"))
                (message (concat label
                                 ": "
                                 (gethash label
                                          numbex--label-line
                                          " ")))
              (message label)))
        ;; Point is not on an item, just rescan the buffer and
        ;; renumber the items.  Do it only if this wouldn't cripple
        ;; everything.
        (numbex--highlight)
        (unless (or (> numbex--total-number-of-items 1000)
                    (not numbex--automatic-refresh))
          (numbex--scan-buffer)
          (numbex--add-numbering))))))

(defun numbex--count-and-ask ()
  "With more than 1000 items, ask whether to automatically refresh."
  (save-excursion
    (goto-char (point-min))
    (setq numbex--total-number-of-items 0)
    (while (re-search-forward numbex--item-re nil t)
      (setq numbex--total-number-of-items
            (1+ numbex--total-number-of-items)))
    (when (and (> numbex--total-number-of-items 1000)
               numbex--automatic-refresh)
      (let* ((question
              (format "There are %s items in the buffer.\nDo you want to disable automatic refresh (you can refresh yourself with 'numbex-refresh')?"
                      numbex--total-number-of-items))
             (choice (yes-or-no-p question)))
        (when choice
            (setq numbex--automatic-refresh nil))))))

(defun numbex--toggle-font-lock-keyword (&optional add)
  "Remove or add font-lock-keyword making numbex items invisible.
If the value of ADD is t, add the keyword and set
'numbex--hidden-labels' to t.  Otherwise, remove the keyword and
set the same variable to nil.  Finally, evaluate
'font-lock-update' for the changes to take effect."
  (if add
      (font-lock-add-keywords
       nil
       '(("{\\[[pnr]?ex:\\([^\]]*\\)\\]}"
          0 '(face nil invisible t) append)))
    (font-lock-remove-keywords
     nil
     '(("{\\[[pnr]?ex:\\([^\]]*\\)\\]}"
        0 '(face nil invisible t) append))))
  (font-lock-update))

;;;###autoload
(define-minor-mode numbex-mode
  "Automatically number examples and references to them."
  :init-value nil
  :global nil
  :lighter " nbex"
  :group 'convenience
  (if numbex-mode
      ;; activating numbex-mode
      (progn
        (unless numbex--idle-timer
          (setq numbex--idle-timer
                (run-with-idle-timer 0.3 t #'numbex--timed)))
        (unless font-lock-mode
          (font-lock-mode 1))
        (font-lock-add-keywords
         nil
         '(("{\\[[pnr]?ex:\\([^\]]*\\)\\]}"
            0 '(face nil invisible t) append)))
        (font-lock-update)
        (numbex--count-and-ask)
        (numbex-refresh)
        (add-hook 'auto-save-hook #'numbex-refresh nil t)
        (add-hook 'before-save-hook #'numbex-refresh nil t))
    ;; leaving numbex-mode
    (cancel-timer numbex--idle-timer)
    (setq numbex--idle-timer nil)
    (numbex--remove-numbering)
    (font-lock-remove-keywords
         nil
         '(("{\\[[pnr]?ex:\\([^\]]*\\)\\]}"
            0 '(face nil invisible t) append)))
    (font-lock-update)
    (remove-hook 'auto-save-hook #'numbex-refresh t)
    (remove-hook 'before-save-hook #'numbex-refresh t)))

(provide 'numbex)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; numbex.el ends here
