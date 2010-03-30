;;; umlaute.el --- support for german umlaute

;; Copyright (C) 1999 by Roland Winkler

;; Author: Roland.Winkler@physik.uni-erlangen.de
;; Version 0.5
;; Time-stamp: <2003-05-17 21:56:41 winkler>

;; umlaute.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; umlaute.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; umlaute.el provides support for typing German umlaute, and it may
;; be useful for other human (European) languages, too.
;;
;; Iso-8859-1 is fine. But depending on the mode of the buffer
;; umlaute should be 'spelled' in different ways:
;; (I live in southern Germany)
;;
;; text-mode:          Grüß Gott
;; tex-mode:           Gr\"u{\ss} Gott
;; german latex-mode:  Gr"u"s Gott
;; html-mode:          Gr&uuml;&szlig; Gott
;; other modes:        Gruess Gott
;;
;; The idea of umlaute.el is that it is convenient to use the same
;; keys for typing umlaute such that one key inserts either ü, \"u,
;; "u, &uuml;, etc. This can be obtained by putting the function
;; umlaute-keys into the mode-hooks with appropriate choices for the
;; spelling of the umlaute.

;; Example:  (more or less what I have in my .emacs)
;;
;; (require 'umlaute)
;; (umlaute-keys umlaute-7bit t)   ;; my default choice
;; (add-hook 'text-mode-hook '(lambda () (umlaute-keys umlaute-8bit)))
;; (add-hook 'latex-mode-hook '(lambda () (umlaute-keys umlaute-tex)))
;; (add-hook 'hm--html-mode-hook '(lambda () (umlaute-keys umlaute-html)))

;; In abbrev-mode abbreviations can be expanded in a mode-dependent
;; way by using umlaute-abbrev-hook. The original expansion text
;; should use iso-8859-1. Example:
;;
;; (define-abbrev text-mode-abbrev-table 
;;                "gg" "Grüß Gott" 'umlaute-abbrev-hook)
;;
;; This will expand in different modes as shown above.

;; The function umlaute-replace replaces one spelling of umlaute in
;; a buffer by other spellings.

;; The keys for typing umlaute are defined in the variable
;; umlaute-key. I have a US keyboard. Therefore I use 'C-c ..'. You
;; might want to change that. Moreover you might want to add other
;; ways of spelling umlaute.

;; Comments / suggestions welcome!

;;; Code:

(provide 'umlaute)

(defvar umlaute-key  '("\C-cA" "\C-cO" "\C-cU" "\C-ca" "\C-co" "\C-cu"
                       "\C-cs")
  "Keys for typing umlaute.")
(defvar umlaute-8bit (mapcar 'string '(196 214 220 228 246 252 223))
  "Umlaute according to iso-8859-1 (8bit).")
(defvar umlaute-7bit '("Ae" "Oe" "Ue" "ae" "oe" "ue" "ss")
  "Umlaute according to 7bit transcription.")
(defvar umlaute-tex  '("\\\"A" "\\\"O" "\\\"U" "\\\"a" "\\\"o" "\\\"u"
                       "{\\ss}")
  "Umlaute in TeX documents.")
(defvar umlaute-tex-long  '("\\\"{A}" "\\\"{O}" "\\\"{U}"
                            "\\\"{a}" "\\\"{o}" "\\\"{u}" "{\\ss}")
  "'Long' Umlaute in TeX documents.")
(defvar umlaute-gtex  '("\"A" "\"O" "\"U" "\"a" "\"o" "\"u" "\"s")
  "Umlaute in German LaTeX documents.")
(defvar umlaute-html '("&Auml;" "&Ouml;" "&Uuml;" "&auml;" "&ouml;"
                       "&uuml;" "&szlig;")
  "Umlaute in HTML documents.")
(defvar umlaute-mime '("=C4" "=D6" "=DC" "=E4" "=F6" "=FC" "=DF")
  "Umlaute in MIME documents.")
(defvar umlaute-alist (mapcar 'list '("8bit" "7bit" "tex" "gtex" "html"
                                      "tex-long" "mime"))
  "Alist with all spellings for umlaute.")

(defvar umlaute-abbrev-list nil
  "Umlaute for expansion of abbreviations.
This variable's value is local in all buffers. It is set by
umlaute-keys. Otherwise use setq-default to change the default
setting or setq to change the buffer-local value of this variable.")
(make-variable-buffer-local 'umlaute-abbrev-list)

(defun umlaute-keys (umlaute &optional global)
  "Locally define keys for umlaute according to list UMLAUTE.
With prefix GLOBAL define keys globally. This function also sets the
variable umlaute-abbrev-list to UMLAUTE."
  (interactive
   (list (symbol-value (intern (concat "umlaute-"
                                       (completing-read
                                        "Umlaute: " umlaute-alist nil t))))
         current-prefix-arg))
  (if global (setq-default umlaute-abbrev-list umlaute)
    (setq umlaute-abbrev-list umlaute))
  (let ((key umlaute-key)
        (set-key (if global 'global-set-key 'local-set-key)))
    (while (and (car key) (car umlaute))
      (funcall set-key (car key) 
               `(lambda () (interactive "*") (insert ,(car umlaute))))
      (setq key (cdr key)
            umlaute (cdr umlaute)))))

(defun umlaute-replace (from to &optional start end query)
  "Replace umlaute in list FROM by umlaute in list TO.
By default operate on the content of the buffer. Optional arguments
START, END delimit the region. When called interactively, in
transient mark mode, if the mark is active, operate on the contents
of the region.
With prefix QUERY do query-replace."
  (interactive (let* ((from (completing-read "From: " umlaute-alist nil t))
                      (to (completing-read (concat "From: " from " to: ")
                                           umlaute-alist nil t))
                      start end)
                 (when (and transient-mark-mode mark-active)
                   (setq start (region-beginning)
                         end (region-end))
                   (deactivate-mark))
                 (list (symbol-value (intern (concat "umlaute-" from)))
                       (symbol-value (intern (concat "umlaute-" to)))
                       start end current-prefix-arg)))
  (let (case-fold-search old new)
    (if (not start) (setq start (point-min)))
    (if (not end)   (setq end   (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (while (and (setq old (car from))
                    (setq new (car to)))
          (goto-char (point-min))
          ;; German 7bit transcription for umlaute requires spell-checking
          (if query (query-replace old new)
            (while (search-forward old nil t)
              (replace-match new t t)))
          (setq from (cdr from)
                to   (cdr to)))))))

(defun umlaute-abbrev-hook ()
  "Hook for proper umlaute when expanding abbreviations.

Requires that the variable umlaute-abbrev-list is set. This
happens, e.g., via the function umlaute-keys."
  (if umlaute-abbrev-list
      (let ((from umlaute-8bit) (to umlaute-abbrev-list)
            start (end (point)) old new case-fold-search)
        (save-excursion
          (save-restriction
            (narrow-to-region (progn (search-backward 
                                      (symbol-value last-abbrev))
                                     (point)) end)
            (while (and (setq old (car from))
                        (setq new (car to)))
              (goto-char (point-min))
              (while (search-forward old nil t)
                (replace-match new nil t))
              (setq from (cdr from)
                    to   (cdr to))))))))

;;; umlaute.el ends here
