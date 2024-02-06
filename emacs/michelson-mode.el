;;; michelson-mode.el --- Major mode for editing Michelson smart contracts  -*- lexical-binding: t -*-

;; Open Source License
;; Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Author: Nomadic Labs, <contact@nomadic-labs.com>
;; Created: 20 Jul 2020
;; Keywords: languages
;; Homepage: https://gitlab.com/tezos/tezos
;; URL: https://gitlab.com/tezos/tezos
;; Version: 0.1
;; Licence: MIT
;; Package-Requires: ((deferred "0.5.1") (emacs "24.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file defines a major mode for editing Michelson files.  The
;; documentation for this major mode is available at
;; https://tezos.gitlab.io/user/various.html#environment-for-writing-michelson-contracts,
;; the documentation of the Michelson language is available at
;; https://tezos.gitlab.io/whitedoc/michelson.html

;;; Code:

(require 'cl-lib)
(require 'deferred)
(require 'font-lock)

(defgroup michelson nil
  "Major mode for editing Michelson smart contracts."
  :prefix "michelson-"
  :group 'languages)

(defgroup michelson-options nil
  "General options for Michelson mode."
  :prefix "michelson-"
  :group 'michelson)

(defcustom michelson-client-command "tezos-client"
  "Path to the `octez-client' binary."
  :type 'string
  :group 'michelson-options)

(defcustom michelson-alphanet nil
  "Is the client command currently using a Docker script such as mainnet.sh or carthagenet.sh?"
  :type 'boolean
  :group 'michelson-options)

(defgroup michelson-faces nil
  "Font lock faces for Michelson mode."
  :prefix "michelson-"
  :group 'michelson
  :group 'faces)

(defcustom michelson-live-editing t
  "Toggles live types and error printing.
Overrides `michelson-print-errors' and `michelson-highlight-errors'"
  :group 'michelson-options
  :type 'boolean)

(defcustom michelson-print-errors t
  "Print the errors in the output buffer."
  :type 'boolean
  :group 'michelson-options)

(defcustom michelson-highlight-errors t
  "Highlight errors in the source buffer."
  :type 'boolean
  :group 'michelson-options)

(defcustom michelson-extra-flags nil
  "Additional flags to pass to octez-client when type checking."
  :type '(list string)
  :group 'michelson-options)

(put 'michelson-extra-flags 'safe-local-variable t)

(defvar michelson-face-instruction
  'michelson-face-instruction
  "Face name for Michelson instructions.")
(defface michelson-face-instruction
  '((t (:inherit font-lock-keyword-face)))
  "Face for Michelson instructions."
  :group 'michelson-faces)

(defvar michelson-face-type
  'michelson-face-type
  "Face name for Michelson types.")
(defface michelson-face-type
   '((t (:inherit font-lock-type-face)))
   "Face for Michelson types."
   :group 'michelson-faces)

(defvar michelson-face-constant
  'michelson-face-constant
  "Face name for Michelson constants.")
(defface michelson-face-constant
   '((t (:inherit font-lock-constant-face)))
   "Face for Michelson constants."
   :group 'michelson-faces)

(defvar michelson-face-var-annotation
  'michelson-face-var-annotation
  "Face name for Michelson variable or binding annotations.")
(defface michelson-face-var-annotation
   '((t (:inherit font-lock-variable-name-face)))
   "Face for Michelson variable or binding annotations."
   :group 'michelson-faces)

(defvar michelson-face-type-annotation
  'michelson-face-type-annotation
  "Face name for Michelson type or field annotations.")
(defface michelson-face-type-annotation
   '((t (:inherit font-lock-string-face)))
   "Face for Michelson type or field annotations."
   :group 'michelson-faces)

(defvar michelson-face-comment
  'michelson-face-comment
  "Face name for Michelson comments.")
(defface michelson-face-comment
   '((t (:inherit font-lock-comment-face)))
   "Face for Michelson comments."
   :group 'michelson-faces)

(defvar michelson-face-declaration
  'michelson-face-declaration
  "Face name for Michelson declarations.")

(defface michelson-face-declaration
   '((t (:inherit font-lock-keyword-face)))
   "Face for Michelson constants."
   :group 'michelson-faces)

(defvar michelson-face-error
  'michelson-face-error
  "Face name for Michelson comments.")

(defface michelson-face-error
  '(( ((class color) (background light)) (:background "MistyRose") )
    ( ((class color) (background dark)) (:background "DarkRed") ))
   "Face for Michelson annotations."
   :group 'michelson-faces)

(defface michelson-stack-highlight-face
  '(( ((class color) (background light)) (:background "gray86") )
    ( ((class color) (background dark)) (:background "grey21") ))
  "Face for alternating lines of the stack."
  :group 'michelson-faces)

(defun michelson-customize-options ()
  "Open the general customization group for Michelson mode."
  (interactive)
  (customize-group-other-window `michelson-options))

(defun michelson-customize-faces ()
  "Open the face customization group for Michelson mode."
  (interactive)
  (customize-group-other-window `michelson-faces))

(defun michelson-toggle-print-errors ()
  (interactive)
  (setq michelson-print-errors (not michelson-print-errors)))

(defun michelson-highlight-errors ()
  (interactive)
  (setq michelson-highlight-errors (not michelson-highlight-errors)))

(defconst michelson-mode-map
  (let ((michelson-mode-map (make-sparse-keymap)))
    ;; menu
    (define-key michelson-mode-map
      [menu-bar michelson-menu]
      (cons "Michelson" (make-sparse-keymap "michelson-menu")))
    (define-key michelson-mode-map
      [menu-bar michelson-menu faces]
      (cons "Display options group" 'michelson-customize-faces))
    (define-key michelson-mode-map
      [menu-bar michelson-menu options]
      (cons "General options group" 'michelson-customize-options))
    (define-key michelson-mode-map
      [menu-bar michelson-menu separator]
      '(menu-item "--"))
    (define-key michelson-mode-map
      [menu-bar michelson-menu what]
      (cons "What's under the cursor?" 'michelson-type-at-point))
    ;; keys
    (define-key michelson-mode-map
      (kbd "C-j") 'newline-and-indent)
    (define-key michelson-mode-map
      (kbd "TAB") 'indent-for-tab-command)
    (define-key michelson-mode-map
      (kbd "<f1>") 'michelson-type-at-point)
    (define-key michelson-mode-map
      (kbd "C-c C-x") 'michelson-goto-next-error)
    michelson-mode-map))

(defun michelson-font-lock-syntactic-face-function (s)
  (cond ((nth 3 s) 'font-lock-constant-face)
        (t 'michelson-face-comment)))

(defconst michelson-font-lock-defaults
  (list
   (list
    '("\\<[@]\\(\\|%\\|%%\\|[A-Za-z-_][A-Za-z-_0-9\.]*\\)\\>" . michelson-face-var-annotation)
    '("\\<[%:]\\(\\|@\\|[A-Za-z-_][A-Za-z-_0-9\.]*\\)\\>" . michelson-face-type-annotation)
    '("-?\\<[0-9]+\\>" . michelson-face-constant)
    '("\\<0[xX][0-9a-fA-F]*\\>" . michelson-face-constant)
    '("\\<[A-Z][a-z_0-9]+\\>" . michelson-face-constant)
    '("\\<[A-Z][A-Z_0-9]*\\>" . michelson-face-instruction)
    ;; This will have problems if users have whitespace in front of the declarations
    '("^parameter\\|^return\\|^storage\\|^code" . michelson-face-declaration)
    '("\\<[a-z][a-z_0-9]*\\>" . michelson-face-type))
   nil nil nil nil
   '(font-lock-syntactic-face-function . michelson-font-lock-syntactic-face-function)))

(defconst michelson-mode-syntax-table
  (let ((michelson-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" michelson-mode-syntax-table)
    (modify-syntax-entry ?@ "w" michelson-mode-syntax-table)
    (modify-syntax-entry ?: "w" michelson-mode-syntax-table)
    (modify-syntax-entry ?% "w" michelson-mode-syntax-table)
    (modify-syntax-entry ?/ ". 1n4" michelson-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" michelson-mode-syntax-table)
    (modify-syntax-entry ?# "<b" michelson-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" michelson-mode-syntax-table)
    michelson-mode-syntax-table))

(defun michelson-in-space-or-comment ()
  (or (nth 4 (syntax-ppss))                       ; inside comment
      (memq (char-syntax (char-after)) '(?> ?\s)) ; space or eol
      ))

(defun michelson-goto-previous-token ()
  (interactive)
  (if (bobp)
      (cons 0 nil)
    (progn
      (backward-char 1)
      (while (and (not (bobp)) (michelson-in-space-or-comment)) (backward-char 1))
      (let ((token-face (get-text-property (point) 'face)))
        (forward-char 1)
        (let ((end-of-token (point)))
          (backward-char 1)
          (unless (looking-at "[{()};]")
            (while (and (not (bobp))
                        (equal (get-text-property (point) 'face) token-face))
              (backward-char 1))
            (when (not (equal (get-text-property (point) 'face) token-face))
              (forward-char 1)))
          (cons (point) (buffer-substring-no-properties (point) end-of-token)))))))

(defun michelson-goto-next-token ()
  (interactive)
  (if (eobp)
      (cons (point) nil)
    (progn
      (while (and (not (eobp)) (michelson-in-space-or-comment)) (forward-char 1))
      (let ((token-face (get-text-property (point) 'face)))
        (let ((start-of-token (point)))
          (if (looking-at "[{()};]")
              (forward-char 1)
            (progn
              (while (and (not (eobp))
                          (equal (get-text-property (point) 'face) token-face))
                (forward-char 1))))
          (cons start-of-token (buffer-substring-no-properties start-of-token (point))))))))

(defun michelson-goto-opener ()
  (interactive)
  (let ((paren-level 0))
    (while (and (not (bobp))
                (or (> paren-level 0)
                    (not (looking-at "[{(]"))))
      (cond ((looking-at "[{(]")
             (setq paren-level (- paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (+ paren-level 1))))
      (michelson-goto-previous-token))
    (cons (point)
          (when (looking-at "[{(]")
            (buffer-substring-no-properties (point) (+ (point) 1))))))

(defun michelson-goto-closer ()
  (interactive)
  (let ((paren-level 0) (last-token ""))
    (while (and (not (eobp))
                (or (> paren-level 0)
                    (not (string-match "[)}]" last-token))))
      (cond ((looking-at "[{(]")
             (setq paren-level (+ paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (- paren-level 1))))
      (setq last-token (cdr (michelson-goto-next-token))))
    (cons (point)
          (when (looking-at "[)}]")
            (buffer-substring-no-properties (point) (+ (point) 1))))))

(defun michelson-goto-previous-application-start ()
  (interactive)
  (let ((paren-level 0) (application-start 0))
    (while (and (not (bobp))
                (or (> paren-level 0)
                    (not (looking-at "[{(;]"))))
      (cond ((looking-at "[{(]")
             (setq paren-level (- paren-level 1)))
            ((looking-at "[})]")
             (setq paren-level (+ paren-level 1))))
      (setq application-start (point))
      (michelson-goto-previous-token))
    (cons application-start (goto-char application-start))))

(defun michelson-indent ()
  "Indent current line of Michelson code."
  (interactive)
  (let ((new-indentation 0)
        (previous-indentation (current-indentation))
        (previous-column (current-column))
        (current-token
         (save-excursion
           (beginning-of-line 1)
           (michelson-goto-next-token))))
    (save-excursion
      (end-of-line 0)
      (let ((previous-token
             (save-excursion (michelson-goto-previous-token)))
            (previous-opener
             (save-excursion (michelson-goto-opener))))
        (cond ((and (not (cdr previous-opener))
                    (not (cdr previous-token)))
               (setq new-indentation 0))
              ((and (not (cdr previous-opener))
                    (equal (cdr previous-token) ";"))
               (setq new-indentation 0))
              ((not (cdr previous-opener))
               (setq new-indentation 2))
              ((and (equal (cdr current-token) "}")
                    (equal (cdr previous-opener) "{"))
               (goto-char (car previous-opener))
               (setq new-indentation (current-column)))
              ((and (or (equal (cdr previous-token) ";")
                        (equal (cdr previous-token) "{"))
                    (equal (cdr previous-opener) "{"))
               (goto-char (car previous-opener))
               (setq new-indentation (+ (current-column) 2)))
              ((equal (cdr previous-opener) "{")
               (progn
                 (michelson-goto-previous-application-start)
                 (let ((default-param-indentation
                         (+ (current-column) 2))
                       (first-param-point
                        (save-excursion
                          (michelson-goto-next-token)
                          (car (michelson-goto-next-token)))))
                   (if (= first-param-point (car current-token))
                       (setq new-indentation default-param-indentation)
                     (progn
                       (goto-char first-param-point)
                       (setq new-indentation (current-column)))))))
              ((and (equal (cdr current-token) ")")
                    (equal (cdr previous-opener) "("))
               (goto-char (car previous-opener))
               (setq new-indentation (current-column)))
              ((equal (cdr previous-token) "(")
               (goto-char (car previous-token))
               (setq new-indentation (+ (current-column) 1)))
              ((equal (cdr previous-opener) "(")
               (goto-char (car previous-opener))
               (setq new-indentation (+ (current-column) 3))))))
    (indent-line-to new-indentation)
    (beginning-of-line)
    (forward-char
     (+ (- previous-column previous-indentation) new-indentation))
    (when (< (current-column) new-indentation)
      (beginning-of-line)
      (forward-char new-indentation))))

(defvar michelson-output-buffer-name
  "*Michelson*")

(defun michelson-token-at-point ()
  "Display the token closest to the cursor."
  (interactive)
  (let ((message
         (cdr (save-excursion
                (michelson-goto-next-token)
                (michelson-goto-previous-token)))))
    (display-message-or-buffer message michelson-output-buffer-name)))

(cl-defstruct cache
  "Cache for types. Invalid entries are removed"
  types
  errors)

(defvar michelson-cached-buffer-info (make-cache :types '() :errors '()))

(defvar michelson-process-output-buffer "*Michelson-process*")

(defun michelson-erase-process-buffer ()
  "Remove all text from process buffer."
  (get-buffer-create michelson-process-output-buffer)
  (with-current-buffer michelson-process-output-buffer
    (erase-buffer)))

(defun michelson-async-command-to-string (command callback)
  "Asynchronously execute `COMMAND' and call the `CALLBACK' on the resulting string."
  (deferred:$
    (deferred:$
      (apply 'deferred:process-shell (append command '("2>" "/dev/null")))
      (deferred:nextc it callback))
    ;; TODO: make this show only the client error
    (deferred:error it (lambda (err) (michelson-write-output-buffer (cadr err))))))

(defun michelson-clean-cache ()
  "Clean the buffer's program info cache."
  (let ((types (cache-types michelson-cached-buffer-info))
        (errors (cache-errors michelson-cached-buffer-info))
        (clean-cache-entry
         (lambda (alist)
           (cl-remove-if (lambda (entry)
                        (let ((tok-end (cadr entry)))
                          (> tok-end (point))))
                      alist))))
    (setq michelson-cached-buffer-info
          (make-cache :types (funcall clean-cache-entry types)
                      :errors (funcall clean-cache-entry errors)))))

(defun michelson-get-info (buffer-name)
  "Refresh the info about the program in `BUFFER-NAME' from the command."
  (let ((tmp-file (make-temp-file buffer-name)))
    (set-file-modes tmp-file #o644)
    (write-region (point-min) (point-max) tmp-file nil 'no-message)
    (let ((command
           (append (split-string michelson-client-command " ")
                   `("typecheck"
                     "script"
                     ,(shell-quote-argument
                       (if michelson-alphanet
                           (concat "container:" tmp-file)
                         tmp-file))
                     "-details"
                     "--emacs")
                   michelson-extra-flags)))
      (michelson-async-command-to-string
       command
       (lambda (output)
         (condition-case _err
             (let*
                 ((record (car (read-from-string output)))
                  (errors (cdr (assoc 'errors record)))
                  (types  (cdr (assoc 'types record))))
               (setq michelson-cached-buffer-info (make-cache :types types :errors errors))
               (delete-file tmp-file))
           (error
            (let ((inhibit-message t))
              (message output)))))))))

(defun michelson-output-width ()
  (let* ((buffer (get-buffer-create michelson-output-buffer-name))
         (message-window
          (or (get-buffer-window buffer)
              (display-buffer-below-selected buffer nil))))
    (window-body-width message-window)))

(define-derived-mode michelson-stack-mode special-mode "Michelson-stack"
  "Major mode for visualizing the Michelson stack."
  :syntax-table michelson-mode-syntax-table
  (setq font-lock-defaults michelson-font-lock-defaults)
  (setq indent-tabs-mode nil))

(defun michelson-write-output-buffer (data &optional do-not-overwrite)
  "Write the given `DATA' to the output buffer.
If `DATA' is a string, it is written directly,
overwriting the data in the buffer.
If `DATA' is a list of strings, the strings are written into the buffer,
with alternating lines highlighted.
If `DO-NOT-OVERWRITE' is non-nil, the existing contents of the buffer are maintained."
  (let* ((buffer (get-buffer-create michelson-output-buffer-name))
         (message-window
          (or (get-buffer-window buffer)
              (display-buffer-below-selected buffer nil)))
         (lines 0))
    (when (get-buffer-window buffer)
      (set-window-dedicated-p message-window t))
    (with-current-buffer michelson-output-buffer-name
      (let ((inhibit-read-only t))
        (unless do-not-overwrite
          (erase-buffer))
        (goto-char (point-min))
        (remove-overlays)
        (if (listp data)
            (let ((michelson-highlighting t))
              (dolist (ele (reverse data))
                (let ((prev-point (point)))
                  (insert ele)
                  (when michelson-highlighting
                    (overlay-put (make-overlay prev-point (point))
                                 'face 'michelson-stack-highlight-face))
                  (setq michelson-highlighting (not michelson-highlighting)))))
          (insert data))
        (michelson-stack-mode)
        (goto-char (point-min))
        (while (not (eobp))
          (vertical-motion 1)
          (setq lines (+ 1 lines)))
        (window-resize
         message-window
         (min (- (window-total-height) 5)
              (+ (- (max 4 lines)
                    (window-size message-window))
                 2)))))))

(defun michelson-format-stack-top (bef-ele aft-ele width)
  (let*
      ((pp-no-trailing-newline
        (lambda (sexp)
          (let* ((str (replace-regexp-in-string "\\\\\." "." (pp-to-string sexp)))
                 (len (length str)))
            (if (equal "\n" (substring str (- len 1) len))
                (substring str 0 (- len 1))
              str))))
       (bef-strs (if bef-ele (split-string (funcall pp-no-trailing-newline bef-ele) "\n") '("")))
       (aft-strs (if aft-ele (split-string (funcall pp-no-trailing-newline aft-ele) "\n") '(""))))
    (letrec ((format-strings
              (lambda (befs afts)
                (if (or befs afts)
                    (let ((aft-stack (if afts (car afts) "")))
                      (concat (format (format "%%-%ds|%s%%s\n"
                                              (/ width 2)
                                              (if (equal aft-stack "") "" "     "))
                                      (if befs (car befs) "")
                                      aft-stack)
                              (funcall format-strings (cdr befs) (cdr afts))))
                  ""))))
      (funcall format-strings bef-strs aft-strs))))


(defun michelson-format-stacks (bef-stack aft-stack)
  (letrec ((michelson-format-stacks-help
            (lambda (bef aft)
              (if (or bef aft)
                  (cons (michelson-format-stack-top (car bef) (car aft) (michelson-output-width))
                        (funcall michelson-format-stacks-help (cdr bef) (cdr aft)))
                '()))))
    (funcall michelson-format-stacks-help (reverse bef-stack) (reverse aft-stack))))

(cl-defstruct michelson-stacks
  "A pair of stacks, from `BEF' (before) and `AFT' (after) the instruction"
  bef
  aft)

(defun michelson-get-previous-stack ()
  (save-excursion
    (michelson-goto-previous-token)
    (let ((stacks nil)
          (brace-count 0)
          (break nil))
      (while (and (not break)
                  (not stacks)
                  (> (point) 0)
                  (>= brace-count 0))
        (backward-char)
        (cond ((and (equal (get-text-property (point) 'face)
                           'michelson-face-instruction)
                    (= brace-count 0))
               (setq break t)
               (setq stacks (michelson-stacks-at-loc (point))))
              ((equal (string (char-after (point))) "{")
               (setq brace-count (- brace-count 1)))
              ((equal (string (char-after (point))) "}")
               (setq brace-count (+ brace-count 1)))
              (t nil)))
      stacks)))


(defun michelson-completion-at-point ()
  (let ((prev-stack (michelson-get-previous-stack)))
    (if prev-stack
        (let* ((bds (bounds-of-thing-at-point 'word))
               (start (car bds))
               (end (cdr bds))
               (completion-stack (michelson-stacks-aft prev-stack))
               (instrs (michelson-get-suggestion-list completion-stack)))
          (list start end instrs . nil))
      nil)))

(defun michelson-stacks-at-loc (loc)
  (let ((types-info nil))
    (dolist (elt (cache-types michelson-cached-buffer-info))
      (when (and (<= (car elt) loc) (<= loc (cadr elt))
                 (equal (get-text-property loc 'face)
                        'michelson-face-instruction))
          (setq types-info (make-michelson-stacks :bef (caddr elt)
                                                  :aft (cadddr elt)))))
    types-info))

(defun michelson-show-program-info ()
  "Show the program's `TYPES' and `ERRORS'."
  (interactive)
  (remove-overlays)
  (let* ((stacks (michelson-stacks-at-loc (point)))
         (types-info (and stacks (michelson-format-stacks (michelson-stacks-bef stacks)
                                                          (michelson-stacks-aft stacks))))
         (errors-info nil))
    (when michelson-highlight-errors
      (dolist (elt (cache-errors michelson-cached-buffer-info))
        (overlay-put (make-overlay (car elt) (cadr elt)) 'face 'michelson-face-error)
        (when (and (<= (car elt) (point)) (<= (point) (cadr elt)))
          (progn
            (when michelson-print-errors
              (unless errors-info
                (setq errors-info (concat errors-info "\n")))
              (setq errors-info (concat errors-info (cadr (cdr elt)))))))))
    (cond ((and types-info errors-info)
           (michelson-write-output-buffer errors-info nil)
           (michelson-write-output-buffer types-info t))
          (types-info
           (michelson-write-output-buffer types-info nil))
          (errors-info
           (michelson-write-output-buffer errors-info nil))
          (t (michelson-write-output-buffer "\nNo information available at point")))))

(defun michelson-type-at-point ()
  "Display the type of the expression under the cursor."
  (interactive)
  (michelson-get-info (buffer-name))
  (michelson-show-program-info))


(defun michelson-goto-next-error ()
  "goto the first error found after the cursor, wrapping at the end of
buffer."
  (interactive)
  (let ((errors (cache-errors michelson-cached-buffer-info)))
    (if errors
        (let ((next (cl-find-if
                     (lambda (err) (> (car err) (point)) errors nil)
                     errors)))
          (if next
              (goto-char next)
            (goto-char (caar errors))))
      (message "no error"))))

(defun michelson-make-suggest (instr pred)
  "Suggest `INSTR' if `PRED' is not nil."
  (lambda (stack)
    (if (funcall pred stack)
        (if (listp instr)
            instr
          `(,instr))
      nil)))


(defun michelson-constrained-p (var hash)
  (not (equal var (gethash var hash var))))

(defun michelson-polymorphic-match (tbl match-types stack)
  (cond ((not match-types) t)
        ((not stack) nil)
        ((and (consp match-types) (consp stack))
         (and (michelson-polymorphic-match tbl (car match-types) (car stack))
              (michelson-polymorphic-match tbl (cdr match-types) (cdr stack))))
        ((and (symbolp match-types) (symbolp stack))
         (if (and (michelson-constrained-p match-types tbl)
                  (gethash match-types tbl))
             (equal (gethash match-types tbl nil) stack)
           (progn
             (puthash match-types stack tbl)
             t)))
        (t nil)))

(defmacro michelson-forall (vars matching-stack)
  (declare (indent defun))
  (unless (listp ',vars)
    (error "michelson-forall must take a list of vars"))
  `(lambda (stack)
     (let ((tbl (make-hash-table :test 'equal)))
       ,@(mapcar (lambda (var) `(puthash ',var ',var tbl)) vars)
       (michelson-polymorphic-match tbl ',matching-stack stack))))

(defun michelson-literals-match-p (types)
  "Generate a predicate that matches `TYPES' against the top of the stack."
  (lambda (stack)
    (michelson-polymorphic-match
     (make-hash-table :test 'equal)
     types
     stack)))

(defun michelson-suggest-literals (instr &rest types)
  "Suggest `INSTR' when `TYPES' are on the top of the stack."
  (michelson-make-suggest
   instr
   (michelson-literals-match-p types)))


(defun michelson-suggest-or (instr pred1 pred2)
  (michelson-make-suggest
   instr
   (lambda (stack) (or (funcall pred1 stack) (funcall pred2 stack)))))

(defun michelson-suggest-reorderable (instr type1 type2)
  (michelson-suggest-or instr
                        (michelson-literals-match-p `(,type1 ,type2))
                        (michelson-literals-match-p `(,type2 ,type1))))

(defvar michelson-suggest-always-available
  '("FAIL" "PUSH" "UNIT" "LAMBDA" "NONE"
    "EMPTY_SET" "EMPTY_MAP" "NIL" "BALANCE"
    "AMOUNT" "STEPS_TO_QUOTA" "NOW"))

(defun michelson-comparable-p (type)
  "Is the `TYPE' comparable?"
  (memq type '(int nat string tez bool key timestamp)))


(defun michelson-suggest-pairs-help (pair-type accessor-prefix)
  "Suggest all possible pair accessors on the given `PAIR-TYPE' and `ACCESSOR-PREFIX'."
  (cons (concat accessor-prefix "R")
        (if (and (consp pair-type) (equal (car pair-type) 'pair))
            (let ((car-ele (cadr pair-type))
                  (cdr-ele (caddr pair-type)))
              (append
               (michelson-suggest-pairs-help car-ele
                                             (concat accessor-prefix "A"))
               (michelson-suggest-pairs-help car-ele
                                             (concat accessor-prefix "D")))))))

(defun michelson-suggest-pairs (stack)
  "Suggest all possible pair accessors on the given `STACK'."
  (if (and (consp (car stack)) (equal (caar stack) 'pair))
      (append (michelson-suggest-pairs-help (cadar stack) "CA")
              (michelson-suggest-pairs-help (caddar stack) "CD"))))

(defconst michelson-comparison-operations
  '("EQ" "NEQ" "LT" "LE" "GT" "GE"))

(defun michelson-suggest-comparable (stack)
  (let ((first (car stack))
        (second (cadr stack)))
    (if (and first
             second
             (michelson-comparable-p first)
             (equal first second))
        (cons
         "COMPARE"
         (append
          (mapcar (lambda (x) (concat "CMP" x))
                  michelson-comparison-operations)
          (mapcar (lambda (x) (concat "IFCMP" x))
                  michelson-comparison-operations)))
      '())))

(defun michelson-suggest-depth (instrs depth)
  "Suggest `INSTRS' if the stack depth is greater than or equal to `DEPTH'."
  (michelson-make-suggest
   instrs
   (lambda (stack) (>= (length stack) depth))))

(defun michelson-suggest-prefix-depth (prefix additional suffix)
  (lambda (stack)
    (reverse (car (cl-reduce
                   (lambda (acc ele)
                     (let ((existing (car acc))
                           (prefix (concat (cdr acc) additional)))
                       (cons (cons (concat prefix suffix) existing)
                             prefix)))
                   stack
                   :initial-value (cons nil "D"))))))

(defvar michelson-type-completion-list
  (list
   (michelson-make-suggest "EXEC" (michelson-forall (arg ret) (arg (lambda arg ret))))
   (michelson-make-suggest "MEM" (michelson-forall (val-type) (val-type (set val-type))))
   (michelson-make-suggest "MEM" (michelson-forall (key-type val-type) (key-type (map key-type val-type))))
   (michelson-make-suggest "UPDATE" (michelson-forall (val-type) (val-type bool (set val-type))))
   (michelson-make-suggest "UPDATE" (michelson-forall (key-type val-type)
                                      (key-type (option val-type) (map key-type val-type))))
   (michelson-make-suggest "MAP" (michelson-forall (lt rt) ((lambda lt rt) (list lt))))
   (michelson-make-suggest "MAP" (michelson-forall (k v b) ((lambda (pair k v) b) (map k v))))
   (michelson-suggest-literals "IF" 'bool)
   (michelson-suggest-literals "LOOP" 'bool)
   (michelson-suggest-literals michelson-comparison-operations 'int)
   'michelson-suggest-comparable
   'michelson-suggest-pairs
   (michelson-suggest-prefix-depth "D" "I" "P")
   (michelson-suggest-prefix-depth "D" "U" "P")
   (lambda (stack) (and (cdr stack)
                        (funcall (michelson-suggest-prefix-depth "PA" "A" "IP") (cdr stack))))
   (michelson-suggest-literals "NOT" 'bool)
   (michelson-suggest-literals '("OR" "AND" "XOR") 'bool 'bool)
   (michelson-suggest-literals "ABS" 'int)
   (michelson-make-suggest
    '("ADD" "SUB" "MUL" "EDIV")
    (lambda (stack)
      (let ((first (car stack))
            (second (cadr stack))
            (intnat '(int nat)))
        (and first
             second
             (memq first intnat)
             (memq second intnat)))))
   (michelson-suggest-reorderable "ADD" 'nat 'timestamp)
   (michelson-suggest-literals "NOT" 'int)
   (michelson-suggest-literals '("OR" "AND" "XOR" "LSL" "LSR") 'nat 'nat)
   (michelson-suggest-literals '("CONCAT") 'string 'string)
   (michelson-suggest-depth '("SOME" "LEFT" "RIGHT") 1)
   (michelson-suggest-literals '("ADD" "SUB") 'tez 'tez)
   (michelson-suggest-reorderable '("ADD" "SUB" "MUL") 'tez 'nat)
   (michelson-suggest-literals "EDIV" 'tez 'nat)
   (michelson-suggest-literals "EDIV" 'tez 'tez)
   (michelson-suggest-literals "IMPLICIT_ACCOUNT" 'key)
   (michelson-suggest-depth "SWAP" 2)
   (michelson-suggest-depth '("DROP" "H") 1)
   (michelson-suggest-literals "CHECK_SIGNATURE" 'key '(pair signature string))
   (michelson-suggest-literals "CREATE_ACCOUNT" 'key '(option key) 'bool 'tez)
   (michelson-make-suggest "IF_NONE" (michelson-forall (x) (option x)))
   (michelson-make-suggest "IF_LEFT" (michelson-forall (x y) (or x y)))
   ;; This is not exactly the type of TRANSFER_TOKENS.
   ;; It will be changed once the concurrency model is worked out
   (michelson-make-suggest "TRANSFER_TOKENS" (michelson-forall (p r g) (p tez (contract p r) g)))
   (michelson-make-suggest
    "CREATE_CONTRACT"
    (michelson-forall (p r g) (key (option key) bool bool tez (lambda (pair p g) (pair r g)) g)))
   (michelson-make-suggest "MANAGER" (michelson-forall (p r) ((contract p r))))
   (michelson-make-suggest "CONS" (michelson-forall (a) (a (list a))))
   (michelson-make-suggest "IF_CONS" (michelson-forall (a) (list a)))
   (michelson-make-suggest "GET" (michelson-forall (k v) (k (map k v))))
   (michelson-make-suggest "UPDATE" (michelson-forall (v) (v bool (set v))))
   (michelson-make-suggest "UPDATE" (michelson-forall (k v) (k (option v) (map k v))))
   (michelson-make-suggest "REDUCE" (michelson-forall (elt b) ((lambda (pair elt b) b) (set elt) b)))
   (michelson-make-suggest "REDUCE" (michelson-forall (key val b) ((lambda (pair (pair key val) b) b) (map key val) b)))
   (michelson-make-suggest "REDUCE" (michelson-forall (a b) ((lambda (pair a b) b) (list a) b)))))

;; Special handling
;; PA+IR


(defun michelson-get-suggestion-list (stack)
  (cl-reduce (lambda (func acc) (append (funcall func stack) acc))
             michelson-type-completion-list
             :from-end t
             :initial-value michelson-suggest-always-available))


(defun michelson-toggle-live-editing ()
  "Toggle `michelson-live-editing'.
Enables or disables stack and error display."
  (interactive)
  (when (and michelson-live-editing
             (get-buffer michelson-output-buffer-name))
    (with-current-buffer michelson-output-buffer-name
      (kill-buffer-and-window)))
  (setq michelson-live-editing (not michelson-live-editing)))

(defvar-local michelson-state nil)

(defun michelson-update-minibuffer-info ()
  (when (nth 2 michelson-state)
    (cancel-timer (nth 2 michelson-state)))
  (setf
   (nth 2 michelson-state)
   (run-at-time
    "0.3 sec" nil
    (lambda (buffer)
      (with-current-buffer buffer
        (setf (nth 2 michelson-state) nil)
        (when (and (not (= (nth 0 michelson-state) (point)))
                   michelson-live-editing)
          (setf (nth 0 michelson-state) (point))
          (michelson-type-at-point))))
    (current-buffer))))

(defun michelson-close-output-buffer ()
  "Close the interactive editing buffer."
  (interactive)
  (let ((buffer (get-buffer michelson-output-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (if window (quit-window t window) (kill-buffer buffer))))))

;;;###autoload
(define-derived-mode michelson-mode prog-mode "Michelson"
  "Major mode for editing Michelson smart contracts."
  (setq-local font-lock-defaults michelson-font-lock-defaults)
  (setq-local indent-line-function 'michelson-indent)
  (setq-local michelson-state (list 0 0 nil))
  (setq-local michelson-cached-buffer-info
              (make-cache :types nil
                          :errors nil))
  (add-hook 'pre-command-hook 'michelson-update-minibuffer-info nil 'local)
  (add-hook 'focus-in-hook 'michelson-update-minibuffer-info nil 'local)
  (add-hook 'post-self-insert-hook 'michelson-clean-cache)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq buffer-file-coding-system 'utf-8-unix)
  (add-hook 'completion-at-point-functions 'michelson-completion-at-point nil 'local)
  (setq-local process-environment
              (cons "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y"
                    (cons "ALPHANET_EMACS=true"
                          (cons "TEZOS_ALPHANET_DO_NOT_PULL=yes"
                                process-environment)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tz\\'" . michelson-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tez\\'" . michelson-mode))

(provide 'michelson-mode)

;;; michelson-mode.el ends here
