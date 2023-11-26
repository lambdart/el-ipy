;;; ipy-repl.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/ipy
;; Version: 0.0.1 Alpha
;; Keywords:
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2023 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

(require 'ipy-util)
(require 'ipy-proc)

(defgroup ipy-repl nil
  "IPY REPL utilities."
  :prefix "ipy-repl-"
  :group 'ipy-repl)

;; (defcustom ipy-repl-prompt-read-only t
;;   "If non-nil, the comint buffer will be read-only."
;;   :group 'ipy-repl
;;   :type 'boolean)

(defcustom ipy-repl-buffer-name "*CLJ-REPL*"
  "Python REPL comint buffer name."
  :group 'ipy-repl
  :type 'string)

(defcustom ipy-repl-prompt-regexp "^\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize prompt."
  :group 'ipy-repl
  :type 'regexp)

(defcustom ipy-repl-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "Python comint input filter regex."
  :group 'ipy-repl
  :type 'regexp)

(defcustom ipy-repl-output-timeout 15
  "Comint accept output timeout in seconds."
  :group 'ipy-repl
  :type 'integer)

(defvar ipy-repl-buffer nil
  "Comint process buffer.")

(defvar ipy-repl-output-cache nil
  "Comint output filtered text list.")

(defun ipy-repl-handler (output-buffer _source-buffer)
  "Get the response from OUTPUT-BUFFER and show in the SOURCE-BUFFER."
  (ipy-util-with-buffer-content output-buffer t
                                (prin1 content)))

(defun ipy-repl-grab-input-at-point ()
  "Grab input at point and filter it."
  (ipy-repl-input-filter
   (let ((input "(+ 1 1)"))
     input)))

(defun ipy-repl-send ()
  "Get input filter it and send it to be evaluated."
  (interactive)
  (ipy-proc-send 'eval
                 'ipy-repl-handler
                 nil
                 (ipy-repl-grab-input-at-point)))

(defun ipy-repl-input-filter (string)
  "Don't save anything on the STRING matching `ipy-repl-filter-regexp'."
  (not (string-match-p ipy-repl-filter-regexp string)))

(defun ipy-repl-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun ipy-repl-setup ()
  "Helper function to setup `comint-mode' related variables."
  ;; setup buffer local variables
  (setq-local window-point-insertion-type t
              font-lock-defaults '(nil t))
  ;;
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) ipy-repl-prompt-regexp))

(defvar ipy-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (x)
            (apply 'define-key map x))
          `((,(kbd "RET") #'ipy-repl-input)
            (,(kbd "C-j") #'ipy-repl-send)))
    ;; return key map
    map)
  "Extended Comint Mode Map.")

(define-derived-mode ipy-repl-mode fundamental-mode "CLJ-REPL"
  "Major mode for `ipy-repl' REPL buffer.

Runs a Python interpreter with the help of `comint-mode',
use the buffer abstraction as the main I/O bridge between
Emacs and the inferior process.

The following commands are available:

\\{ipy-repl-mode-map}"
  :group 'ipy-repl
  ;; set local paragraph variables
  (ipy-repl-setup))

(defun ipy-repl-run ()
  (interactive)
  ;; create buffer
  ;; setup local variables
  ;; setup font lock
  ;; setup prompt and text properties
  ;; display buffer
  )

(provide 'ipy-repl)
;;; ipy-repl.el ends here
