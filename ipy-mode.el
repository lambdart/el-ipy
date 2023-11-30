;;; ipy-mode.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://codeberg.org/lambdart/ipy
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

(require 'ipy-cmd)
(require 'ipy-proc)
(require 'ipy-eval)
(require 'ipy-eldoc)
(require 'ipy-overlay)
(require 'ipy-completion)

(defvar ipy-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (pair)
            (apply 'define-key
                   map
                   (kbd (car pair))
                   (cdr pair)))
          `(("C-c C-e" ipy-eval-last-sexp)
            ("C-c C-r" ipy-eval-region)
            ("C-c C-c" ipy-eval-current-buffer)
            ("C-c C-b" ipy-eval-buffer)
            ("C-c C-d" ipy-doc)
            ("C-c k"   ipy-kill-output-buffers)
            ("C-c C-k" ipy-kill-output-buffers)
            ("C-c C-f" ipy-find-doc)
            ("C-c C-t" ipy-run-tests)
            ("C-c p"   ipy-proc-restart)
            ("C-c C-p" ipy-proc-restart)
            ("C-c x"   ipy-proc-restart-connection)
            ("C-c C-x" ipy-proc-restart-connection)
            ("<f5>"    ipy-run-tests)
            ("<f6>"    ipy-load-tests-and-run)
            ("C-c a"   ipy-apropos)
            ("C-c C-a" ipy-apropos)
            ("C-c l"   ipy-load-file)
            ("C-c C-l" ipy-load-file)))
    map)
  "Python REPL commands (or operations) keymap.")

(defun ipy-define-menu ()
  "Define the minor mode menu."
  (easy-menu-define ipy-mode-menu ipy-mode-map
    "Ipy Minor Mode Menu"
    '("IPY"
      ["Eval region" ipy-eval-region t]
      ["Eval buffer" ipy-eval-current-buffer t]
      ["Eval function" ipy-eval-defn t]
      ["Eval last sexp" ipy-eval-last-sexp t]
      "--"
      ["Load file" ipy-load-file t]
      "--"
      ["Doc" ipy-doc t]
      ["Apropos" ipy-apropos t]
      ["Find-Doc" ipy-find-doc t]
      "--"
      ["Quit REPL" ipy-comint-quit])))

;; avoid warning
(defvar ipy-mode nil)

;;;###autoload
(defun ipy-mode-state ()
  "Show \\{ipy-mode} state, i.e: on or off."
  (interactive)
  (message "ipy-mode %s" (if ipy-mode "on" "off")))

(defun ipy-mode-setup (functions)
  "Call setup FUNCTIONS list (enable or disable functions)."
  (mapc (lambda (f) (funcall f)) functions))

;;;###autoload
(define-minor-mode ipy-mode
  "Minor mode for interacting with the Python REPL.

If called interactively, toggle \\[ipy-mode].  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode. Enable the mode if ARG is nil,
omitted, or is a positive number. Disable the mode if
ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{ipy-mode-map}"

  :lighter ""
  :keymap ipy-mode-map
  (ipy-mode-setup
   (if ipy-mode
       '(ipy-define-menu
         ipy-eldoc-disable
         ipy-overlay-enable)
     '(ipy-eldoc-disable
       ipy-overlay-disable))))

(provide 'ipy-mode)

;;; ipy-mode.el ends here
