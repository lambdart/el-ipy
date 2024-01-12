;;; ipy-cmd.el --- summary -*- lexical-binding: t -*-
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

(require 'python)
(require 'ipy-proc)
(require 'ipy-completion)

(defun ipy-eval-string (str)
  "Eval STR string, i.e, send it to Python comint process."
  (interactive (ipy-util-minibuffer-read 'word "String"))
  ;; eval string symbolic expression
  (ipy-proc-send 'raw nil nil str))

(defun ipy-eval-last-statement ()
  "Send the previous statement to the inferior process."
  (interactive)
  ;; send region of the last statement
  (save-excursion
    (ipy-proc-send 'eval-last-sexp
                   nil
                   t
                   (python-nav-beginning-of-statement)
                   (python-nav-end-of-statement))))

(defun ipy-eval-defun ()
  "Send the previous function definition to the inferior process."
  (interactive)
  (let ((start-point (point)))
    (save-excursion
      (ipy-proc-send 'eval-last-sexp
                     nil
                     t
                     (progn
                       (end-of-line 1)
                       (while (and (or (python-nav-beginning-of-defun)
                                       (beginning-of-line 1))
                                   (> (current-indentation) 0)))
                       (point))
                     (progn
                       (goto-char start-point)
                       (or (python-nav-end-of-defun)
                           (end-of-line 1))
                       (point))))))

(defun ipy-eval-region (beg end)
  "Eval BEG/END region."
  (interactive "r")
  (ipy-proc-send 'eval nil nil beg end))

(defun ipy-eval-current-buffer ()
  "Eval current buffer."
  (interactive)
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (ipy-proc-send 'eval
                     nil
                     nil
                     (point-min)
                     (point-max)))))

(defun ipy-eval-buffer (buffer)
  "Eval target BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (ipy-eval-current-buffer)))

(defun ipy--eval-file-or-buffer (filename)
  "Copy FILENAME contents and eval the temporary buffer."
  (setq ipy-util-prev-l/c-dir/file
        (let ((filename (expand-file-name filename)))
          ;; the user is queried to see if he wants to save
          ;; the buffer before proceeding with the load or compile
          (ipy-util-save-buffer filename)
          ;; get or create the file buffer
          (let ((buffer (get-file-buffer filename)))
            (if buffer
                (ipy-eval-buffer buffer)
              ;; insert buffer contents and call eval buffer operation
              (with-temp-buffer
                (insert-file-contents-literally filename)
                (ipy-eval-current-buffer)))
            ;; cache previous directory/filename
            (cons (file-name-directory filename)
                  (file-name-nondirectory filename))))))

(defun ipy-eval-file (filename)
  "Evaluate target FILENAME content."
  (interactive (ipy-util-read-source-file))
  ;; tries to eval file contest and cache previous local directory
  (ipy--eval-file-or-buffer filename))

(defalias 'ipy-load-file 'ipy-eval-file
  "Load file is more intuitive command name.")

(defun ipy-run-tests ()
  "Invoke Python (run-tests) operation."
  (interactive)
  (ipy-proc-send 'run-tests nil nil ""))

(defun ipy-load-tests-and-run (filename)
  "Load FILENAME and send run-test command."
  (interactive (ipy-util-read-source-file))
  ;; send `eval-buffer' operation with file contents in a temporary buffer
  ;; and wait for it
  (ipy-tq-wait-proc-output ipy-proc-tq
    (ipy--eval-file-or-buffer filename))
  ;; run the tests after "load"
  (ipy-proc-send 'run-tests nil nil ""))

(defun ipy-doc (&optional input)
  "Describe identifier INPUT."
  (interactive (ipy-util-minibuffer-read 'word "Doc"))
  (ipy-proc-send 'doc
                 nil
                 nil
                 (format "pydoc.plain(pydoc.render_doc(%S))" input)))

(defun ipy-find-doc (input)
  "Find INPUT documentation ."
  (interactive (ipy-util-minibuffer-read 'symbol "Find-doc"))
  ;; send find doc operation
  (ipy-proc-send 'find-doc nil nil input))

(defun ipy-apropos (input)
  "Send apropos operation with the arbitrary INPUT."
  (interactive (ipy-util-minibuffer-read 'symbol "Apropos"))
  ;; send apropos command
  (ipy-proc-send 'apropos
                 nil
                 nil
                 (format "pydoc.apropos(%S)" input)))

(defun ipy-complete ()
  "Send complete operation."
  (interactive)
  (ipy-tq-with-live-process ipy-proc-tq
    (ipy-tq-eval-after-handler
        ipy-proc-tq
        ipy-completion-send-cmd
      ipy-completion-list)))

(defun ipy-list-modules ()
  "List all modules."
  (interactive)
  (ipy-proc-send 'ls-modules nil nil "help(\"modules\")"))

(defun ipy-kill-output-buffers ()
  "Kill temporary output buffers."
  (interactive)
  (let ((counter 0))
    (mapc (lambda (buffer)
            (when (string-match-p
                   ipy-util-buffer-name-regex
                   (buffer-name buffer))
              (setq counter (progn
                              (kill-buffer buffer)
                              (1+ counter)))))
          (buffer-list))
    ;; show message
    (and (> counter 0)
         (message ipy-util-kill-buffer-message counter))))

(provide 'ipy-cmd)

;;; ipy-cmd.el ends here
