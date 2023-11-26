;;; ipy-completion.el --- summary -*- lexical-binding: t -*-
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

(require 'ipy-tq)
(require 'ipy-proc)
(require 'ipy-util)
(require 'ipy-apropos)

(defvar ipy-completions '())
(defvar ipy-completion-thing nil)
(defvar ipy-completion-handler-ends nil)

(defun ipy-completion--append-clean-ns (collection)
  "Append COLLECTION after name space cleanup."
  (append
   (mapcar (lambda (x)
             (replace-regexp-in-string "^.+/" "" x))
           collection)
   collection))

(defun ipy-completion-handler (output-buffer _)
  "Completion OUTPUT-BUFFER handler."
  (setq ipy-completions
        (ipy-completion--append-clean-ns
         (ipy-apropos-collection
          (ipy-util-buffer-content output-buffer ipy-util-eoc)))))

(defun ipy-completion-send-cmd ()
  "Send completion command."
  ;; send apropos operation
  (ipy-tq-with-live-process ipy-proc-tq
    (apply #'ipy-proc-send
           `(apropos
             ipy-completion-handler
             t
             ""))))

(defun ipy-completion-all-completions ()
  "Return list of completions."
  (interactive)
  (ipy-tq-with-live-process ipy-proc-tq
    (ipy-tq-eval-after-handler
        ipy-proc-tq
        ipy-completion-send-cmd
      ipy-completions)))

(defun ipy-completion-completions (_input)
  "Return list of completions."
  ipy-completions)

(provide 'ipy-completion)

;;; ipy-completion.el ends here
