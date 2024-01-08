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

(defvar ipy-completion-list '()
  "Cached completion at point list.")

(defvar ipy-completion-thing-at-point '()
  "Cached completion thing at point (beg end).")

(defun ipy-completion-handler (output-buffer _)
  "Completion OUTPUT-BUFFER handler."
  (setq ipy-completion-list
        (let ((temp (ipy-util-buffer-content output-buffer ipy-util-eoc)))
          (string-split
           (string-trim (replace-regexp-in-string "[]['(]" "" temp))
           "," t "[:blank: ]"))))

(defun ipy-completion-send-cmd ()
  "Send completion command."
  ;; send apropos operation
  (ipy-tq-with-live-process ipy-proc-tq
    (let ((input (ipy-util-thing-at-point)))
      (progn
        (setq ipy-completion-thing-at-point input)
        (apply #'ipy-proc-send `(complete nil nil ,@input))))))

(provide 'ipy-completion)

;;; ipy-completion.el ends here
