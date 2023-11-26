;;; ipy-eldoc.el --- summary -*- lexical-binding: t -*-
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

(require 'subr-x)
(require 'ipy-tq)
(require 'ipy-proc)

(eval-when-compile
  (require 'ipy-util))

(defvar ipy-eldoc-thing ""
  "Cache eldoc thing at point.")

(defvar ipy-eldoc-meta-data nil
  "Cache eldoc meta data information.")

(defvar ipy-eldoc-callback nil
  "Cache eldoc callback function.")

(defun ipy-eldoc-beginning-of-sexp ()
  "Move to the beginning of current sexp.
Return the number of nested sexp the point was over or after."
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun ipy-eldoc-fnsym ()
  "Return function symbol from current sexp."
  (save-excursion
    (save-restriction
      ;; goes to the beginning of the s-expression
      (ipy-eldoc-beginning-of-sexp)
      ;; if current word is inside a string, vector,
      ;; hash or set literal return an empty string, tries
      ;; to return the symbol at point otherwise
      (if (member (or (char-after (1- (point))) 0) '(?\" ?\{ ?\[))
          "" ;; return empty string
        (or (thing-at-point 'symbol t) "")))))

(defun ipy-eldoc-docstring (string)
  "Return format documentation STRING."
  (string-trim (replace-regexp-in-string "[\t\n\r]" "" string)))

(defun ipy-eldoc-display-docstring (meta-data)
  "Display eldoc documentation string.
Parse the documentation string and its META-DATA,
using the CALLBACK function."
  (when-let* ((fnsym (car-safe meta-data))
              (args (cadr meta-data))
              (docstring (caddr meta-data)))
    (funcall ipy-eldoc-callback
             (concat args " " (ipy-eldoc-docstring docstring))
             :thing fnsym
             :face font-lock-function-name-face)))

(defun ipy-eldoc-cmd-send (input)
  "Invoke \\{ipy-eldoc} operation.
INPUT, clojure symbol string that'll be extract the
necessary metadata."
  (and (stringp input)
       (not (string-empty-p input))
       (ipy-proc-send 'eldoc nil t input)))

(defun ipy-eldoc-function (callback &rest _ignored)
  "Python documentation function.
Each hook function is called with at least one argument CALLBACK,
a function, and decides whether to display a doc short string
about the context around point."
  (setq ipy-eldoc-callback (or (and (functionp callback)
                                     callback)
                                (lambda (&rest _) nil))
        ipy-eldoc-thing
        (ipy-tq-with-live-process ipy-proc-tq
          (let ((thing (ipy-eldoc-fnsym)))
            (if (string= thing ipy-eldoc-thing)
                (ipy-eldoc-display-docstring ipy-eldoc-meta-data)
              ;; send command send eldoc
              (ipy-eldoc-cmd-send thing))
            ;; always cache thing will be used in next interaction
            thing)))
  ;; the hook function should return a non-nil, non-string
  0)

(defun ipy-eldoc-handler (output-buffer _)
  "Handler Eldoc OUTPUT-BUFFER to print/display the documentation."
  (ipy-eldoc-display-docstring
   (ipy-util-with-buffer-content output-buffer t
     (setq ipy-eldoc-meta-data
           (let ((meta-data (read (or content "()"))))
             (and (consp meta-data) meta-data))))))

(defun ipy-eldoc-enable ()
  "Enable eldoc operation."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'ipy-eldoc-function nil t))

(defun ipy-eldoc-disable ()
  "Disable eldoc operation."
  (interactive)
  (remove-hook 'eldoc-documentation-functions #'ipy-eldoc-function t))

(provide 'ipy-eldoc)

;;; ipy-eldoc.el ends here
