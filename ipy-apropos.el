;;; ipy-apropos.el --- summary -*- lexical-binding: t -*-
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

(require 'button)
(require 'ipy-util)

(defvar ipy-apropos-buffer-name "*ipy-apropos-output*"
  "Apropos buffer name.")

(defvar ipy-apropos-buffer nil
  "Apropos temporary buffer.")

(defvar ipy-apropos-collection '()
  "Apropos cached collection.")

(defvar ipy-apropos-cache-flag nil
  "Non-nil means cache collection flag.")

(define-button-type 'ipy-apropos-button
  'face
  'font-lock-keyword-face
  'help-echo "mouse-2, RET: Display documentation"
  'follow-link t
  'action (lambda (button)
            (and (fboundp 'ipy-doc)
                 (funcall 'ipy-doc (button-get button 'label)))))

(defun ipy-apropos-buffer ()
  "Return apropos buffer."
  (setq ipy-apropos-buffer
        (or (and (buffer-live-p ipy-apropos-buffer)
                 ipy-apropos-buffer)
            (with-current-buffer (ipy-util-get-buffer-create
                                  ipy-apropos-buffer-name
                                  nil)
              (setq-local buffer-read-only t)
              (current-buffer)))))

(defun ipy-apropos-insert-button (label doc)
  "Insert button with LABEL and DOC."
  (when (and (stringp label)
             (not (string-empty-p label)))
    (prog1 nil
      (insert-button label 'label label :type 'ipy-apropos-button)
      (insert (format "\nDoc:%s\n\n" (or doc ""))))))

(defun ipy-apropos-collection (output)
  "Parse OUTPUT to a collection of string elements."
  (and (stringp output)
       (sort (split-string output "[\n]") #'string<)))

(defun ipy-apropos-handler (output-buffer _)
  "Apropos OUTPUT-BUFFER operation handler."
  (save-excursion
    (let ((inhibit-read-only t)
          (buffer (ipy-apropos-buffer)))
      (display-buffer buffer)
      (ipy-util-with-buffer-content output-buffer t
        (with-current-buffer buffer
          (erase-buffer)
          (mapc (lambda (intput)
                  (let ((item (split-string intput "[-]")))
                    (ipy-apropos-insert-button (string-trim (car item))
                                               (cadr item))))
                (ipy-apropos-collection content))
          (goto-char (point-min)))))))

(provide 'ipy-apropos)

;;; ipy-apropos.el ends here
