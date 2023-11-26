;;; ipy-op-table.el --- summary -*- lexical-binding: t -*-
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

(defvar ipy-op-eldoc-fmt
  ""
  "Eldoc operation format.")

(defvar ipy-modules-list-fmf
  ""
  "Return list all of available modules.")

(defvar ipy-op-table
  `((input          . (:cf "%s"))
    (eval           . (:cf "%s"))
    (eval-last-sexp . (:cb ipy-eval-handler :cf "%s" :wp t))
    (doc            . (:cf ""))
    (find-doc       . (:cf ""))
    (run-tests      . (:cf ""))
    (eldoc          . (:cb ipy-eldoc-handler :cf ,ipy-op-eldoc-fmt))
    (apropos        . (:cb ipy-apropos-handler
                           :cf ""
                           :wp t))
    (source         . (:cf ""))
    ;; (meta           . (:cf ""))
    )
  "Operation associative list: (OP-KEY . (OP-PLIST))
OP-KEY, the operation key selector.
OP-PLIST, response handler, operation format string and
dedicated output buffer.")

(defun ipy-op-table-get-plist (op-key)
  "Get the property list from the operation OP-KEY table."
  (cdr (assoc op-key ipy-op-table))) ; select operation

(defun ipy-op-table-get-property (op-key property)
  "Get property list element from the operation table.
OP-KEY, operation key property list selector.
PROPERTY, possible values are: :cb, and :cf."
  (plist-get (ipy-op-table-get-plist op-key) property))

(provide 'ipy-op-table)

;;; ipy-op-table.el ends here

