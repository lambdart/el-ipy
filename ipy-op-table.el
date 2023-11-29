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

(defconst ipy-op-doc-setup
  "import pydoc")

;; from python.el
(defconst ipy-op-eval-setup
  "\
def __PYTHON_EL_eval(source, filename):
    import ast, sys
    if sys.version_info[0] == 2:
        from __builtin__ import compile, eval, globals
    else:
        from builtins import compile, eval, globals
    try:
        p, e = ast.parse(source, filename), None
    except SyntaxError:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)
        return
    if p.body and isinstance(p.body[-1], ast.Expr):
        e = p.body.pop()
    try:
        g = globals()
        exec(compile(p, filename, 'exec'), g, g)
        if e:
            return eval(compile(ast.Expression(e.value), filename, 'eval'), g, g)
    except Exception:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)"
  "Code used to evaluate statements in inferior Python processes.")

(defvar ipy-op-setups '(ipy-op-doc-setup
                        ipy-op-eval-setup))

(defvar ipy-op-eldoc ""
  "Eldoc operation format.")

(defvar ipy-op-table
  `((raw            . (:cf "%s"))
    (eval           . (:cf "__PYTHON_EL_eval(%s, %s)" :pf t))
    (eval-last-sexp . (:cb ipy-eval-handler
                           :cf "__PYTHON_EL_eval(%s, %s)"
                           :pf t
                           :wp t))
    (doc            . (:cf "%s"
                           :cb ipy-doc-handler
                           :wp t
                           :pf nil))
    (find-doc       . (:cf ""))
    (run-tests      . (:cf ""))
    (ls-modules     . (:cf "__PYTHON_EL_eval(%s, %s)" :pf t))
    (eldoc          . (:cb ipy-eldoc-handler :cf ,ipy-op-eldoc))
    (apropos        . (:cb ipy-apropos-handler
                       :cf "%s"
                       :wp t
                       :pf nil)))
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

