;;; ipy-proc.el --- summary -*- lexical-binding: t -*-
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
(require 'ipy-util)
(require 'ipy-op-table)

(defvar ipy-proc-tq nil
  "Default tq instance.")

(defvar ipy-proc-hooks nil
  "Process hooks to be called after the process is initialized.")

(defvar ipy-proc-subprompt-regexp ""
  "Regexp to recognize subprompts in the Inferior Python mode.")

(defvar ipy-proc-prompt-regexp ">>> "
  "Regexp to recognize both main prompt and subprompt for comint.")

(defvar ipy-proc-stream-name
  " *ipy-proc-repl-stream*"
  "Default repl stream name.")

(defvar ipy-proc-default-command "python")

(defcustom ipy-proc-env '("")
  "Python process environment."
  :group 'ipy-mode
  :safe 'consp
  :type '(set string))

(defcustom ipy-proc-virtualenv-root nil
  "Path to virtualenv root.
This variable, when set to a string, makes the environment to be
modified such that shells are started within the specified
virtualenv."
  :group 'ipy-mode
  :type '(choice (const nil) directory))

(defun ipy-proc--environment ()
  "Append the `ipy-proc-env' with `process-environment'."
  (append ipy-proc-env process-environment))

(defun ipy-proc--command ()
  "Return Python process command."
  (list (if (not ipy-proc-virtualenv-root)
            ipy-proc-default-command
          (concat
           (expand-file-name "bin/" ipy-proc-virtualenv-root)
           ipy-proc-default-command))))

(defun ipy-proc--open-stream ()
  "Create process repl stream."
  (let ((process-environment (ipy-proc--environment)))
    (make-process :name "ipy-repl"
                  :buffer (get-buffer-create "*ipy-proc-output-log*")
                  :command (ipy-proc--command)
                  :coding nil
                  :connection-type 'pty)))

(defun ipy-proc--open-network-stream (host port)
  "Open network-stream using HOST/PORT."
  (open-network-stream ipy-proc-stream-name
                       nil
                       host
                       port
                       :return-list nil
                       :type 'network
                       :nogreeting t
                       :nowait nil))

(defun ipy-proc--default-handler (output-buffer &optional _)
  "Switch to OUTPUT-BUFFER (the process output buffer)."
  (save-mark-and-excursion
    (when (buffer-live-p output-buffer)
      (display-buffer output-buffer))))

(defun ipy-proc--format-append-eoc (cmd)
  "Return CMD with end of command indicator."
  (format "%s\r\nprint(%S)" cmd ipy-util-eoc))

(defun ipy-proc--parse-json-input (input cmd-fmt)
  "Format INPUT (string or region) with CMD-FMT (command format)."
  (let* ((text (if (not (stringp (car input)))
                   (apply 'buffer-substring-no-properties input)
                 (car input))))
    (ipy-proc--format-append-eoc
     (format cmd-fmt
             (ipy-util-encode-text text)
             (ipy-util-encode-text (or (buffer-file-name) "<string>"))))))

(defun ipy-proc--parse-input (input cmd-fmt)
  "Format INPUT (string or region) with CMD-FMT (command format)."
  (ipy-proc--format-append-eoc
   (apply 'format
          (append `(,cmd-fmt
                    ,@(if (not (stringp (car input)))
                          (list (apply 'buffer-substring-no-properties input))
                        input))))))

(defun ipy-proc--ensure-connection ()
  "Return cached transmission queue or create the connection."
  (or
   (setq ipy-proc-tq
         (if (and ipy-proc-tq
                  (not (ipy-tq-proc-live-p ipy-proc-tq)))
             nil
           ipy-proc-tq))
   ;; needs user interaction
   (call-interactively 'ipy-proc-connect)))

(defun ipy-proc-send (op-key handler waitp &rest input)
  "Send the operation defined by OP-KEY.
HANDLER, function that will be called by the transmission queue with the
output buffer.

WAITP, when non-nil wait the end of command indicator before call the proper
handler.

INPUT, the string or the region bounds."
  (when (ipy-proc--ensure-connection)
    (ipy-tq-enqueue ipy-proc-tq
                    ;; parsed command plus input
                    (let ((cmd-fmt (ipy-op-table-get-property op-key :cf))
                          (cmd-pfn (ipy-op-table-get-property op-key :pf)))
                      (apply #'funcall `(,(if cmd-pfn
                                              'ipy-proc--parse-json-input
                                            'ipy-proc--parse-input)
                                         ,input
                                         ,cmd-fmt)))
                    ;; wait predicate
                    (or waitp (ipy-op-table-get-property op-key :wp))
                    ;; callback handler
                    (or handler
                        (ipy-op-table-get-property op-key :cb)
                        'ipy-proc--default-handler)
                    ;; source buffer
                    (current-buffer)
                    ;; delay flag
                    t)))

;;;###autoload
(defun ipy-proc-connect (host port)
  "Connect to Python REPL Server using the (HOST PORT) parameters."
  (interactive (list
                (ipy-util-read-host)
                (ipy-util-read-port ipy-util-port)))
  (cond ((string= host "")
         (ipy-util-log "error: missing host value!"))
        ;; default: tries open host/port stream and cache it
        (t (condition-case err
               (setq ipy-proc-tq
                     (ipy-tq-make (ipy-proc--open-network-stream host port)
                                  ipy-util-eoc
                                  ipy-proc-prompt-regexp))
             ;; handle errors
             (error (ipy-util-log (concat "error: " (cadr err)) nil))
             ;; handle success
             (:success
              (prog1 ipy-proc-tq
                (ipy-util-log "success: TQ connected!"
                    ;; maybe cache host to be used again
                    (unless (member host ipy-util-host-history)
                      (push host ipy-util-host-history))
                  ;; run hooks
                  (run-hooks 'ipy-proc-hooks))))))))

;;;###autoload
(defun ipy-proc-disconnect ()
  "Disconnect from transmission queue."
  (interactive)
  (setq ipy-proc-tq
        (prog1
            ;; erase if necessary
            (and ipy-proc-tq (ipy-tq-proc-delete ipy-proc-tq))
          ;; just a hint to the user
          (ipy-util-log "TQ disconnected!" t))))

;;;###autoload
(defun ipy-proc-restart-connection ()
  "Restart the connection."
  (interactive)
  (progn
    (ipy-proc-disconnect)
    (let ((host (car ipy-util-host-history))
          (port ipy-util-port))
      (ipy-proc-connect host port))))

;;;###autoload
(defun ipy-proc-start ()
  "Run Python process."
  (interactive)
  (condition-case err
      (setq ipy-proc-tq
            (ipy-tq-make (ipy-proc--open-stream)
                         ipy-util-eoc
                         ipy-proc-prompt-regexp))
    ;; handle errors
    (error (ipy-util-log (concat "error: " (cadr err)) nil))
    ;; handle success
    (:success
     (prog1 ipy-proc-tq
       (ipy-util-log "success: python process created!"
           (dolist (op ipy-op-setups)
             (ipy-proc-send 'raw nil nil (symbol-value op)))
         ;; run hooks
         (run-hooks 'ipy-proc-hooks))))))

;;;###autoload
(defun ipy-proc-stop ()
  "Stop from transmission queue."
  (interactive)
  (setq ipy-proc-tq
        (prog1
            ;; erase if necessary
            (and ipy-proc-tq
                 (ipy-tq-proc-delete ipy-proc-tq)))))

;;;###autoload
(defun ipy-proc-restart ()
  "Restart python process."
  (interactive)
  (mapc #'funcall
        '(ipy-proc-stop
          ipy-proc-start)))

(defun ipy-proc-tq-pop ()
  "Pop queue."
  (interactive)
  (ipy-tq-queue-pop ipy-proc-tq))

(provide 'ipy-proc)

;;; ipy-proc.el ends here
