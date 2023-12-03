;;; ipy-tq.el --- summary -*- lexical-binding: t -*-
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

(defvar ipy-tq-proc-eoc-found nil
  "End of command status indicator.")

(defun ipy-tq-queue (tq)
  "Fetch TQ queue.
Looks like (queue proc . eoc)."
  (car tq))

(defun ipy-tq-proc (tq)
  "Fetch TQ process."
  (cadr tq))

(defun ipy-tq-proc-eoc (tq)
  "Fetch TQ process end of command indicator."
  (caddr tq))

(defun ipy-tq-prompt-regexp (tq)
  "Fetch TQ prompt regexp."
  (cadddr tq))

(defun ipy-tq-queue-head-input (tq)
  "Fetch TQ question.
The structure of `queue' is as follows:
\((input regexp closure . (handler . buffer))).
Input: string to send to the process."
  (caar (ipy-tq-queue tq)))

(defun ipy-tq-queue-head-waitp (tq)
  "Fetch TQ wait response predicate."
  (cadar (ipy-tq-queue tq)))

(defun ipy-tq-queue-head-handler (tq)
  "Fetch TQ handler.
Handler: function to call upon receiving a complete response from the process."
  (caddar (ipy-tq-queue tq)))

(defun ipy-tq-queue-head-orig-buffer (tq)
  "Fetch TQ origin buffer."
  (car (cdddar (ipy-tq-queue tq))))

(defun ipy-tq-queue-head-temp-buffer (tq)
  "Fetch TQ temporary buffer.
Buffer: process output buffer."
  (cadr (cdddar (ipy-tq-queue tq))))

(defun ipy-tq-queue-head-wait-handler (tq)
  "Fetch TQ temporary buffer.
Buffer: process output buffer."
  (cdr (cdr (cdddar (ipy-tq-queue tq)))))

(defun ipy-tq-queue-empty-p (tq)
  "Return non-nil if queue (TQ) is empty."
  (not (ipy-tq-queue tq)))

(defmacro ipy-tq-with-live-process (tq &rest body)
  "Evaluate BODY forms if the TQ process is alive."
  (declare (indent 1)
           (debug t))
  `(when (ipy-tq-proc-live-p ,tq)
     ,@body))

(defun ipy-tq-filter-chunk (tq output)
  "Return OUTPUT string after TQ prompt-regexp cleanup."
  (replace-regexp-in-string (ipy-tq-prompt-regexp tq) "" output))

(defun ipy-tq-call-handler (tq)
  "Call TQ function handler."
  (mapc (lambda (fn)
          (funcall fn tq))
        `((lambda (tq)
            (funcall (ipy-tq-queue-head-handler tq)
                     (ipy-tq-queue-head-temp-buffer tq)
                     (ipy-tq-queue-head-orig-buffer tq)))
          (lambda (tq)
            ;; change callback (handler function) ends indicator (state)
            (setcdr (cdr (cdddar (ipy-tq-queue tq))) nil)))))

(defun ipy-tq-proc-filter (tq string)
  "Cache TQ output STRING."
  (mapc (lambda (fn)
          (and fn (funcall fn tq)))
        (if (setq ipy-tq-proc-eoc-found
                  (let ((temp (ipy-tq-filter-chunk tq
                                                   (if (and string
                                                            (stringp string))
                                                       string
                                                     ""))))
                    ;; insert chunk in buffer
                    (ipy-util-insert-chunk
                     (ipy-tq-queue-head-temp-buffer tq) temp)
                    ;; verifies if end of command was found
                    (ipy-util-with-log "process: end of command!" nil
                      (string-match-p (ipy-tq-proc-eoc tq) temp))))
            `((lambda (tq)
                (when (ipy-tq-queue-head-waitp tq)
                  (ipy-tq-call-handler tq)))
              ipy-tq-queue-pop)
          '())))

(defun ipy-tq-queue-add (tq
                         input
                         waitp
                         handler
                         orig-buffer
                         temp-buffer)
  "Add queue element: (INPUT WAIT-RESP-P HANDLER ORIG-BUFFER TEMP-BUFFER)
to the TQ head."
  (prog1 t
    (setcar tq (nconc (ipy-tq-queue tq)
                      `((,input
                         ,waitp
                         ,handler
                         ,orig-buffer
                         ,temp-buffer .
                         t))))))

(defun ipy-tq-wait--proc-loop (tq)
  "Wait TQ process output loop."
  (let ((inhibit-quit nil))
    (with-local-quit
      (let ((proc (ipy-tq-proc tq)))
        (and (process-live-p proc)
             ;; TODO: add timeout here
             (while (and (null ipy-tq-proc-eoc-found)
                         (accept-process-output proc 1 0 t))
               (sleep-for 0.01)))))))

(defmacro ipy-tq-wait-proc-output (tq &rest body)
  "Evaluate BODY forms and force waiting for TQ process output confirmation."
  (declare (indent 1)
           (debug t))
  `(progn
     ,@body
     (ipy-tq-wait--proc-loop ,tq)))

(defmacro ipy-tq-eval-after-handler (tq func &rest body)
  "Evaluate BODY forms after TQ handler that will be called by FUNC ends."
  (declare (indent 2)
           (debug t))
  `(if (not (ipy-tq-proc-live-p ,tq))
       (ipy-util-log "error, missing connection!")
     (mapc  #'funcall
            '(,func
              (lambda ()
                ;; TODO: add timeout here
                (while (eq (ipy-tq-queue-head-wait-handler ,tq) t)
                  (sleep-for 0.01)))))
     ,@body))

(defun ipy-tq--proc-send-input (proc string)
  "Send STRING to PROC stream."
  (if (not (process-live-p proc))
      (ipy-tq-proc-delete proc)
    ;; return true
    (prog1 t
      ;; reset eoc indicator variable
      (setq ipy-tq-proc-eoc-found nil)
      ;; send string to process stream
      (process-send-string proc (concat string "\n")))))

(defun ipy-tq-proc-send-input (tq)
  "Send TQ input using the correct process-send function."
  (and (ipy-tq--proc-send-input
        (ipy-tq-proc tq)
        (ipy-tq-queue-head-input tq))
       ;; call the handler function ASAP if wait is non-nil
       (not (ipy-tq-queue-head-waitp tq))
       (ipy-tq-call-handler tq)))

(defun ipy-tq-queue-head-kill-temp-buffer (tq)
  "Kill temporary output buffer if TQ queue head waitp is non-nil."
  (when (ipy-tq-queue-head-waitp tq)
    (kill-buffer (ipy-tq-queue-head-temp-buffer tq))))

(defun ipy-tq-queue-head-clean-temp-buffer (tq)
  "Delete end of command indicator from TQ temporary buffer."
  (apply 'ipy-util-delete-regexp
         `(,(or (ipy-tq-queue-head-temp-buffer tq) nil)
           ,(ipy-tq-proc-eoc tq))))

(defun ipy-tq-queue-pop (tq)
  "Pop TQ queue element."
  (mapc (lambda (fn)
          (and tq (funcall fn tq)))
        `(ipy-tq-queue-head-kill-temp-buffer
          ;; ipy-tq-queue-head-clean-temp-buffer
          (lambda (tq)
            (setcar tq (cdr (car tq))))
          ;; send the next from the queue
          (lambda (tq)
            (or (ipy-tq-queue-empty-p tq)
                (ipy-tq-proc-send-input tq))))))

(defun ipy-tq-enqueue (tq
                       input
                       waitp
                       handler
                       orig-buffer
                       &optional
                       delay)
  "Add a transaction to transaction queue TQ.
This sends the INPUT string to the process that TQ communicates with.

If WAITP is non-nil we call the HANDLER function ASAP, otherwise we
wait for the response end of command indicator.

The HANDLER is called passing two arguments: the response and
the ORIG-BUFFER.

If DELAY is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes."
  ;; add queue to the transmission queue
  (ipy-tq-queue-add tq
                    input
                    waitp
                    handler
                    orig-buffer
                    (ipy-util-get-buffer-create
                     (generate-new-buffer-name "*ipy-proc-output*") t))
  ;; send queue to process
  (and (or (not delay)
           (not (ipy-tq-queue tq))))
  (ipy-tq-proc-send-input tq))

(defun ipy-tq-proc-live-p (tq)
  "Return non-nil if process in the TQ is alive."
  (process-live-p (ipy-tq-proc tq)))

(defun ipy-tq-proc-delete (tq)
  "Shut down transaction queue TQ terminating the process.
This function always returns nil."
  (prog1 nil
    (ipy-util-with-log "process deleted" t
      (delete-process (ipy-tq-proc tq)))))

(defun ipy-tq-make (process process-eoc &optional prompt-regexp)
  "Set PROCESS filter and return the transaction queue.

PROCESS should be a sub process capable of sending and receiving
streams of bytes. It may be a local process, or it may be connected
to a tcp server on another machine.

PROCESS-EOC is used to indicate the end of command.

PROMPT-REGEXP the prompt regex, is used to clean the response buffer's content."
  (let ((tq `(nil ,process ,process-eoc ,prompt-regexp)))
    (set-process-filter process
                        (lambda (_ s)
                          (ipy-tq-proc-filter tq s)))
    tq))

(provide 'ipy-tq)

;;; ipy-tq.el ends here
