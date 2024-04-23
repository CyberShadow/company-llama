;;; company-llama.el --- Glue between company-mode and llama.cpp   -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/company-llama
;; Package-Requires: ((emacs "24.3") (company "0.9.13"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package implements a company-mode backend which fetches
;; completion candidates from a llama.cpp instance's completions
;; endpoint.

;; For more information, please see the accompanying README.md file.

;;; Code:

(require 'company)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup company-llama nil
  "Completion back-end for Llama"
  :group 'company)

(defcustom company-llama-server-url "http://127.0.0.1:8080/"
  "llama.cpp server URL."
  :type 'string)

(defcustom company-llama-choice-threshold 0.1
  "Probability threshold for displaying a choice."
  :type 'float)

(defcustom company-llama-show-probabilities nil
  "Show probabilities of choices as annotations."
  :type 'boolean)

(defun company-llama--fetch (prefix event-handler)
  "Fetch completion Candidates from Llama based on given PREFIX."
  (let* ((url (concat company-llama-server-url "completion"))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data
          (json-encode `(("prompt" . ,(encode-coding-string prefix 'utf-8))
                         ("stream" . t)
		         ("n_probs" . ,(min 10 (round (/ 1.0 company-llama-choice-threshold))))
		         ("n_predict" . 32)
		         ("temperature" . 0)
		         )))
         (buf (url-retrieve
               url
               (lambda (_status)
                 (funcall event-handler 'done))
               nil t))
         change-queued
         (after-change-hook
          (lambda (_beg _end _len)
            ;; Call via run-with-timer so that url.el can do the
            ;; content-encoding handling first.
            (unless change-queued
              (setq change-queued t)
              (run-with-timer
               0 nil
               (lambda ()
                 (setq change-queued nil)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (funcall event-handler 'change)))))))))
    (with-current-buffer buf
      (add-hook 'after-change-functions after-change-hook nil t)
      (funcall event-handler 'start))))

(defun company-llama--make-data-handler (candidates-callback)
  "Return a lambda which accepts JSON packets from a streaming completion
response.  CANDIDATES-CALLBACK will be called with company-mode candidates."
  (let* ((s ""))
    (lambda (data)
      (if data
          (let* ((content (cdr (assoc 'content data)))
	         (tokens-vec (cdr (assoc 'completion_probabilities data)))
	         (tokens (append tokens-vec nil))
                 ;; An empty content indicates a summary packet
	         (tokens (if (string-empty-p content) nil tokens))
                 done)
            (mapc
             (lambda (token)
               (let* (;; Get the probs
                      (probs (cdr (assoc 'probs token)))
                      ;; Convert from JSON to a cons cell
                      (probs (mapcar (lambda (item)
			               (cons
			                (cdr (assoc 'tok_str item))
			                (cdr (assoc 'prob item))))
			             probs))
		      ;; Filter out improbable completions
		      (likely-probs (seq-filter
		              (lambda (prob)
		                (>= (cdr prob) company-llama-choice-threshold))
		              probs))

                      ;; If all candidates are improbable,
                      ;; just pick the first one
                      (probs (or likely-probs
                                 (list (car probs)))))

                 (cond
                  ;; Already returned a result
                  (done)

                  ;; Only one likely candidate: remember and keep going
                  ((eq (length probs) 1)
                   (setq s (concat s (car (car probs)))))

                  ;; Multiple likely candidates
                  (t
                   (funcall candidates-callback
                            (if (string-empty-p s)
	                        ;; Report branches as one candidate each.
	                        (mapcar
	                         (lambda (prob)
                                   (propertize (car prob)
                                               'llama-probability (cdr prob)))
	                         probs)
                              ;; Otherwise, just return the common prefix.
                              (list s)))
                   (setq done t)))))
             tokens)
            ;; Return nil if we don't want any more data.
            (not done))
        ;; data is nil (EOF):
        (funcall candidates-callback
                            (if (string-empty-p s)
                                nil
                              (list s)))
        nil))))

(defun company-llama--read-one-packet ()
  "Read one text/event-stream JSON packet at point, as a `(type . json)' CONS cell.

Return nil on failure (incomplete or missing data)."
  (condition-case nil
      (save-match-data
        (re-search-forward "^\\([^\n:]*\\): ")
        (cons
         (match-string 1)
         (json-read)))
    (t
     nil)))

(defvar company-llama--active-request nil
  "The currently running request.")

(defun company-llama--disconnect ()
  "Hack - terminate the url.el HTTP connection.

url.el does not support this natively, so we are forced to do
something uncouth here.  Note that disconnecting early from
llama.cpp is critical, so that its resources are freed up for
other completions."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (process-send-eof proc)))
  (let ((buf (current-buffer)))
    (run-with-timer
     0 nil
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let (kill-buffer-query-functions)
             (kill-buffer buf)))))))
  (when (eq company-llama--active-request (current-buffer))
    (setq company-llama--active-request nil)))

(defvar company-llama--counter 0
  "A counter for company completion requests.

Used to know when an ongoing (asynchronous) request is
outdated (and its result will not be useful).")

(defun company-llama--make-url-event-handler (candidates-callback)
  "Return a lambda which accepts events, as invoked by `company-llama--fetch'."

  ;; Disconnect any other ongoing (now obsolete)
  ;; request, to free up the GPU ASAP.
  (when (buffer-live-p company-llama--active-request)
    (with-current-buffer company-llama--active-request
      (company-llama--disconnect)))

  (let* ((counter (cl-incf company-llama--counter))
         (data-handler (company-llama--make-data-handler candidates-callback))
         last-pos done)
    (lambda (event-type)

      ;; Check if this request is still desired.
      (unless done
        (when (not (= counter company-llama--counter))
          ;; Obsoleted.
          (company-llama--disconnect)
          (setq done t)))

      ;; Process new data.
      (unless done
        (setq company-llama--active-request (current-buffer))

        ;; Check if we have headers, and can begin parsing data.
        (when (and (null last-pos)
                   (boundp 'url-http-end-of-headers)
                   url-http-end-of-headers)
          (setq last-pos url-http-end-of-headers))

        ;; Parse new data.
        (when last-pos
          (save-excursion
            (goto-char last-pos)
            (let (packet)
              (while
                  (and
                   (setq packet (company-llama--read-one-packet))
                   (setq last-pos (point))
                   (pcase packet
                     (`("data" . ,data)
                      (if (funcall data-handler data)
                          ;; OK, keep going
                          t
                        ;; data-handler is done, stop
                        (setq done t)
                        (company-llama--disconnect)
                        nil))

                     (`("error" . ((content . "slot unavailable")))
                      ;; Retry
                      (run-with-timer
                       0.1 nil
                       (lambda ()
                         (when (= counter company-llama--counter)
                           (company-llama--candidates candidates-callback))))
                      (setq done t)
                      (company-llama--disconnect))

                     (`("error" . ((content . ,other-error)))
                      (setq done t)
                      (company-llama--disconnect)
                      (error "llama server error: %s" other-error))

                     (`(,other-type . ,data)
                      (setq done t)
                      (company-llama--disconnect)
                      (error "Unknown packet type from llama server: %s: %S" other-type data)))))))))

      ;; Process disconnects.
      (unless done
        (when (eq event-type 'done)
          (funcall data-handler nil)
          (company-llama--disconnect)
          (setq done t))))))

(defun company-llama--prefix ()
  "Return the prefix (context) used for the completion."
  (buffer-substring
   (max
    (point-min)
    (- (point) 16384))
   (point)))

(defun company-llama--candidates (candidates-callback)
  "Company \"candidates\" command implementation."
  (company-llama--fetch
   (company-llama--prefix)
   (company-llama--make-url-event-handler candidates-callback)))

(defun company-llama--annotation (candidate)
  "Company \"annotation\" command implementation."
  (let ((probability (get-text-property 0 'llama-probability candidate)))
    (when (and probability company-llama-show-probabilities)
      (format "\t\t\t[%.2f%%]" (* 100 probability)))))

(defun company-llama-backend (command &optional arg &rest _ignored)
  "Company Mode backend function for Llama.
COMMAND and ARG are as per the `company-backends' API."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-llama-backend))
    (prefix (cons "" (length (company-llama--prefix))))
    (candidates (cons :async #'company-llama--candidates))
    (annotation (company-llama--annotation arg))
    (sorted t)))

(defun company-llama-healthy-p ()
  "Ask the llama.cpp server if it is healthy, and return t if so."
  (with-current-buffer
      (url-retrieve-synchronously (concat company-llama-server-url "health"))
    (= (url-http-parse-response) 200)))

(provide 'company-llama)
;;; company-llama.el ends here
