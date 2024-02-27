;;; -*- lexical-binding: t -*-

(require 'company)
(require 'json)
(require 'url)

(defgroup company-llama nil
  "Completion back-end for Llama"
  :group 'company)

(defcustom company-llama-api-url "http://127.0.0.1:8080/completion"
  "Llama API completion URL."
  :type 'float)

(defcustom company-llama-choice-threshold 0.1
  "Probability threshold for displaying a choice."
  :type 'float)

(defun company-llama--fetch (prefix event-handler)
  "Fetch completion Candidates from Llama based on given PREFIX."
  (let* ((url company-llama-api-url)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data
          (json-encode `(("prompt" . ,prefix)
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
      (add-hook 'after-change-functions after-change-hook nil t))))

(defun company-llama--make-data-handler (candidates-callback)
  "Return a lambda which accepts JSON packets from a streaming completion
response.  CANDIDATES-CALLBACK will be called with company-mode candidates."
  (let* ((s ""))
    (lambda (data)
      (if data
          (let* ((tokens-vec (cdr (assoc 'completion_probabilities data)))
	         (tokens (append tokens-vec nil))
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
		      (probs (seq-filter
		              (lambda (prob)
		                (>= (cdr prob) company-llama-choice-threshold))
		              probs)))

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
	                           (car prob))
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

(defun company-llama--disconnect ()
  "Hack - terminate the url.el HTTP connection.

url.el does not support this natively, so we are forced to do
something uncouth here.  Note that disconnecting early from
llama.cpp is critical, so that its resources are freed up for
other completions."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (process-send-eof proc))
  (let ((buf (current-buffer)))
    (run-with-timer
     0 nil
     (lambda ()
       (with-current-buffer buf
         (let (kill-buffer-query-functions)
           (kill-buffer buf)))))))

(defvar company-llama--counter 0
  "A counter for company completion requests.

Used to know when an ongoing (asynchronous) request is
outdated (and its result will not be useful).")

(defun company-llama--make-url-event-handler (candidates-callback)
  "Return a lambda which accepts events, as invoked by `company-llama--fetch'."
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

(defun company-llama-backend (command &optional _arg &rest _ignored)
  "Company Mode backend function for Llama.
COMMAND and ARG are as per the `company-backends' API."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-llama-backend))
    (prefix (cons "" (length (company-llama--prefix))))
    (candidates (cons :async #'company-llama--candidates))
    (sorted t)))

(provide 'company-llama)
