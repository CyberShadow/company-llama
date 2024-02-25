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
         (after-change-hook (lambda (_beg _end _len)
                              (funcall event-handler 'change))))
    (with-current-buffer buf
      (add-hook 'after-change-functions after-change-hook nil t))))

(defun company-llama--make-data-handler (candidates-callback)
  (let* ((s ""))
    (lambda (data)
      ;; (message "Got JSON: %S" data)
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
                              (list s)))))))

(defun company-llama--make-url-event-handler (candidates-callback)
  (let* ((data-handler (company-llama--make-data-handler candidates-callback))
         last-pos done)
    (lambda (event-type)
      (unless done
        (cl-case event-type
          (change
           ;; Check if we have headers, and can begin parsing data.
           (when (and (null last-pos)
                      (boundp 'url-http-end-of-headers)
	              url-http-end-of-headers)
             (setq last-pos url-http-end-of-headers))

           ;; Parse new data.
           (when last-pos
             (save-excursion
               (save-match-data
                 (while
                     (condition-case nil
                         (progn
                           (goto-char last-pos)
                           (re-search-forward "data: ")
                           (let ((json (json-read)))
                             (if (funcall data-handler json)
                                 t
                               (setq done t)
                               (interrupt-process)
                               nil)))
                       (t
                        nil))
                   (setq last-pos (point)))))))
          (done
           (funcall data-handler nil)
           (setq done t)))))))

(defun company-llama--prefix ()
  (buffer-substring
   (max
    (point-min)
    (- (point) 16384))
   (point)))

(defun company-llama--candidates (candidates-callback)
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
