;;; -*- lexical-binding: t -*-

(require 'company)
(require 'json)
(require 'url)

(defgroup company-llama nil
  "Completion back-end for Llama"
  :group 'company)

(defcustom company-llama-api-url "http://127.0.0.1:8080/completion"
  "Llama API base url.")

(defcustom company-llama-choice-threshold 0.1
  "Probability threshold for displaying a choice.")

(defun company-llama-fetch (prefix callback)
  "Fetch completion Candidates from Llama based on given PREFIX."
  (let ((url company-llama-api-url)
    (url-request-extra-headers
      `(("Content-Type" . "application/json")))
    (url-request-method "POST")
    (url-request-data
      (json-encode `(("prompt" . ,prefix)
		     ("n_predict" . 32)
		     ("n_probs" . ,(min 10 (round (/ 1.0 company-llama-choice-threshold))))
		     ("temperature" . 0)
		     ))))
    (url-retrieve
     url
     (lambda (status)
       (funcall callback))
     nil t)))

(defun company-llama-parse-response ()
  "Parse the HTTP response in BUFFER."
  (goto-char url-http-end-of-headers)
  (json-read))

(defun company-llama-candidate (data prefix)
  "Return a list of candidates from DATA."
  (let* ((s prefix)
	 (tokens-vec (cdr (assoc 'completion_probabilities data)))
	 (tokens (append tokens-vec nil))
	 probs)

    ;; Collect the tokens that the model is sure about.
    (while (and tokens
		;; Get the probs
		(setq probs
		      (cdr (assoc 'probs
				  (car tokens))))
		;; Convert from JSON to a cons cell
		(setq probs
		      (mapcar (lambda (item)
				(cons
				 (cdr (assoc 'tok_str item))
				 (cdr (assoc 'prob item))))
			      probs))
		;; Filter out improbable completions
		(setq probs
		      (seq-filter
		       (lambda (prob)
			 (>= (cdr prob) company-llama-choice-threshold))
		       probs))
		;; Keep going if there is only one option.
		(eq (length probs) 1))
      (setq tokens (cdr tokens))
      (setq s (concat s (car (car probs))))
      (setq probs nil))

    (if probs
	;; Report branches as one candidate each.
	(mapcar
	 (lambda (prob)
	   (concat s (car prob)))
	 probs)
      (list s))))

(defun company-llama-candidates (callback)
  (company-llama-fetch
   (company-llama-prefix)
   (lambda ()
     (message "company-llama-candidates res")
     (let* ((data (company-llama-parse-response))
	    (candidates (company-llama-candidate data "")))
       (funcall callback candidates)))))

(defun company-llama-prefix ()
  (buffer-substring
   (max
    (point-min)
    (- (point) 16384))
   (point)))

(defun company-llama-backend (command &optional arg &rest ignored)
  "Company Mode backend function for Llama.
COMMAND and ARG are as per the `company-backends' API."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-llama-backend))
    (prefix "")
    (candidates (cons :async (lambda (callback)
			       (company-llama-candidates callback))))
    (sorted t)))

(add-to-list 'company-backends 'company-llama-backend)
(setq company-backends (delete 'company-tabnine company-backends))

