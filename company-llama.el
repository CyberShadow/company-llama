(require 'company)
(require 'json)
(require 'url)

(defgroup company-llama nil
  "Completion back-end for Llama"
  :group 'company)

(defcustom company-llama-api-url "http://127.0.0.1:8080/completion"
  "Llama API base url.")

(defun company-llama-fetch (prefix)
  "Fetch completion Candidates from Llama based on given PREFIX."
  (let ((url company-llama-api-url)
    (url-request-extra-headers
      `(("Content-Type" . "application/json")))
    (url-request-method "POST")
    (url-request-data
      (json-encode `(("prompt" . ,prefix)
		     ("n_predict" . 32)
		     ;; ("n_probs" . 5)
		     ("temperature" . 0)
		     ))))
    (url-retrieve-synchronously url nil 5)))

(defun company-llama-parse-response (buffer)
  "Parse the HTTP response in BUFFER."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun company-llama-candidate (data prefix)
  "Return a list of candidates from DATA."
  ;; (let ((choices (cdr (assoc 'choices (assoc 'model data))))
  ;;       (candidate '()))
  ;;   (dolist (choice choices candidate)
  ;;     (setq candidate 
  ;;           (append candidate (split-string 
  ;;       		       (cdr (assoc 'text (assoc 'finishing_choice choice))) "\\.")))))
  (list
   (concat prefix
	   (cdr (assoc 'content data)))))

(defun company-llama-candidates (prefix)
  (let* ((response (company-llama-fetch prefix))
            (data (company-llama-parse-response response)))
       (company-llama-candidate data prefix)))

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
    (prefix (company-llama-prefix))
    (candidates (company-llama-candidates arg))))

(add-to-list 'company-backends 'company-llama-backend)
(setq company-backends (delete 'company-tabnine company-backends))

