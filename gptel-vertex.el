;;; gptel-vertex.el --- Google Cloud Vertex AI support for gptel  -*- lexical-binding: t; -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/gptel-vertex
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0"))

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (gptel "0.9"))
;; Keywords: ai, llm, google, gcp, vertex, claude, gemini
;; URL: https://github.com/yourusername/gptel-vertex

;;; Commentary:
;;
;; This package adds support for Google Cloud Vertex AI to gptel.

;;; Code:

(require 'gptel)
(require 'cl-generic)
(require 'map)
(eval-when-compile (require 'cl-lib))

(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json")
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--base64-encode "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--insert-file-string "gptel")
(declare-function gptel--json-read "gptel")
(defvar json-object-type)

;;; Customization
(defgroup gptel-vertex nil
  "Google Cloud Vertex AI support for gptel."
  :group 'gptel)

(defcustom gptel-vertex-default-location "us-east5"
  "Default GCP region for Vertex AI.
For Claude models, us-east5 is currently the primary region."
  :type 'string
  :group 'gptel-vertex)

(defcustom gptel-vertex-token-refresh-seconds 3500
  "Seconds before token expiry to refresh (tokens last 3600 seconds)."
  :type 'integer
  :group 'gptel-vertex)

;;; Vertex AI backend struct
(cl-defstruct
    (gptel-vertex (:constructor gptel--make-vertex)
                  (:copier nil)
                  (:include gptel-backend))
  "A backend for Google Cloud Vertex AI.
Extends the base gptel-backend with Vertex-specific features."
  project-id
  location
  access-token
  token-expiry
  publisher)  ;; Track whether this is "anthropic" or "google"

;;; Authentication
(defun gptel-vertex--get-access-token ()
  "Get or refresh the GCP access token using gcloud CLI.
Returns the access token string."
  (let ((result (string-trim
                 (shell-command-to-string "gcloud auth print-access-token"))))
    (if (string-match-p "ERROR" result)
        (error "Could not get GCP access token. Ensure you are logged in with 'gcloud auth login': %s" result)
      result)))

(cl-defmethod gptel-vertex--ensure-auth ((backend gptel-vertex))
  "Ensure BACKEND has a valid access token.
Refreshes the token if it's expired or missing."
  (let ((now (current-time)))
    (when (or (not (gptel-vertex-access-token backend))
              (not (gptel-vertex-token-expiry backend))
              (time-less-p (gptel-vertex-token-expiry backend) now))
      ;; Refresh token - GCP tokens expire after 1 hour
      (let ((token (gptel-vertex--get-access-token)))
        (setf (gptel-vertex-access-token backend) token
              (gptel-vertex-token-expiry backend)
              (time-add now (seconds-to-time gptel-vertex-token-refresh-seconds)))))
    (gptel-vertex-access-token backend)))

;;; Request handling
(cl-defmethod gptel--request-data ((backend gptel-vertex) prompts)
  "JSON encode PROMPTS for sending to Vertex AI."
  (let* ((model-name (gptel--model-name gptel-model))
         (is-claude (string-match-p "claude" model-name))
         (publisher (if is-claude "anthropic" "google")))

    ;; Update publisher in backend dynamically based on current model
    (setf (gptel-vertex-publisher backend) publisher)

    (if is-claude
        ;; Claude request format via Vertex
        (let ((messages
               (cl-loop for prompt across prompts
                        for role = (plist-get prompt :role)
                        for content = (plist-get prompt :content)
                        ;; Handle different content formats
                        for text = (cond
                                    ;; Direct text content
                                    ((stringp content) content)
                                    ;; Parts format (array of parts)
                                    ((plist-get prompt :parts)
                                     (let ((parts (plist-get prompt :parts)))
                                       (if (vectorp parts)
                                           (plist-get (aref parts 0) :text)
                                         (plist-get (car parts) :text))))
                                    ;; Other formats - try to extract text
                                    (t (or content "")))
                        ;; Convert role names: "model" -> "assistant" for Claude
                        for claude-role = (cond
                                           ((equal role "user") "user")
                                           ((equal role "model") "assistant")
                                           ((equal role "assistant") "assistant")
                                           (t role))
                        when (and claude-role text)
                        collect `(:role ,claude-role :content ,text))))

          ;; Build the request body for Claude
          (let ((request-body
                 `(:anthropic_version "vertex-2023-10-16"
                   :messages ,(vconcat messages)
                   :max_tokens ,(or gptel-max-tokens 4096))))

            ;; Add optional parameters
            (when gptel-temperature
              (plist-put request-body :temperature gptel-temperature))

            (when gptel--system-message
              (plist-put request-body :system gptel--system-message))

            ;; Add streaming flag if needed
            (when (and (boundp 'gptel-stream) gptel-stream)
              (plist-put request-body :stream t))

            request-body))

      ;; Gemini request format
      (let ((prompts-plist
             (list :contents (vconcat prompts)
                   :safetySettings [(:category "HARM_CATEGORY_HARASSMENT"
                                     :threshold "BLOCK_NONE")
                                    (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
                                     :threshold "BLOCK_NONE")
                                    (:category "HARM_CATEGORY_DANGEROUS_CONTENT"
                                     :threshold "BLOCK_NONE")
                                    (:category "HARM_CATEGORY_HATE_SPEECH"
                                     :threshold "BLOCK_NONE")]))
            params)

        (when gptel--system-message
          (plist-put prompts-plist :systemInstruction
                     `(:parts [(:text ,gptel--system-message)])))

        (when gptel-temperature
          (setq params
                (plist-put params :temperature (max gptel-temperature 1.0))))

        (when gptel-max-tokens
          (setq params
                (plist-put params :maxOutputTokens gptel-max-tokens)))

        (when params
          (plist-put prompts-plist :generationConfig params))

        prompts-plist))))

;;; Stream parsing
(cl-defmethod gptel-curl--parse-stream ((backend gptel-vertex) info)
  "Parse a Vertex AI data stream.
Return the text response accumulated since the last call."
  (let* ((publisher (gptel-vertex-publisher backend))
         (content-strs))
    (if (equal publisher "anthropic")
        ;; Claude stream parsing
        (condition-case nil
            (while (re-search-forward "^event: " nil t)
              (when (looking-at "message_delta")
                (forward-line 1)
                (when (looking-at "^data: ")
                  (forward-char 6)
                  (when-let* ((json-response (gptel--json-read))
                              (delta (plist-get json-response :delta))
                              (text (plist-get delta :text)))
                    (push text content-strs)))))
          (error nil))
      ;; Gemini stream parsing
      (condition-case nil
          (while (prog1 (search-forward "{" nil t)
                   (backward-char 1))
            (save-match-data
              (when-let* ((response (gptel--json-read))
                          (candidates (plist-get response :candidates))
                          (candidate (and candidates (aref candidates 0)))
                          (content (plist-get candidate :content))
                          (parts (plist-get content :parts)))
                (cl-loop for part across parts
                         for text = (plist-get part :text)
                         when text
                         do (push text content-strs)))))
        (error nil)))
    (apply #'concat (nreverse content-strs))))

;;; Response parsing
(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info)
  "Parse Vertex AI RESPONSE and call INFO's callback."
  (let* ((json-object-type 'plist)
         (publisher (gptel-vertex-publisher backend)))

    (if (equal publisher "anthropic")
        ;; Handle Claude response format
        (if-let ((content (plist-get response :content)))
            ;; Claude returns content as an array
            (if (vectorp content)
                (mapconcat (lambda (item)
                             (plist-get item :text))
                           content "")
              content)
          ;; Error handling
          (or (plist-get response :error)
              ""))
      ;; Handle Gemini response format
      (if-let ((candidates (plist-get response :candidates)))
          (let ((candidate (aref candidates 0)))
            (if-let ((content (plist-get candidate :content)))
                (let ((parts (plist-get content :parts)))
                  (if (vectorp parts)
                      (mapconcat (lambda (part)
                                   (plist-get part :text))
                                 parts "")
                    ""))
              ""))
        ""))))


;;; Parse prompt list
(cl-defmethod gptel--parse-list ((backend gptel-vertex) prompt-list)
  "Parse PROMPT-LIST for Vertex AI."
  (let* ((model-name (gptel--model-name gptel-model))
         (is-claude (string-match-p "claude" model-name)))

    (if is-claude
        ;; For Claude, convert to simple format
        (if (consp (car prompt-list))
            (let ((prompts))
              (dolist (item prompt-list)
                (pcase item
                  (`(:role ,role :content ,content)
                   (push `(:role ,role :content ,content) prompts))
                  (`(,role ,content)
                   (push `(:role ,(symbol-name role) :content ,content) prompts))
                  (_
                   (when (stringp item)
                     (push `(:role "user" :content ,item) prompts)))))
              (vconcat (nreverse prompts)))
          prompt-list)

      ;; For Gemini, convert to its expected format
      (if (consp (car prompt-list))
          (let ((prompts))
            (dolist (item prompt-list)
              (pcase item
                (`(:role ,role :content ,content)
                 (let ((vertex-role (if (equal role "assistant") "model" role)))
                   (push `(:role ,vertex-role :parts [(:text ,content)]) prompts)))
                (`(,role ,content)
                 (let ((vertex-role (if (eq role 'assistant) "model" (symbol-name role))))
                   (push `(:role ,vertex-role :parts [(:text ,content)]) prompts)))
                (_
                 (when (stringp item)
                   (push `(:role "user" :parts [(:text ,item)]) prompts)))))
            (vconcat (nreverse prompts)))
        prompt-list))))

;;; Parse buffer method for Gemini
(cl-defmethod gptel--parse-buffer ((backend gptel-vertex) &optional max-entries)
  "Parse the buffer content for Vertex AI."
  (let* ((model-name (gptel--model-name gptel-model))
         (is-claude (string-match-p "claude" model-name)))

    (if is-claude
        ;; For Claude, use simple text extraction
        (let ((prompts) (prev-pt (point)))
          (if (or gptel-mode gptel-track-response)
              (while (and (or (not max-entries) (>= max-entries 0))
                          (goto-char (previous-single-property-change
                                      (point) 'gptel nil (point-min)))
                          (not (= (point) prev-pt)))
                (pcase (get-char-property (point) 'gptel)
                  ('response
                   (when-let* ((content (gptel--trim-prefixes
                                         (buffer-substring-no-properties (point) prev-pt))))
                     (push `(:role "assistant" :content ,content) prompts)))
                  ('nil
                   (when-let* ((content (gptel--trim-prefixes
                                         (buffer-substring-no-properties (point) prev-pt))))
                     (push `(:role "user" :content ,content) prompts))))
                (setq prev-pt (point))
                (and max-entries (cl-decf max-entries)))
            (let ((content (string-trim (buffer-substring-no-properties
                                         (point-min) (point-max)))))
              (push `(:role "user" :content ,content) prompts)))
          (vconcat (nreverse prompts)))

      ;; For Gemini, use the parts format
      (let ((prompts) (prev-pt (point)))
        (if (or gptel-mode gptel-track-response)
            (while (and (or (not max-entries) (>= max-entries 0))
                        (goto-char (previous-single-property-change
                                    (point) 'gptel nil (point-min)))
                        (not (= (point) prev-pt)))
              (pcase (get-char-property (point) 'gptel)
                ('response
                 (when-let* ((content (gptel--trim-prefixes
                                       (buffer-substring-no-properties (point) prev-pt))))
                   (push (list :role "model" :parts `[(:text ,content)]) prompts)))
                ('nil
                 (when-let* ((content (gptel--trim-prefixes
                                       (buffer-substring-no-properties (point) prev-pt))))
                   (push (list :role "user" :parts `[(:text ,content)]) prompts))))
              (setq prev-pt (point))
              (and max-entries (cl-decf max-entries)))
          (let ((content (string-trim (buffer-substring-no-properties
                                       (point-min) (point-max)))))
            (push (list :role "user" :parts `[(:text ,content)]) prompts)))
        (vconcat (nreverse prompts))))))

;;; Backend constructor
(cl-defun gptel-make-vertex
    (name &key project-id
          (location gptel-vertex-default-location)
          (host (format "%s-aiplatform.googleapis.com" location))
          header
          (models '((gemini-2.5-flash :description "Gemini 2.5 Flash")
                    (gemini-2.5-pro :description "Gemini 2.5 Pro")
                    (claude-4.5-sonnet@20250929 :description "Claude 4.5 Sonnet")
                    (claude-4.1-opus@20250805 :description "Claude 4.1 Opus")))
          (stream t)
          (protocol "https")
          curl-args
          request-params)
  "Create a Vertex AI backend for Gemini and Claude models.

NAME is a unique name for this backend.

Keyword arguments:

PROJECT-ID (required): Your GCP project ID.

LOCATION: GCP region (default: us-east5).

MODELS: List of available models (both Gemini and Claude).

STREAM: Enable streaming responses (default: t).

CURL-ARGS: Additional arguments for curl requests.

REQUEST-PARAMS: Additional model parameters."
  (unless project-id
    (error "PROJECT-ID is required for Vertex AI backend"))

  ;; Determine initial publisher based on first model
  (let* ((first-model (if (listp models)
                          (caar models)
                        models))
         (model-name (if (symbolp first-model)
                         (symbol-name first-model)
                       first-model))
         (publisher (if (string-match-p "claude" model-name) "anthropic" "google"))
         ;; Create a unique variable to capture the backend being created
         (backend-var nil))

    ;; Create the header function that will capture the backend
    (unless header
      (setq header
            (lambda ()
              (let* ((backend (or backend-var gptel-backend))
                     (token (gptel-vertex--ensure-auth backend)))
                `(("Authorization" . ,(format "Bearer %s" token)))))))

    (setq backend-var
          (gptel--make-vertex
           :curl-args curl-args
           :name name
           :host host
           :header header
           :models (gptel--process-models models)
           :protocol protocol
           :stream stream
           :request-params request-params
           :project-id project-id
           :location location
           :publisher publisher
           :url (lambda ()
                  (let* ((model-name (gptel--model-name gptel-model))
                         (is-claude (string-match-p "claude" model-name))
                         (publisher (if is-claude "anthropic" "google"))
                         (method (cond
                                  ;; Claude methods
                                  ((and is-claude gptel-stream) "streamRawPredict")
                                  (is-claude "rawPredict")
                                  ;; Gemini methods
                                  ((and gptel-stream gptel-use-curl) "streamGenerateContent")
                                  (t "generateContent"))))
                    (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/%s/models/%s:%s"
                            location project-id location publisher model-name method)))))

    (prog1 backend-var
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend-var))))

;;; Convenience setup function
(cl-defun gptel-vertex-setup-backend
    (&key (name "VertexAI")
          project-id
          (location gptel-vertex-default-location)
          (models '((gemini-2.5-flash :description "Gemini 2.5 Flash")
                    (gemini-2.5-pro :description "Gemini 2.5 Pro")
                    (claude-4.5-sonnet@20250929 :description "Claude 4.5 Sonnet")
                    (claude-4.1-opus@20250805 :description "Claude 4.1 Opus")))
          (stream t)
          (default-model 'claude-4.5-sonnet@20250929))
  "Setup a Vertex AI backend for gptel.

This is a convenience function that creates a backend and sets it as default.

PROJECT-ID (required): Your GCP project ID.

NAME: Backend name (default: \"VertexAI\").

LOCATION: GCP region (default: us-east5).

MODELS: List of available models (both Gemini and Claude).

STREAM: Enable streaming (default: t).

DEFAULT-MODEL: Model to use by default."
  (unless project-id
    (error "PROJECT-ID is required"))

  (let ((backend (gptel-make-vertex
                     name
                   :project-id project-id
                   :location location
                   :models models
                   :stream stream)))
    ;; Set as default backend
    (setq-default gptel-backend backend)
    (setq-default gptel-model default-model)

    ;; Return the backend
    backend))

(provide 'gptel-vertex)

;;; gptel-vertex.el ends here
