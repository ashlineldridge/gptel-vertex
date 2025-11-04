;;; gptel-vertex.el --- Vertex support for gptel  -*- lexical-binding: t; -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/gptel-vertex
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.0") (gptel "0.9.0"))
;; Keywords: ai, llm, google, gcp, vertex, claude, gemini

;;; Commentary:
;;
;; This package extends gptel to support Google Cloud Vertex AI.

;;; Code:

(require 'gptel)
(require 'cl-lib)

(eval-when-compile
  (require 'cl-lib))

;; Declare functions from gptel to avoid byte-compiler warnings
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--base64-encode "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--json-read "gptel")
(declare-function gptel--json-encode "gptel")
(declare-function gptel--process-models "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--model-capable-p "gptel")

(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-use-curl)
(defvar gptel-backend)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel--system-message)
(defvar gptel-log-level)
(defvar gptel-mode)
(defvar gptel-track-response)
(defvar gptel-track-media)
(defvar gptel--known-backends)
(defvar json-object-type)

;;; User options

(defgroup gptel-vertex nil
  "Google Cloud Vertex AI backend for gptel."
  :group 'gptel)

(defcustom gptel-vertex-default-location "us-east5"
  "Default GCP region for Vertex AI.

Common regions:
- us-east5, us-central1, us-west1 (United States)
- europe-west4, europe-west1 (Europe)
- asia-northeast1, asia-southeast1 (Asia Pacific)

For Claude models, check regional availability in GCP documentation."
  :type 'string
  :group 'gptel-vertex)

(defcustom gptel-vertex-token-refresh-seconds 3500
  "Seconds before token expiry to refresh.

GCP access tokens expire after 3600 seconds (1 hour).
Default refreshes 100 seconds before expiry to ensure reliability."
  :type 'integer
  :group 'gptel-vertex)

(defcustom gptel-vertex-models
  '((gemini-2.5-flash :description "Gemini 2.5 Flash")
    (gemini-2.5-pro :description "Gemini 2.5 Pro")
    (claude-sonnet-4-5@20250929 :description "Claude Sonnet 4.5")
    (claude-opus-4-1@20250805 :description "Claude Opus 4.1"))
  "Models available in Vertex AI.

Each entry is a list of (MODEL-ID . PROPERTIES) where MODEL-ID is the
identifier used in Vertex AI API calls.

Note: Claude models require the @VERSION format for Vertex AI."
  :type '(alist :key-type symbol :value-type plist)
  :group 'gptel-vertex)

;;; Backend Definition

(cl-defstruct (gptel-vertex (:constructor gptel--make-vertex)
                            (:copier nil)
                            (:include gptel-backend))
  "A Vertex AI backend for gptel.

This backend supports both Google's Gemini models and Anthropic's
Claude models through Google Cloud Vertex AI."
  project-id    ; GCP project identifier
  location      ; GCP region
  access-token  ; Cached OAuth2 token
  token-expiry  ; Token expiration time
  publisher)    ; Current publisher ("google" or "anthropic")

;;; Authentication

(defun gptel-vertex--get-access-token ()
  "Obtain GCP access token using gcloud CLI.

This function calls the gcloud CLI to get an access token for
authenticating with Vertex AI.  The user must be authenticated
with gcloud before using this backend."
  (let ((result (string-trim
                 (shell-command-to-string "gcloud auth print-access-token"))))
    (if (or (string-empty-p result)
            (string-match-p "ERROR" result))
        (user-error "Failed to obtain GCP access token.  Please run `gcloud auth login'")
      result)))

(cl-defmethod gptel-vertex--ensure-auth ((backend gptel-vertex))
  "Ensure BACKEND has a valid access token.

Refreshes the token if it's expired or will expire soon."
  (let ((now (current-time)))
    (when (or (not (gptel-vertex-access-token backend))
              (not (gptel-vertex-token-expiry backend))
              (time-less-p (gptel-vertex-token-expiry backend) now))
      ;; Token needs refresh
      (when gptel-log-level
        (message "gptel-vertex: Refreshing access token"))
      (let ((token (gptel-vertex--get-access-token)))
        (setf (gptel-vertex-access-token backend) token
              (gptel-vertex-token-expiry backend)
              (time-add now (seconds-to-time gptel-vertex-token-refresh-seconds)))))
    (gptel-vertex-access-token backend)))

;;; Request handling

(cl-defmethod gptel--request-data ((backend gptel-vertex) prompts)
  "Encode PROMPTS for sending to Vertex AI via BACKEND.

PROMPTS is a list of plists with :role and :content keys."
  (let* ((model-name (gptel--model-name gptel-model))
         (is-claude (string-match-p "claude" model-name))
         (publisher (if is-claude "anthropic" "google")))

    ;; Update publisher based on current model
    (setf (gptel-vertex-publisher backend) publisher)

    (if is-claude
        ;; Claude format via Vertex AI
        (let ((request-body
               `(:anthropic_version "vertex-2023-10-16"
                 :messages ,(vconcat
                             (mapcar (lambda (msg)
                                       (list :role (plist-get msg :role)
                                             :content (plist-get msg :content)))
                                     prompts))
                 :max_tokens ,(or gptel-max-tokens 4096))))
          (when gptel-temperature
            (plist-put request-body :temperature gptel-temperature))
          (when gptel--system-message
            (plist-put request-body :system gptel--system-message))
          (when gptel-stream
            (plist-put request-body :stream t))
          request-body)

      ;; Gemini format
      (let ((prompts-plist
             `(:contents ,(vconcat
                           (mapcar
                            (lambda (msg)
                              (let ((role (plist-get msg :role)))
                                (list :role (if (equal role "assistant") "model" role)
                                      :parts (vector (list :text (plist-get msg :content))))))
                            prompts))
               :safetySettings
               [(:category "HARM_CATEGORY_HARASSMENT" :threshold "BLOCK_NONE")
                (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT" :threshold "BLOCK_NONE")
                (:category "HARM_CATEGORY_DANGEROUS_CONTENT" :threshold "BLOCK_NONE")
                (:category "HARM_CATEGORY_HATE_SPEECH" :threshold "BLOCK_NONE")]))
            params)
        (when gptel--system-message
          (plist-put prompts-plist :systemInstruction
                     `(:parts [(:text ,gptel--system-message)])))
        (when gptel-temperature
          (setq params (plist-put params :temperature (max gptel-temperature 1.0))))
        (when gptel-max-tokens
          (setq params (plist-put params :maxOutputTokens gptel-max-tokens)))
        (when params
          (plist-put prompts-plist :generationConfig params))
        prompts-plist))))

;;; Response parsing

(cl-defmethod gptel-curl--parse-stream ((backend gptel-vertex) _info)
  "Parse streaming response from Vertex AI for BACKEND."
  (let* ((publisher (gptel-vertex-publisher backend))
         (content-strs))
    (if (equal publisher "anthropic")
        ;; Claude SSE stream format
        (condition-case nil
            (while (re-search-forward "^data:" nil t)
              (save-match-data
                (when-let* ((response (gptel--json-read))
                            (delta (plist-get response :delta))
                            (text (or (plist-get delta :text)
                                      (plist-get delta :partial_json)))
                            ((not (eq text :null))))
                  (push text content-strs))))
          (error (goto-char (match-beginning 0))))
      ;; Gemini JSON stream format
      (condition-case nil
          (while (prog1 (search-forward "{" nil t)
                   (backward-char 1))
            (save-match-data
              (when-let* ((response (gptel--json-read))
                          (text (gptel--parse-response backend response _info)))
                (push text content-strs))))
        (error (goto-char (match-beginning 0)))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info)
  "Parse RESPONSE from Vertex AI for BACKEND.

INFO is the request info plist."
  (let* ((json-object-type 'plist)
         (publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "anthropic")
        ;; Claude response
        (progn
          ;; Store metadata
          (when-let ((stop-reason (plist-get response :stop_reason)))
            (plist-put info :stop-reason stop-reason))
          (when-let ((usage (plist-get response :usage)))
            (plist-put info :output-tokens (plist-get usage :output_tokens)))

          (cond
           ;; Error response
           ((plist-get response :error)
            (or (plist-get (plist-get response :error) :message)
                (format "%S" (plist-get response :error))))
           ;; Normal response
           ((plist-get response :content)
            (let ((content (plist-get response :content)))
              (cond
               ((vectorp content)
                (mapconcat
                 (lambda (block)
                   (when (equal (plist-get block :type) "text")
                     (plist-get block :text)))
                 content ""))
               ((listp content)
                (mapconcat
                 (lambda (block)
                   (when (equal (plist-get block :type) "text")
                     (plist-get block :text)))
                 content ""))
               ((stringp content) content)
               (t ""))))
           (t "")))
      ;; Gemini response
      (if-let ((candidates (plist-get response :candidates)))
          (if-let* ((candidate (aref candidates 0))
                    (content (plist-get candidate :content))
                    (parts (plist-get content :parts)))
              (mapconcat (lambda (part) (plist-get part :text))
                         parts "")
            "")
        ""))))

;;; Prompt parsing methods

(cl-defmethod gptel--parse-list ((_backend gptel-vertex) prompt-list)
  "Parse PROMPT-LIST for Vertex AI.

Handles both simple (list of strings) and advanced (list of
role-content pairs) formats."
  (if (consp (car prompt-list))
      ;; Advanced format: ((role . content) ...)
      (let ((prompts))
        (dolist (entry prompt-list)
          (pcase entry
            (`(prompt . ,content)
             (push (list :role "user" :content (or (car-safe content) content))
                   prompts))
            (`(response . ,content)
             (push (list :role "assistant" :content (or (car-safe content) content))
                   prompts))
            (_)))
        (nreverse prompts))
    ;; Simple format: alternating strings
    (cl-loop for text in prompt-list
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant") :content text))))

(cl-defmethod gptel--parse-buffer ((_backend gptel-vertex) &optional max-entries)
  "Parse current buffer for conversation history.

Optional MAX-ENTRIES limits the number of entries parsed."
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
               (unless (string-blank-p content)
                 (push (list :role "assistant" :content content) prompts))))
            ('nil
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (unless (string-blank-p content)
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      ;; Non-gptel buffer: use entire content as user prompt
      (when-let* ((content (string-trim (buffer-substring-no-properties
                                         (point-min) (point-max)))))
        (unless (string-blank-p content)
          (push (list :role "user" :content content) prompts))))
    prompts))

;;; User-facing functions

;;;###autoload
(cl-defun gptel-make-vertex
    (name &key project-id
          (location gptel-vertex-default-location)
          (host (format "%s-aiplatform.googleapis.com" location))
          header
          (models gptel-vertex-models)
          (stream t)
          (protocol "https")
          curl-args
          request-params)
  "Create a Vertex AI backend for gptel.

NAME is a unique name for this backend instance.

Keyword arguments:

PROJECT-ID (required): Your Google Cloud Platform project ID.

LOCATION: GCP region for API requests.  Default is
`gptel-vertex-default-location'.

HOST: API endpoint host.  Automatically set based on LOCATION.

HEADER: Optional HTTP headers function.  If not provided,
creates one that adds the Bearer token automatically.

MODELS: List of available models.  Each entry can be a symbol
or a list (SYMBOL :description STRING ...).  Both Gemini and
Claude models are supported.

STREAM: Whether to enable response streaming.  Default is t.

PROTOCOL: HTTP protocol, default \"https\".

CURL-ARGS: Additional arguments to pass to curl.

REQUEST-PARAMS: Additional API request parameters."
  (unless project-id
    (user-error "PROJECT-ID is required for Vertex AI backend"))

  ;; Create header function if not provided
  (unless header
    (setq header
          (lambda ()
            ;; Ensure we have a valid vertex backend
            (unless (gptel-vertex-p gptel-backend)
              (error "Current backend is not a Vertex AI backend"))
            (let ((token (gptel-vertex--ensure-auth gptel-backend)))
              `(("Authorization" . ,(format "Bearer %s" token)))))))

  (let ((backend
         (gptel--make-vertex
          :name name
          :host host
          :header header
          :models (gptel--process-models models)
          :protocol protocol
          :stream stream
          :request-params request-params
          :curl-args curl-args
          :project-id project-id
          :location location
          :publisher "google"  ; Default, updated dynamically
          :url (lambda ()
                 (let* ((model-name (gptel--model-name gptel-model))
                        (is-claude (string-match-p "claude" model-name))
                        (publisher (if is-claude "anthropic" "google"))
                        (method (cond
                                 ((and is-claude gptel-stream) "streamRawPredict")
                                 (is-claude "rawPredict")
                                 ((and gptel-stream gptel-use-curl) "streamGenerateContent")
                                 (t "generateContent"))))
                   (format "%s://%s/v1/projects/%s/locations/%s/publishers/%s/models/%s:%s"
                           protocol host project-id location publisher model-name method))))))

    (prog1 backend
      ;; Register the backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

(provide 'gptel-vertex)

;;; gptel-vertex.el ends here
