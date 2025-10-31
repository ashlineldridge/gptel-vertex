;;; gptel-vertex.el --- Google Cloud Vertex AI support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Keywords: convenience, tools, llm, ai
;; URL: https://github.com/yourusername/gptel-vertex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds support for Google Cloud Vertex AI to gptel.
;; It supports both Gemini and Claude models via the Vertex AI API.
;;
;; Prerequisites:
;; - Google Cloud SDK installed and authenticated
;; - A GCP project with Vertex AI API enabled
;;
;; Basic usage:
;;   (require 'gptel-vertex)
;;   (gptel-vertex-setup-backend
;;     :project-id "your-project-id"
;;     :location "us-east5")

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
(defvar json-object-type)

;;; Customization
(defgroup gptel-vertex nil
  "Google Cloud Vertex AI support for gptel."
  :group 'gptel)

(defcustom gptel-vertex-default-location "global"
  "Default GCP region for Vertex AI.
Use 'global' for maximum availability (recommended for Claude models).
Specific regions like 'us-east1' or 'europe-west1' can be used for 
data residency requirements (10% pricing premium for Claude models)."
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
  publisher)

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
      (setf (gptel-vertex-access-token backend) (gptel-vertex--get-access-token))
      (setf (gptel-vertex-token-expiry backend)
            (time-add now (seconds-to-time gptel-vertex-token-refresh-seconds)))))
  (gptel-vertex-access-token backend))

;;; Request handling
(cl-defmethod gptel-curl--parse-stream ((backend gptel-vertex) info)
  "Parse a Vertex AI data stream.
Return the text response accumulated since the last call to this
function. Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((publisher (gptel-vertex-publisher backend))
         (content-strs))
    (if (equal publisher "google")
        ;; Gemini streaming format
        (condition-case nil
            (while (prog1 (search-forward "{" nil t)
                     (backward-char 1))
              (save-match-data
                (when-let* ((response (gptel--json-read))
                            (text (gptel--parse-response backend response info 'include)))
                  (push text content-strs))))
          (error
           (goto-char (match-beginning 0))))
      ;; Claude streaming format via Vertex
      (condition-case nil
          (while (re-search-forward "^event: " nil t)
            (cond
             ((looking-at "content_block_delta")
              (forward-line 1) (forward-char 5)
              (when-let* ((delta (plist-get (gptel--json-read) :delta))
                          (content (plist-get delta :text))
                          ((not (eq content :null))))
                (push content content-strs)))
             ((looking-at "message_delta")
              (forward-line 1) (forward-char 5)
              (when-let* ((response (gptel--json-read)))
                (plist-put info :output-tokens
                           (map-nested-elt response '(:usage :output_tokens)))
                (plist-put info :stop-reason
                           (map-nested-elt response '(:delta :stop_reason)))))))
        (error nil)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info
                                     &optional include-text)
  "Parse a Vertex AI (non-streaming) RESPONSE and return response text.
Mutate state INFO with response metadata.
If INCLUDE-TEXT is non-nil, include response text in the prompts list."
  (let ((publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "google")
        ;; Gemini response format
        (let* ((cand0 (map-nested-elt response '(:candidates 0)))
               (parts (map-nested-elt cand0 '(:content :parts))))
          (plist-put info :stop-reason (plist-get cand0 :finishReason))
          (plist-put info :output-tokens
                     (map-nested-elt response '(:usageMetadata :candidatesTokenCount)))
          (cl-loop
           for part across parts
           for tx = (plist-get part :text)
           if (and tx (not (eq tx :null)))
           collect tx into content-strs
           else if (plist-get part :functionCall)
           collect (copy-sequence it) into tool-use
           finally do
           (when (or tool-use include-text)
             (let* ((data (plist-get info :data))
                    (prompts (plist-get data :contents))
                    (last-prompt (aref prompts (1- (length prompts)))))
               (if (equal (plist-get last-prompt :role) "model")
                   (plist-put last-prompt :parts
                              (vconcat (plist-get last-prompt :parts) parts))
                 (plist-put data :contents
                            (vconcat prompts `((:role "model" :parts ,parts)))))))
           (when tool-use
             (plist-put info :tool-use
                        (nconc (plist-get info :tool-use) tool-use)))
           finally return
           (and content-strs (apply #'concat content-strs))))
      ;; Claude response format via Vertex
      (plist-put info :stop-reason (plist-get response :stop_reason))
      (plist-put info :output-tokens
                 (map-nested-elt response '(:usage :output_tokens)))
      (cl-loop
       with content = (plist-get response :content)
       for cblock across content
       for type = (plist-get cblock :type)
       if (equal type "text")
       collect (plist-get cblock :text) into content-strs
       finally return (and content-strs (apply #'concat content-strs))))))

(cl-defmethod gptel--request-data ((backend gptel-vertex) prompts)
  "JSON encode PROMPTS for sending to Vertex AI."
  (let ((publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "google")
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
          (gptel--merge-plists
           prompts-plist
           gptel--request-params
           (gptel-backend-request-params backend)
           (gptel--model-request-params gptel-model)))
      ;; Claude request format via Vertex
      (let ((claude-messages
             (cl-loop for prompt in prompts
                      collect (cond
                               ((equal (plist-get prompt :role) "user")
                                `(:role "user" 
                                  :content ,(plist-get (aref (plist-get prompt :parts) 0) :text)))
                               ((equal (plist-get prompt :role) "model")
                                `(:role "assistant"
                                  :content ,(plist-get (aref (plist-get prompt :parts) 0) :text)))))))
        (when gptel--system-message
          (push `(:role "system" :content ,gptel--system-message) claude-messages))
        (let ((prompts-plist
               `(:anthropic_version "vertex-2023-10-16"
                 :messages ,(vconcat claude-messages)
                 :stream ,(or gptel-stream :json-false)
                 :max_tokens ,(or gptel-max-tokens 4096))))
          (when gptel-temperature
            (plist-put prompts-plist :temperature gptel-temperature))
          (gptel--merge-plists
           prompts-plist
           gptel--request-params
           (gptel-backend-request-params backend)
           (gptel--model-request-params gptel-model)))))))

(cl-defmethod gptel--parse-list ((backend gptel-vertex) prompt-list)
  "Parse PROMPT-LIST for Vertex AI."
  (let ((publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "google")
        ;; Gemini format
        (if (consp (car prompt-list))
            (let ((full-prompt))
              (dolist (entry prompt-list)
                (pcase entry
                  (`(prompt . ,msg)
                   (push (list :role "user"
                               :parts `[(:text ,(or (car-safe msg) msg))])
                         full-prompt))
                  (`(response . ,msg)
                   (push (list :role "model"
                               :parts `[(:text ,(or (car-safe msg) msg))])
                         full-prompt))))
              (nreverse full-prompt))
          (cl-loop for text in prompt-list
                   for role = t then (not role)
                   if text
                   if role
                   collect (list :role "user" :parts `[(:text ,text)]) into prompts
                   else collect (list :role "model" :parts `[(:text ,text)]) into prompts
                   finally return prompts))
      ;; Claude format - delegate to OpenAI-style format
      (cl-call-next-method))))

(cl-defmethod gptel--parse-buffer ((backend gptel-vertex) &optional max-entries)
  "Parse the current buffer for Vertex AI prompts."
  (let ((publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "google")
        ;; Gemini format
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
          prompts)
      ;; Claude format - delegate
      (cl-call-next-method))))

(cl-defmethod gptel--inject-prompt ((backend gptel-vertex) data new-prompt &optional _position)
  "Append NEW-PROMPT to existing prompts in query DATA."
  (let ((publisher (gptel-vertex-publisher backend)))
    (if (equal publisher "google")
        (let ((prompts (plist-get data :contents)))
          (plist-put data :contents (vconcat prompts (list new-prompt))))
      ;; Claude format
      (let ((prompts (plist-get data :messages)))
        (plist-put data :messages (vconcat prompts (list new-prompt)))))))

(cl-defmethod gptel--headers ((backend gptel-vertex))
  "Return the request headers for BACKEND."
  (let ((token (gptel-vertex--ensure-auth backend))
        (base-headers (gptel-backend-header backend)))
    (append
     `(("Authorization" . ,(format "Bearer %s" token)))
     (if (functionp base-headers)
         (funcall base-headers)
       base-headers))))

;;; Model definitions
(defconst gptel-vertex--gemini-models
  '(gemini-2.5-flash
    gemini-2.5-pro)
  "List of available Gemini models via Vertex AI.")

(defconst gptel-vertex--claude-models
  '((claude-sonnet-4-5@20250929 :description "Claude Sonnet 4.5 - Most capable balanced model")
    (claude-sonnet-4@20250514 :description "Claude Sonnet 4")
    (claude-3-7-sonnet@20250219 :description "Claude Sonnet 3.7 (Deprecated Oct 28, 2025)")
    (claude-opus-4-1@20250805 :description "Claude Opus 4.1 - Most capable")
    (claude-opus-4@20250514 :description "Claude Opus 4")
    (claude-3-opus@20240229 :description "Claude Opus 3 (Deprecated Jun 30, 2025)")
    (claude-haiku-4-5@20251001 :description "Claude Haiku 4.5 - Fast and efficient")
    (claude-3-5-haiku@20241022 :description "Claude Haiku 3.5")
    (claude-3-haiku@20240307 :description "Claude Haiku 3"))
  "List of available Claude models via Vertex AI with version identifiers.")

;;; Backend creation
;;;###autoload
(cl-defun gptel-make-vertex
    (name &key curl-args header key request-params
          (project-id nil)
          (location gptel-vertex-default-location)
          (stream nil)
          (protocol "https")
          (models gptel-vertex--gemini-models)
          (host nil))
  "Register a Google Cloud Vertex AI backend for gptel with NAME.

Keyword arguments:

PROJECT-ID (required) is your GCP project ID.

LOCATION is the GCP region, defaults to `gptel-vertex-default-location'.

CURL-ARGS (optional) is a list of additional Curl arguments.

MODELS is a list of available model names, as symbols.

STREAM is a boolean to enable streaming responses.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

HOST (optional) overrides the default host.

HEADER (optional) is for additional headers.

KEY is not used for Vertex AI (uses gcloud auth).

REQUEST-PARAMS (optional) is a plist of additional parameters."
  (declare (indent 1))
  (unless project-id
    (error "PROJECT-ID is required for Vertex AI backend"))
  
  (let* ((model-name (if (listp models) (car models) models))
         (is-claude (string-match-p "claude" (symbol-name model-name)))
         (publisher (if is-claude "anthropic" "google"))
         (computed-host (or host
                            (format "%s-aiplatform.googleapis.com" location)))
         (endpoint (format "/v1/projects/%s/locations/%s/publishers/%s/models"
                          project-id location publisher))
         (backend (gptel--make-vertex
                   :curl-args curl-args
                   :name name
                   :host computed-host
                   :header (or header
                               (lambda ()
                                 (let ((token (gptel-vertex--ensure-auth gptel-backend)))
                                   `(("Authorization" . ,(format "Bearer %s" token))))))
                   :models (gptel--process-models models)
                   :protocol protocol
                   :endpoint endpoint
                   :stream stream
                   :request-params request-params
                   :key key
                   :project-id project-id
                   :location location
                   :publisher publisher
                   :url (lambda ()
                          (let* ((model-name (gptel--model-name gptel-model))
                                 (is-claude (string-match-p "claude" model-name))
                                 (publisher (if is-claude "anthropic" "google"))
                                 (method (cond
                                          ((and is-claude gptel-stream)
                                           "streamRawPredict")
                                          (is-claude "rawPredict")
                                          ((and gptel-stream gptel-use-curl)
                                           "streamGenerateContent")
                                          (t "generateContent"))))
                            (format "%s://%s/v1/projects/%s/locations/%s/publishers/%s/models/%s:%s"
                                    protocol
                                    computed-host
                                    project-id
                                    location
                                    publisher
                                    model-name
                                    method))))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

;;; Convenience setup function
;;;###autoload
(cl-defun gptel-vertex-setup-backend (&key
                                       (name "Vertex-AI")
                                       project-id
                                       (location gptel-vertex-default-location)
                                       (models 'all)
                                       (stream t))
  "Set up a Vertex AI backend for gptel.

NAME is the name for the backend (default \"Vertex-AI\").

PROJECT-ID (required) is your GCP project ID.

LOCATION is the GCP region.

MODELS can be:
  - 'gemini - Use Gemini models only
  - 'claude - Use Claude models only
  - 'all (default) - Use all available models
  - A list of specific model symbols

STREAM enables streaming responses (default t)."
  (unless project-id
    (setq project-id (read-string "Enter your GCP project ID: ")))
  
  (let* ((model-list
          (pcase models
            ('gemini gptel-vertex--gemini-models)
            ('claude gptel-vertex--claude-models)
            ('all (append gptel-vertex--gemini-models
                         gptel-vertex--claude-models))
            ((pred listp) models)
            (_ (append gptel-vertex--gemini-models
                      gptel-vertex--claude-models))))
         (backend (gptel-make-vertex name
                    :project-id project-id
                    :location location
                    :models model-list
                    :stream stream)))
    
    ;; Set as default backend with first model
    (setq-default gptel-backend backend)
    (setq-default gptel-model (car model-list))
    
    (message "Vertex AI backend '%s' configured and set as default" name)
    backend))

(provide 'gptel-vertex)
;;; gptel-vertex.el ends here
