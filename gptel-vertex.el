;;; gptel-vertex.el --- Vertex AI support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ashlin Eldridge

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; Keywords: ai, llm, google, gcp, vertex, anthropic, gemini

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

;; This file adds support for Google Cloud Vertex AI to gptel.

;;; Code:

(require 'cl-generic)
(require 'cl-lib)
(require 'map)
(eval-when-compile
  (require 'cl-lib))
(require 'gptel)

(defvar gptel-backend)
(defvar gptel-include-reasoning)
(defvar gptel-log-level)
(defvar gptel-max-tokens)
(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-temperature)
(defvar gptel-tools)
(defvar gptel-track-media)
(defvar gptel-track-response)
(defvar gptel-use-curl)
(defvar gptel-use-tools)
(defvar gptel--known-backends)
(defvar gptel--request-params)
(defvar gptel--schema)
(defvar gptel--system-message)
(defvar json-object-type)

(declare-function gptel--base64-encode "gptel")
(declare-function gptel--dispatch-schema-type "gptel")
(declare-function gptel--get-api-key "gptel")
(declare-function gptel--insert-file-string "gptel")
(declare-function gptel--json-encode "gptel")
(declare-function gptel--json-read "gptel")
(declare-function gptel--json-read-string "gptel")
(declare-function gptel--merge-plists "gptel")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--model-request-params "gptel")
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--preprocess-schema "gptel")
(declare-function gptel--process-models "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel-context--collect-media "gptel-context")

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

For Anthropic models, check regional availability in GCP documentation."
  :type 'string
  :group 'gptel-vertex)

(defcustom gptel-vertex-token-refresh-seconds 3500
  "Seconds before token expiry to refresh.

GCP access tokens expire after 3600 seconds (1 hour).
Default refreshes 100 seconds before expiry to ensure reliability."
  :type 'integer
  :group 'gptel-vertex)

(defcustom gptel-vertex-models
  '((gemini-2.5-flash
     :description "Best price/performance, with well-rounded capabilities"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 1048
     :input-cost 0.3
     :output-cost 2.50
     :cutoff-date "2025-01")
    (gemini-2.5-pro
     :description "Most powerful Gemini thinking model"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 1048
     :input-cost 1.25
     :output-cost 10.00
     :cutoff-date "2025-01")
    (claude-sonnet-4-5@20250929
     :description "High-performance model with exceptional reasoning"
     :capabilities (media tool-use)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-07")
    (claude-opus-4-1@20250805
     :description "Most capable model for complex reasoning"
     :capabilities (media tool-use)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-03"))
  "Models available in Vertex AI.

Each entry is a list of (MODEL-ID . PROPERTIES) where MODEL-ID is the
identifier used in Vertex AI API calls.

Keys:
- `:description': Brief description of the model
- `:capabilities': List of capabilities (tool-use, json, media, audio, video)
- `:mime-types': List of supported MIME types for media files
- `:context-window': Context window size in thousands of tokens
- `:input-cost': Input cost in US dollars per million tokens
- `:output-cost': Output cost in US dollars per million tokens
- `:cutoff-date': Knowledge cutoff date

Note: Anthropic models require the @VERSION format for Vertex AI."
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
  publisher)    ; Current publisher (\"google\" or \"anthropic\")

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

;;; Publisher Registry

(defconst gptel-vertex--publishers
  '((anthropic
     :name "anthropic"
     :detect (lambda (model) (string-match-p "^claude-" model))
     :api-version "vertex-2023-10-16")
    (google
     :name "google"
     :detect (lambda (model) (string-match-p "^gemini-" model))))
  "Publisher configurations for Vertex AI.

Each entry is a list of (PUBLISHER-SYMBOL . PROPERTIES) where:
- PUBLISHER-SYMBOL: Internal identifier (anthropic, google, etc.)
- :name - Publisher name used in Vertex AI API paths
- :detect - Function to detect if a model belongs to this publisher
- :api-version - API version string (for publishers that require it)")

(defun gptel-vertex--detect-publisher (model-name)
  "Return publisher symbol for MODEL-NAME, or nil if unknown."
  (cl-loop for (publisher . props) in gptel-vertex--publishers
           when (funcall (plist-get props :detect) model-name)
           return publisher))

(defun gptel-vertex--get-publisher-name (publisher)
  "Return API publisher name string for PUBLISHER symbol."
  (plist-get (cdr (assq publisher gptel-vertex--publishers)) :name))

(defun gptel-vertex--get-api-version (publisher)
  "Return API version for PUBLISHER symbol, or nil if not applicable."
  (plist-get (cdr (assq publisher gptel-vertex--publishers)) :api-version))

;;; Request handling - This comprehensive implementation handles all request types

(cl-defmethod gptel--request-data ((backend gptel-vertex) prompts)
  "Encode PROMPTS for sending to Vertex AI via BACKEND.

PROMPTS is a list of plists with :role and :content keys."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (or (gptel-vertex--detect-publisher model-name)
                        (error "Unknown model type for Vertex AI: %s" model-name))))
    ;; Update publisher slot with API name
    (setf (gptel-vertex-publisher backend)
          (gptel-vertex--get-publisher-name publisher))
    ;; Dispatch to publisher-specific implementation
    (pcase publisher
      ('anthropic (gptel-vertex--request-data-anthropic prompts))
      ('google (gptel-vertex--request-data-gemini prompts))
      (_ (error "Unsupported publisher: %s" publisher)))))

(defun gptel-vertex--request-data-anthropic (prompts)
  "Build request data for Anthropic models via Vertex AI.

PROMPTS is a list of message plists."
  (let ((request-body
         `(:anthropic_version "vertex-2023-10-16"
           :messages ,(vconcat prompts)
           :max_tokens ,(or gptel-max-tokens 4096))))
    (when gptel--system-message
      (plist-put request-body :system gptel--system-message))
    (when gptel-temperature
      (plist-put request-body :temperature gptel-temperature))
    (when gptel-stream
      (plist-put request-body :stream t))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put request-body :tool_choice '(:type "any")))
      (when gptel-tools
        (plist-put request-body :tools
                   (gptel--parse-tools gptel-backend gptel-tools))))
    (when gptel--schema
      (plist-put request-body :tools
                 (vconcat
                  (list (gptel--parse-schema gptel-backend gptel--schema))
                  (plist-get request-body :tools)))
      (plist-put request-body :tool_choice
                 `(:type "tool" :name "response_json")))
    ;; Merge request params with model and backend params
    (gptel--merge-plists
     request-body
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))

(defun gptel-vertex--request-data-gemini (prompts)
  "Build request data for Google Gemini models via Vertex AI.

PROMPTS is a list of message plists."
  (let ((prompts-plist
         `(:contents ,(vconcat prompts)
           :safetySettings
           [(:category "HARM_CATEGORY_HARASSMENT" :threshold "BLOCK_NONE")
            (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT" :threshold "BLOCK_NONE")
            (:category "HARM_CATEGORY_DANGEROUS_CONTENT" :threshold "BLOCK_NONE")
            (:category "HARM_CATEGORY_HATE_SPEECH" :threshold "BLOCK_NONE")]))
        params)
    (when gptel--system-message
      (plist-put prompts-plist :system_instruction
                 `(:parts [(:text ,gptel--system-message)])))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put prompts-plist :tool_config
                   '(:function_calling_config (:mode "ANY"))))
      (when gptel-tools
        (plist-put prompts-plist :tools
                   (gptel--parse-tools gptel-backend gptel-tools))))
    (when gptel-temperature
      (setq params (plist-put params :temperature (max gptel-temperature 1.0))))
    (when gptel-max-tokens
      (setq params (plist-put params :maxOutputTokens gptel-max-tokens)))
    (when gptel-include-reasoning
      (setq params (plist-put params :thinkingConfig '(:includeThoughts t))))
    (when gptel--schema
      (setq params (nconc params (gptel-vertex--gemini-filter-schema
                                  (gptel--parse-schema gptel-backend gptel--schema)))))
    (when params
      (plist-put prompts-plist :generationConfig params))
    ;; Merge request params with model and backend params
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))

;;; Schema handling

(cl-defmethod gptel--parse-schema ((_backend gptel-vertex) schema)
  "Parse SCHEMA for Vertex AI BACKEND.

Dispatches to appropriate format based on current model."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic
       ;; Anthropic format: JSON schema via tool call
       (list
        :name "response_json"
        :description "Record JSON output according to user prompt"
        :input_schema (gptel--preprocess-schema
                       (gptel--dispatch-schema-type schema))))
      ('google
       ;; Gemini format: response MIME type and schema
       (list :responseMimeType "application/json"
             :responseSchema (gptel--preprocess-schema
                              (gptel--dispatch-schema-type schema))))
      (_ (error "Unsupported publisher for schema: %s" publisher)))))

(defun gptel-vertex--gemini-filter-schema (schema)
  "Destructively filter unsupported attributes from SCHEMA.

Gemini's API does not support `additionalProperties'."
  (cl-remf schema :additionalProperties)
  (when (plistp schema)
    (cl-loop for (key val) on schema by #'cddr
             do (cond
                 ;; Recursively modify schemas within vectors (anyOf/allOf)
                 ((memq key '(:anyOf :allOf))
                  (dotimes (i (length val))
                    (aset val i (gptel-vertex--gemini-filter-schema (aref val i)))))
                 ;; Recursively modify plist values, which may contain sub-schemas
                 ((plistp val)
                  (when (cl-remf val :additionalProperties)
                    (cl-remf (plist-get schema key) :additionalProperties))
                  (gptel-vertex--gemini-filter-schema val))
                 ;; Default: do nothing to other key-value pairs
                 (t nil))))
  schema)

;;; Tool handling

(cl-defmethod gptel--parse-tools ((_backend gptel-vertex) tools)
  "Parse TOOLS to the Vertex AI tool definition spec.

TOOLS is a list of `gptel-tool' structs, which see.
Dispatches to appropriate format based on current model."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic (gptel-vertex--parse-tools-anthropic tools))
      ('google (gptel-vertex--parse-tools-gemini tools))
      (_ (error "Unsupported publisher for tools: %s" publisher)))))

(defun gptel-vertex--parse-tools-anthropic (tools)
  "Parse TOOLS to Anthropic API format for Vertex AI.

TOOLS is a list of `gptel-tool' structs."
  (vconcat
   (mapcar
    (lambda (tool)
      (list :name (gptel-tool-name tool)
            :description (gptel-tool-description tool)
            :input_schema
            (list :type "object"
                  :properties
                  (cl-loop
                   for arg in (gptel-tool-args tool)
                   for argspec = (copy-sequence arg)
                   for name = (plist-get arg :name)
                   for newname = (or (and (keywordp name) name)
                                     (make-symbol (concat ":" name)))
                   do
                   (cl-remf argspec :name)
                   (cl-remf argspec :optional)
                   if (equal (plist-get arg :type) "object")
                   do (unless (plist-member argspec :required)
                        (plist-put argspec :required []))
                   append (list newname argspec))
                  :required
                  (vconcat
                   (delq nil (mapcar
                              (lambda (arg) (and (not (plist-get arg :optional))
                                                 (plist-get arg :name)))
                              (gptel-tool-args tool)))))))
    (ensure-list tools))))

(defun gptel-vertex--parse-tools-gemini (tools)
  "Parse TOOLS to Gemini API format for Vertex AI.

TOOLS is a list of `gptel-tool' structs."
  (cl-loop
   for tool in (ensure-list tools)
   collect
   (list
    :name (gptel-tool-name tool)
    :description (gptel-tool-description tool)
    :parameters
    (if (not (gptel-tool-args tool))
        :null
      (list :type "object"
            :properties
            (cl-loop
             for arg in (gptel-tool-args tool)
             for argspec = (copy-sequence arg)
             for name = (plist-get arg :name)
             for newname = (or (and (keywordp name) name)
                               (make-symbol (concat ":" name)))
             do
             (cl-remf argspec :name)
             (cl-remf argspec :optional)
             if (equal (plist-get arg :type) "object")
             do (unless (plist-member argspec :required)
                  (plist-put argspec :required []))
             if (equal (plist-get arg :type) "string")
             do (cl-remf argspec :format)
             append (list newname (gptel-vertex--gemini-filter-schema argspec)))
            :required
            (vconcat
             (delq nil (mapcar
                        (lambda (arg) (and (not (plist-get arg :optional))
                                           (plist-get arg :name)))
                        (gptel-tool-args tool)))))))
   into tool-specs
   finally return `[(:function_declarations ,(vconcat tool-specs))]))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-vertex) tool-use)
  "Return a prompt containing tool call results in TOOL-USE.

TOOL-USE is a list of plists containing tool names, arguments and call results.
Dispatches to appropriate format based on current model."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic
       ;; Anthropic format
       (list
        :role "user"
        :content
        (vconcat
         (mapcar
          (lambda (tool-call)
            (let* ((result (plist-get tool-call :result))
                   (formatted
                    (list :type "tool_result"
                          :tool_use_id (plist-get tool-call :id)
                          :content (if (stringp result) result
                                     (prin1-to-string result)))))
              (prog1 formatted
                (when (plist-get tool-call :error)
                  (plist-put formatted :is_error t)))))
          tool-use))))
      ('google
       ;; Gemini format
       (list
        :role "user"
        :parts
        (vconcat
         (mapcar
          (lambda (tool-call)
            (let ((result (plist-get tool-call :result))
                  (name (plist-get tool-call :name)))
              `(:functionResponse
                (:name ,name :response
                 (:name ,name :content ,result)))))
          tool-use))))
      (_ (error "Unsupported publisher for tool results: %s" publisher)))))

(cl-defmethod gptel--inject-prompt ((_backend gptel-vertex) data new-prompt &optional _position)
  "Append NEW-PROMPT to existing prompts in query DATA.

See generic implementation for full documentation.
Dispatches based on current model type."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic
       ;; Anthropic uses :messages
       (let ((prompts (plist-get data :messages)))
         (plist-put data :messages (vconcat prompts (list new-prompt)))))
      ('google
       ;; Gemini uses :contents
       (let ((prompts (plist-get data :contents)))
         (plist-put data :contents (vconcat prompts (list new-prompt)))))
      (_ (error "Unsupported publisher for inject-prompt: %s" publisher)))))

;;; Response parsing - Handles streaming and non-streaming responses

(cl-defmethod gptel-curl--parse-stream ((backend gptel-vertex) info)
  "Parse streaming response from Vertex AI for BACKEND.

Return the text response accumulated since the last call.
Additionally, mutate state INFO to add tool-use information."
  (let ((publisher-name (gptel-vertex-publisher backend)))
    (pcase publisher-name
      ("anthropic" (gptel-vertex--parse-stream-anthropic backend info))
      ("google" (gptel-vertex--parse-stream-gemini backend info))
      (_ (error "Unknown publisher in stream parsing: %s" publisher-name)))))

(defun gptel-vertex--parse-stream-anthropic (_backend info)
  "Parse Anthropic SSE streaming response.

BACKEND is the gptel-vertex backend (unused but kept for consistency).
Return accumulated text since last call.  Mutate INFO with metadata."
  (let ((content-strs)
        (pt (point)))
    (condition-case nil
        (while (re-search-forward "^event: " nil t)
          (setq pt (match-beginning 0))
          (progn
            (if (equal (line-end-position) (point-max))
                (error "Data block incomplete"))
            (cond
             ((looking-at "content_block_delta")
              (forward-line 1) (forward-char 5)
              (when-let* ((delta (plist-get (gptel--json-read) :delta)))
                (if-let* ((content (plist-get delta :text))
                          ((not (eq content :null))))
                    (push content content-strs)
                  (when-let* ((partial-json (plist-get delta :partial_json)))
                    (plist-put info :partial_json
                               (cons partial-json (plist-get info :partial_json)))))))

             ((looking-at "content_block_start")
              (forward-line 1) (forward-char 5)
              (when-let* ((cblock (plist-get (gptel--json-read) :content_block)))
                (pcase (plist-get cblock :type)
                  ("text" (push (plist-get cblock :text) content-strs))
                  ("tool_use" (plist-put info :tool-use
                                         (cons (list :id (plist-get cblock :id)
                                                     :name (plist-get cblock :name))
                                               (plist-get info :tool-use)))))))

             ((looking-at "content_block_stop")
              (when-let* ((partial (plist-get info :partial_json)))
                (condition-case-unless-debug nil
                    (let* ((args-json (apply #'concat (nreverse partial)))
                           (args-decoded
                            (if (string-empty-p args-json)
                                nil (gptel--json-read-string args-json))))
                      (plist-put (car (plist-get info :tool-use)) :input args-decoded))
                  (error (pop (plist-get info :tool-use))))
                (plist-put info :partial_json nil)))

             ((looking-at "message_delta")
              (forward-line 1) (forward-char 5)
              (when-let* ((tool-use (plist-get info :tool-use))
                          (response (gptel--json-read)))
                (let* ((data (plist-get info :data))
                       (prompts (plist-get data :messages)))
                  (plist-put
                   data :messages
                   (vconcat
                    prompts
                    `((:role "assistant"
                       :content ,(vconcat
                                  (and-let* ((strs (plist-get info :partial_text)))
                                    `((:type "text" :text ,(apply #'concat (nreverse strs)))))
                                  (mapcar (lambda (tool-call)
                                            (append (list :type "tool_use")
                                                    (copy-sequence tool-call)))
                                          tool-use))))))
                  (plist-put info :partial_text nil)
                  (mapc (lambda (tool-call)
                          (plist-put tool-call :args (plist-get tool-call :input))
                          (plist-put tool-call :input nil)
                          (plist-put tool-call :id (plist-get tool-call :id)))
                        tool-use))
                (plist-put info :output-tokens
                           (map-nested-elt response '(:usage :output_tokens)))
                (plist-put info :stop-reason
                           (map-nested-elt response '(:delta :stop_reason))))))))
      (error (goto-char pt)))
    (let ((response-text (apply #'concat (nreverse content-strs))))
      (unless (string-empty-p response-text)
        (plist-put info :partial_text
                   (cons response-text (plist-get info :partial_text))))
      response-text)))

(defun gptel-vertex--parse-stream-gemini (backend info)
  "Parse Gemini JSON streaming response.

BACKEND is the gptel-vertex backend.
Return accumulated text since last call.  Mutate INFO with metadata."
  (let ((content-strs))
    (condition-case nil
        (while (prog1 (search-forward "{" nil t)
                 (backward-char 1))
          (save-match-data
            (when-let* ((response (gptel--json-read))
                        (text (gptel--parse-response backend response info 'include)))
              (push text content-strs))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info
                                     &optional include-text)
  "Parse RESPONSE from Vertex AI for BACKEND.

INFO is the request info plist.
If INCLUDE-TEXT is non-nil, include response text in prompts list.
Dispatches based on current model type."
  (let ((publisher-name (gptel-vertex-publisher backend)))
    (pcase publisher-name
      ("anthropic" (gptel-vertex--parse-response-anthropic backend response info))
      ("google" (gptel-vertex--parse-response-gemini backend response info include-text))
      (_ (error "Unknown publisher in response parsing: %s" publisher-name)))))

(defun gptel-vertex--parse-response-anthropic (_backend response info)
  "Parse Anthropic response from Vertex AI.

BACKEND is the gptel-vertex backend (unused but kept for consistency).
RESPONSE is the JSON response plist.
INFO is the request info plist.  Mutate with metadata."
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
    (let ((content (plist-get response :content))
          (content-strs)
          (tool-use))
      (cl-loop
       for cblock across content
       for type = (plist-get cblock :type)
       if (equal type "text")
       collect (plist-get cblock :text) into strs
       else if (equal type "tool_use")
       collect cblock into tools
       finally do
       (setq content-strs strs)
       (setq tool-use tools))

      (when tool-use
        ;; Add tool call to prompts list
        (let* ((data (plist-get info :data))
               (prompts (plist-get data :messages)))
          (plist-put
           data :messages
           (vconcat prompts `((:role "assistant" :content ,content)))))
        ;; Capture tool call data for running tools
        (cl-loop
         for call-raw in tool-use
         for call = (copy-sequence call-raw)
         do
         (plist-put call :args (plist-get call :input))
         (plist-put call :input nil)
         (plist-put call :id (plist-get call :id))
         collect call into calls
         finally do (plist-put info :tool-use calls)))

      (and content-strs (apply #'concat content-strs))))

   (t "")))

(defun gptel-vertex--parse-response-gemini (_backend response info include-text)
  "Parse Gemini response from Vertex AI.

BACKEND is the gptel-vertex backend.
RESPONSE is the JSON response plist.
INFO is the request info plist.  Mutate with metadata.
If INCLUDE-TEXT is non-nil, include response in prompts list."
  (let* ((cand0 (map-nested-elt response '(:candidates 0)))
         (parts (map-nested-elt cand0 '(:content :parts))))
    (plist-put info :stop-reason (plist-get cand0 :finishReason))
    (plist-put info :output-tokens
               (map-nested-elt
                response '(:usageMetadata :candidatesTokenCount)))
    (cl-loop
     for part across parts
     for tx = (plist-get part :text)
     if (and tx (not (eq tx :null)))
     if (plist-get part :thought)
     do (unless (plist-get info :reasoning-block)
          (plist-put info :reasoning-block 'in))
     (plist-put info :reasoning (concat (plist-get info :reasoning) tx))
     else do
     (if (eq (plist-get info :reasoning-block) 'in)
         (plist-put info :reasoning-block t))
     and collect tx into content-strs end
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
           (plist-put
            data :contents
            (vconcat prompts `((:role "model" :parts ,parts)))))))
     (when tool-use
       (plist-put info :tool-use
                  (nconc (plist-get info :tool-use) tool-use)))
     finally return
     (and content-strs (apply #'concat content-strs)))))

;;; Prompt parsing methods

(cl-defmethod gptel--parse-list ((backend gptel-vertex) prompt-list)
  "Parse PROMPT-LIST for Vertex AI.

Handles both simple (list of strings) and advanced (list of
role-content pairs) formats.  Dispatches based on current model."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic (gptel-vertex--parse-list-anthropic backend prompt-list))
      ('google (gptel-vertex--parse-list-gemini backend prompt-list))
      (_ (error "Unsupported publisher for parse-list: %s" publisher)))))

(defun gptel-vertex--parse-list-anthropic (backend prompt-list)
  "Parse PROMPT-LIST for Anthropic models via Vertex AI.

BACKEND is the gptel-vertex backend.
PROMPT-LIST is either simple (strings) or advanced (role-content pairs)."
  (if (consp (car prompt-list))
      (let ((prompts))
        (dolist (entry prompt-list)
          (pcase entry
            (`(prompt . ,msg)
             (push (list :role "user"
                         :content `[(:type "text" :text ,(or (car-safe msg) msg))])
                   prompts))
            (`(response . ,msg)
             (push (list :role "assistant"
                         :content `[(:type "text" :text ,(or (car-safe msg) msg))])
                   prompts))
            (`(tool . ,call)
             (unless (plist-get call :id)
               (plist-put call :id (gptel-vertex--format-tool-id nil)))
             (push
              (list
               :role "assistant"
               :content
               `[(:type "tool_use" :id ,(plist-get call :id)
                  :name ,(plist-get call :name)
                  :input ,(plist-get call :args))])
              prompts)
             (push (gptel--parse-tool-results backend (list (cdr entry))) prompts))))
        (nreverse prompts))
    (cl-loop for text in prompt-list
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant")
                   :content `[(:type "text" :text ,text)]))))

(defun gptel-vertex--parse-list-gemini (backend prompt-list)
  "Parse PROMPT-LIST for Gemini models via Vertex AI.

BACKEND is the gptel-vertex backend.
PROMPT-LIST is either simple (strings) or advanced (role-content pairs)."
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
                   full-prompt))
            (`(tool . ,call)
             (push (list :role "model"
                         :parts (vector `(:functionCall (:name ,(plist-get call :name)
                                                         :args ,(plist-get call :args)))))
                   full-prompt)
             (push (gptel--parse-tool-results backend (list (cdr entry))) full-prompt))))
        (nreverse full-prompt))
    (cl-loop for text in prompt-list
             for role = t then (not role)
             if text
             if role
             collect (list :role "user" :parts `[(:text ,text)]) into prompts
             else collect (list :role "model" :parts `(:text ,text)) into prompts
             finally return prompts)))

(cl-defmethod gptel--parse-buffer ((backend gptel-vertex) &optional max-entries)
  "Parse current buffer for conversation history.

Optional MAX-ENTRIES limits the number of entries parsed.
Dispatches based on current model."
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic (gptel-vertex--parse-buffer-anthropic backend max-entries))
      ('google (gptel-vertex--parse-buffer-gemini backend max-entries))
      (_ (error "Unsupported publisher for parse-buffer: %s" publisher)))))

(defun gptel-vertex--parse-buffer-anthropic (backend max-entries)
  "Parse buffer for Anthropic models via Vertex AI.

BACKEND is the gptel-vertex backend.
MAX-ENTRIES limits the number of conversation turns parsed."
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          (unless (save-excursion (skip-syntax-forward " ") (>= (point) prev-pt))
            (pcase (get-char-property (point) 'gptel)
              ('response
               (when-let* ((content
                            (gptel--trim-prefixes
                             (buffer-substring-no-properties (point) prev-pt))))
                 (when (not (string-blank-p content))
                   (push (list :role "assistant" :content content) prompts))))
              (`(tool . ,id)
               (save-excursion
                 (condition-case nil
                     (let* ((tool-call (read (current-buffer)))
                            (name (plist-get tool-call :name))
                            (arguments (plist-get tool-call :args)))
                       (unless id (setq id (gptel-vertex--format-tool-id nil)))
                       (plist-put tool-call :id id)
                       (plist-put tool-call :result
                                  (string-trim (buffer-substring-no-properties
                                                (point) prev-pt)))
                       (push (gptel--parse-tool-results backend (list tool-call))
                             prompts)
                       (push (list :role "assistant"
                                   :content `[(:type "tool_use" :id ,id :name ,name
                                               :input ,arguments)])
                             prompts))
                   ((end-of-file invalid-read-syntax)
                    (message (format "Could not parse tool-call %s on line %s"
                                     id (line-number-at-pos (point))))))))
              ('ignore)
              ('nil
               (if gptel-track-media
                   (when-let* ((content (gptel-vertex--anthropic-parse-multipart
                                         (gptel--parse-media-links major-mode (point) prev-pt))))
                     (when (> (length content) 0)
                       (push (list :role "user" :content content) prompts)))
                 (when-let* ((content (gptel--trim-prefixes
                                       (buffer-substring-no-properties (point) prev-pt))))
                   (push (list :role "user" :content content) prompts))))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (when-let* ((content (string-trim (buffer-substring-no-properties
                                         (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

(defun gptel-vertex--parse-buffer-gemini (backend max-entries)
  "Parse buffer for Gemini models via Vertex AI.

BACKEND is the gptel-vertex backend.
MAX-ENTRIES limits the number of conversation turns parsed."
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
               (push (list :role "model" :parts (list :text content)) prompts)))
            (`(tool . ,_id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments (plist-get tool-call :args)))
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (gptel--parse-tool-results backend (list tool-call))
                           prompts)
                     (push (list :role "model"
                                 :parts
                                 (vector `(:functionCall (:name ,name
                                                          :args ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call on line %s"
                                   (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (if gptel-track-media
                 (when-let* ((content (gptel-vertex--gemini-parse-multipart
                                       (gptel--parse-media-links major-mode (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (list :role "user" :parts content) prompts)))
               (when-let* ((content (gptel--trim-prefixes
                                     (buffer-substring-no-properties
                                      (point) prev-pt))))
                 (push (list :role "user" :parts `[(:text ,content)]) prompts)))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (push (list :role "user" :parts `[(:text ,content)]) prompts)))
    prompts))

;;; Media handling

(defun gptel-vertex--anthropic-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the Anthropic API format.

The input is an alist of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\")).

The output is a vector of entries in a backend-appropriate format."
  (cl-loop
   for part in parts
   for n upfrom 1
   with last = (length parts)
   with type
   for text = (plist-get part :text)
   for mime = (plist-get part :mime)
   for media = (plist-get part :media)
   if text do
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text))) and
   if text
   collect `(:type "text" :text ,text) into parts-array end
   else if media
   do
   (setq type (cond
               ((equal (substring mime 0 5) "image") "image")
               ((equal mime "application/pdf") "document")
               (t (error (concat "(gptel-vertex) Request aborted: "
                                 "trying to send unsupported MIME type %s")
                         mime))))
   and collect
   `(:type ,type
     :source (:type "base64"
              :media_type ,(plist-get part :mime)
              :data ,(gptel--base64-encode media)))
   into parts-array
   else if (plist-get part :textfile) collect
   `(:type "text"
     :text ,(with-temp-buffer
              (gptel--insert-file-string (plist-get part :textfile))
              (buffer-string)))
   into parts-array
   finally return (vconcat parts-array)))

(defun gptel-vertex--gemini-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the Gemini API format.

The input is an alist of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\")).

The output is a vector of entries in a backend-appropriate format."
  (cl-loop
   for part in parts
   for n upfrom 1
   with last = (length parts)
   for text = (plist-get part :text)
   for media = (plist-get part :media)
   if text do
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text))) and
   if text
   collect (list :text text) into parts-array end
   else if media
   collect
   `(:inline_data
     (:mime_type ,(plist-get part :mime)
      :data ,(gptel--base64-encode media)))
   into parts-array
   else if (plist-get part :textfile)
   collect
   (list :text (with-temp-buffer
                 (gptel--insert-file-string (plist-get part :textfile))
                 (buffer-string)))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod gptel--inject-media ((_backend gptel-vertex) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `gptel-context'.
Dispatches based on current model type."
  (when-let* ((media-list (gptel-context--collect-media))
              (model-name (gptel--model-name gptel-model))
              (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic
       ;; Anthropic format
       (cl-callf (lambda (current)
                   (vconcat
                    (gptel-vertex--anthropic-parse-multipart media-list)
                    (cl-typecase current
                      (string `((:type "text" :text ,current)))
                      (vector current)
                      (t current))))
           (plist-get (car prompts) :content)))
      ('google
       ;; Gemini format
       (cl-callf (lambda (current)
                   (vconcat (gptel-vertex--gemini-parse-multipart media-list)
                            current))
           (plist-get (car prompts) :parts)))
      (_ (error "Unsupported publisher for inject-media: %s" publisher)))))

;;; Utility functions for tool IDs

(defun gptel-vertex--format-tool-id (tool-id)
  "Format TOOL-ID for Vertex AI.

Generates a new ID if TOOL-ID is nil.
Ensures proper prefix based on model type."
  (unless tool-id
    (setq tool-id (substring
                   (md5 (format "%s%s" (random) (float-time)))
                   nil 24)))
  (let* ((model-name (gptel--model-name gptel-model))
         (publisher (gptel-vertex--detect-publisher model-name)))
    (pcase publisher
      ('anthropic
       (if (string-prefix-p "toolu_" tool-id)
           tool-id
         (format "toolu_%s" tool-id)))
      ('google tool-id)
      (_ tool-id))))

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
Anthropic models are supported.

STREAM: Whether to enable response streaming.  Default is t.

PROTOCOL: HTTP protocol, default \"https\".

CURL-ARGS: Additional arguments to pass to curl.

REQUEST-PARAMS: Additional API request parameters.

Example:
-------

 (gptel-make-vertex
  \"Vertex-AI\"
  :project-id \"my-gcp-project\"
  :location \"us-east5\"
  :stream t
  :models \\='(gemini-2.5-flash claude-sonnet-4-5@20250929))"
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
                        (publisher (gptel-vertex--detect-publisher model-name))
                        (publisher-name (gptel-vertex--get-publisher-name publisher))
                        (method (pcase publisher
                                  ('anthropic
                                   (if gptel-stream "streamRawPredict" "rawPredict"))
                                  ('google
                                   (if (and gptel-stream gptel-use-curl)
                                       "streamGenerateContent"
                                     "generateContent"))
                                  (_ "generateContent"))))
                   (format "%s://%s/v1/projects/%s/locations/%s/publishers/%s/models/%s:%s"
                           protocol host project-id location publisher-name model-name method))))))

    (prog1 backend
      ;; Register the backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

(provide 'gptel-vertex)

;;; gptel-vertex.el ends here
