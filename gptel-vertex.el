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
;;     :project-id "your-project-id")

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
    
    ;; Update publisher in backend
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
            (when (boundp 'gptel-stream)
              (plist-put request-body :stream (if gptel-stream t :json-false)))
            
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

;;; Response parsing
(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info)
  "Parse Vertex AI RESPONSE and call INFO's callback."
  (let* ((json-object-type 'plist)
         (json-response (condition-case nil
                            (json-read-from-string response)
                          (json-readtable-error nil)))
         (publisher (gptel-vertex-publisher backend)))
    
    (if json-response
        (if (equal publisher "anthropic")
            ;; Handle Claude response format
            (if-let ((content (plist-get json-response :content)))
                ;; Claude returns content as an array
                (if (vectorp content)
                    (mapconcat (lambda (item)
                                 (plist-get item :text))
                               content "")
                  content)
              ;; Error handling
              (or (plist-get json-response :error)
                  ""))
          ;; Handle Gemini response format
          (if-let ((candidates (plist-get json-response :candidates)))
              (let ((candidate (aref candidates 0)))
                (if-let ((content (plist-get candidate :content)))
                    (let ((parts (plist-get content :parts)))
                      (if (vectorp parts)
                          (mapconcat (lambda (part)
                                       (plist-get part :text))
                                     parts "")
                        ""))
                  ""))
            ""))
      "")))

;;; URL construction
(cl-defmethod gptel--url ((backend gptel-vertex))
  "Get URL for BACKEND."
  (let* ((project-id (gptel-vertex-project-id backend))
         (location (gptel-vertex-location backend))
         (model-name (gptel--model-name gptel-model))
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
            location project-id location publisher model-name method)))

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

;;; Backend constructor
(cl-defun gptel-make-vertex
    (name &key project-id
          (location gptel-vertex-default-location)
          (host (format "%s-aiplatform.googleapis.com" location))
          (header (lambda ()
                    (let ((token (gptel-vertex--ensure-auth gptel-backend)))
                      `(("Authorization" . ,(format "Bearer %s" token))))))
          (models '((gemini-2.5-flash :description "Gemini 2.5 Flash")
                    (gemini-2.5-pro :description "Gemini 2.5 Pro")
                    (claude-sonnet-4-5@20250929 :description "Claude Sonnet 4.5")
                    (claude-opus-4-1@20250805 :description "Claude Opus 4.1")))
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
         (publisher (if (string-match-p "claude" model-name) "anthropic" "google")))
    
    (let ((backend (gptel--make-vertex
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
                    :url #'gptel--url)))
      (prog1 backend
        (setf (alist-get name gptel--known-backends nil nil #'equal)
              backend)))))

;;; Convenience setup function
(cl-defun gptel-vertex-setup-backend
    (&key (name "VertexAI")
          project-id
          (location gptel-vertex-default-location)
          (models '((gemini-2.5-flash :description "Gemini 2.5 Flash")
                    (gemini-2.5-pro :description "Gemini 2.5 Pro")
                    (claude-sonnet-4-5@20250929 :description "Claude Sonnet 4.5")
                    (claude-opus-4-1@20250805 :description "Claude Opus 4.1")))
          (stream t)
          (default-model 'claude-sonnet-4-5@20250929))
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
