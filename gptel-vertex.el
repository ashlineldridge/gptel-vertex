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
;; It supports Claude models via the Vertex AI API.
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
For Claude models, us-east5 is currently the only supported region."
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
  token-expiry)

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

;;; Request handling for Claude via Vertex
(cl-defmethod gptel--request-data ((backend gptel-vertex) prompts)
  "JSON encode PROMPTS for sending to Vertex AI Claude."
  ;; Extract text content from prompts - handle both simple and complex formats
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
    
    ;; Build the request body
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
      
      request-body)))

;;; Response parsing
(cl-defmethod gptel--parse-response ((backend gptel-vertex) response info)
  "Parse Vertex AI RESPONSE and call INFO's callback."
  (let* ((json-object-type 'plist)
         (json-response (condition-case nil
                            (json-read-from-string response)
                          (json-readtable-error nil))))
    (if json-response
        ;; Handle Claude response format
        (if-let ((content (plist-get json-response :content)))
            ;; Claude returns content as an array
            (if (vectorp content)
                (mapconcat (lambda (item)
                             (plist-get item :text))
                           content "")
              content)
          ;; Fallback for simple text responses
          (or (plist-get json-response :text)
              (plist-get json-response :candidates)
              ""))
      "")))

(cl-defmethod gptel--parse-list ((backend gptel-vertex) prompt-list)
  "Parse PROMPT-LIST for Vertex AI."
  ;; For Claude, we need to convert the prompt list to the expected format
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
    ;; Already in the right format
    prompt-list))

;;; URL construction
(cl-defmethod gptel--url ((backend gptel-vertex))
  "Get URL for BACKEND."
  (let* ((project-id (gptel-vertex-project-id backend))
         (location (gptel-vertex-location backend))
         (model-name (gptel--model-name gptel-model))
         (method (if gptel-stream "streamRawPredict" "rawPredict")))
    (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/anthropic/models/%s:%s"
            location project-id location model-name method)))

;;; Backend constructor
(cl-defun gptel-make-vertex
    (name &key project-id
          (location gptel-vertex-default-location)
          (host (format "%s-aiplatform.googleapis.com" location))
          (header (lambda ()
                    (let ((token (gptel-vertex--ensure-auth gptel-backend)))
                      `(("Authorization" . ,(format "Bearer %s" token))))))
          (models '((claude-3-5-sonnet@20240620 :description "Most capable model")
                    (claude-3-opus@20240229 :description "Powerful model for complex tasks")
                    (claude-3-haiku@20240307 :description "Fast and efficient")))
          (stream t)
          (protocol "https")
          (endpoint (format "/v1/projects/%s/locations/%s/publishers/anthropic/models"
                           project-id location))
          curl-args
          request-params)
  "Create a Vertex AI backend for Claude models.

NAME is a unique name for this backend.

Keyword arguments:

PROJECT-ID (required): Your GCP project ID.

LOCATION: GCP region (default: us-east5).

MODELS: List of available models.

STREAM: Enable streaming responses (default: t).

CURL-ARGS: Additional arguments for curl requests.

REQUEST-PARAMS: Additional model parameters."
  (unless project-id
    (error "PROJECT-ID is required for Vertex AI backend"))
  
  (let ((backend (gptel--make-vertex
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :project-id project-id
                  :location location
                  :url #'gptel--url)))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

;;; Convenience setup function
(cl-defun gptel-vertex-setup-backend
    (&key (name "VertexAI-Claude")
          project-id
          (location gptel-vertex-default-location)
          (models '((claude-3-5-sonnet@20240620 :description "Most capable model")
                    (claude-3-opus@20240229 :description "Powerful model for complex tasks")
                    (claude-3-haiku@20240307 :description "Fast and efficient")))
          (stream t)
          (default-model 'claude-3-5-sonnet@20240620))
  "Setup a Vertex AI backend for gptel.

This is a convenience function that creates a backend and sets it as default.

PROJECT-ID (required): Your GCP project ID.

NAME: Backend name (default: \"VertexAI-Claude\").

LOCATION: GCP region (default: us-east5).

MODELS: List of available models.

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
