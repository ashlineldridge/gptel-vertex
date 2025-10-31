;;; test-gptel-vertex.el --- Test configuration for gptel-vertex  -*- lexical-binding: t; -*-

;; This file provides test configurations for gptel-vertex

;;; Commentary:
;; Load this file to test different configurations of gptel-vertex
;; Usage: emacs -Q -l test-gptel-vertex.el

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Mock gptel functions for testing
(defvar gptel-backend nil)
(defvar gptel-model nil)
(defvar gptel-stream t)
(defvar gptel-use-curl t)
(defvar gptel--system-message nil)
(defvar gptel-temperature nil)
(defvar gptel-max-tokens nil)
(defvar gptel--request-params nil)
(defvar gptel--known-backends nil)

(defun gptel--process-models (models)
  "Mock function to process models."
  models)

(defun gptel--model-name (model)
  "Mock function to get model name."
  (if (symbolp model)
      (symbol-name model)
    (symbol-name (car model))))

(defun gptel--merge-plists (&rest plists)
  "Mock function to merge plists."
  (let ((result nil))
    (dolist (plist plists)
      (while plist
        (setq result (plist-put result (car plist) (cadr plist)))
        (setq plist (cddr plist))))
    result))

(defun gptel--model-request-params (model)
  "Mock function for model request params."
  nil)

(defun gptel-backend-request-params (backend)
  "Mock function for backend request params."
  nil)

(defun gptel-backend-header (backend)
  "Mock function for backend header."
  nil)

(cl-defstruct gptel-backend
  name host header protocol stream endpoint
  key models url request-params)

;; Load the actual gptel-vertex package
(require 'gptel-vertex)

;; Test configurations
(defun test-basic-setup ()
  "Test basic setup with default settings."
  (interactive)
  (let ((backend (gptel-vertex-setup-backend
                  :project-id "test-project")))
    (message "Basic setup completed!")
    (message "Backend: %s" backend)
    (message "Default model: %s" gptel-model)
    backend))

(defun test-claude-only ()
  "Test Claude-only configuration with global endpoint."
  (interactive)
  (let ((backend (gptel-vertex-setup-backend
                  :project-id "test-project"
                  :location "global"
                  :models 'claude)))
    (message "Claude-only setup completed!")
    (message "Available models: %s" (gptel-backend-models backend))
    backend))

(defun test-regional-setup ()
  "Test regional endpoint configuration."
  (interactive)
  (let ((backend (gptel-vertex-setup-backend
                  :project-id "test-project"
                  :location "us-east1"
                  :models 'all)))
    (message "Regional setup completed!")
    (message "Location: us-east1")
    (message "Host: %s" (gptel-backend-host backend))
    backend))

(defun test-url-generation ()
  "Test URL generation for different models and streaming modes."
  (interactive)
  (let* ((backend (gptel-make-vertex "TestBackend"
                   :project-id "test-project"
                   :location "global"
                   :models '(claude-sonnet-4-5@20250929
                            gemini-2.5-pro))))
    
    ;; Test Claude non-streaming
    (setq gptel-model 'claude-sonnet-4-5@20250929)
    (setq gptel-stream nil)
    (setq gptel-backend backend)
    (message "Claude non-streaming URL: %s" 
             (funcall (gptel-vertex-url backend)))
    
    ;; Test Claude streaming
    (setq gptel-stream t)
    (message "Claude streaming URL: %s"
             (funcall (gptel-vertex-url backend)))
    
    ;; Test Gemini non-streaming
    (setq gptel-model 'gemini-2.5-pro)
    (setq gptel-stream nil)
    (message "Gemini non-streaming URL: %s"
             (funcall (gptel-vertex-url backend)))
    
    ;; Test Gemini streaming
    (setq gptel-stream t)
    (setq gptel-use-curl t)
    (message "Gemini streaming URL: %s"
             (funcall (gptel-vertex-url backend)))
    
    backend))

(defun test-request-formatting ()
  "Test request data formatting for Claude and Gemini."
  (interactive)
  (let ((backend (gptel-make-vertex "TestBackend"
                  :project-id "test-project"
                  :location "global"
                  :models '(claude-sonnet-4-5@20250929
                           gemini-2.5-pro))))
    
    ;; Test Claude request format
    (setf (gptel-vertex-publisher backend) "anthropic")
    (setq gptel--system-message "You are a helpful assistant")
    (setq gptel-temperature 0.7)
    (setq gptel-max-tokens 1000)
    (setq gptel-stream t)
    
    (let* ((prompts '((:role "user" :parts [(:text "Hello")])))
           (request-data (gptel--request-data backend prompts)))
      (message "Claude request format:")
      (message "%S" request-data))
    
    ;; Test Gemini request format
    (setf (gptel-vertex-publisher backend) "google")
    (let* ((prompts '((:role "user" :parts [(:text "Hello")])))
           (request-data (gptel--request-data backend prompts)))
      (message "Gemini request format:")
      (message "%S" request-data))
    
    backend))

(defun run-all-tests ()
  "Run all test functions."
  (interactive)
  (message "Starting gptel-vertex tests...")
  (message "========================================")
  
  (test-basic-setup)
  (message "========================================")
  
  (test-claude-only)
  (message "========================================")
  
  (test-regional-setup)
  (message "========================================")
  
  (test-url-generation)
  (message "========================================")
  
  (test-request-formatting)
  (message "========================================")
  
  (message "All tests completed!"))

;; Display available test functions
(message "gptel-vertex test suite loaded!")
(message "Available test functions:")
(message "  M-x test-basic-setup        - Test basic configuration")
(message "  M-x test-claude-only        - Test Claude-only setup")
(message "  M-x test-regional-setup     - Test regional endpoint")
(message "  M-x test-url-generation     - Test URL generation")
(message "  M-x test-request-formatting - Test request formatting")
(message "  M-x run-all-tests           - Run all tests")

(provide 'test-gptel-vertex)
;;; test-gptel-vertex.el ends here
