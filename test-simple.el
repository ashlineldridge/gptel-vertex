;;; test-simple.el --- Simple test for gptel-vertex  -*- lexical-binding: t; -*-

;; This file tests the gptel-vertex implementation

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load gptel-vertex
(require 'gptel-vertex)

;; Test 1: Create backend
(defun test-create-backend ()
  "Test creating a Vertex AI backend."
  (let ((backend (gptel-make-vertex "TestBackend"
                  :project-id "test-project"
                  :location "us-east5")))
    (message "Created backend: %s" (gptel-backend-name backend))
    (message "Project: %s" (gptel-vertex-project-id backend))
    (message "Location: %s" (gptel-vertex-location backend))
    backend))

;; Test 2: Test URL generation
(defun test-url-generation ()
  "Test URL generation for Claude models."
  (let* ((backend (gptel-make-vertex "TestBackend"
                   :project-id "my-project"
                   :location "us-east5"))
         (gptel-backend backend)
         (gptel-model 'claude-3-5-sonnet@20240620))
    
    ;; Test non-streaming URL
    (let ((gptel-stream nil))
      (message "Non-streaming URL: %s" (gptel--url backend)))
    
    ;; Test streaming URL  
    (let ((gptel-stream t))
      (message "Streaming URL: %s" (gptel--url backend)))
    
    backend))

;; Test 3: Test request data formatting
(defun test-request-data ()
  "Test request data formatting."
  (let* ((backend (gptel-make-vertex "TestBackend"
                   :project-id "test-project"
                   :location "us-east5"))
         (gptel-backend backend)
         (gptel-max-tokens 1000)
         (gptel-temperature 0.7)
         (gptel--system-message "You are a helpful assistant")
         (gptel-stream nil))
    
    ;; Test with simple message format
    (let* ((prompts [(:role "user" :content "Hello, how are you?")])
           (request-data (gptel--request-data backend prompts)))
      (message "Request data (simple format):")
      (message "%S" request-data))
    
    ;; Test with parts format (Gemini-style)
    (let* ((prompts [(:role "user" :parts [(:text "Hello from parts format")])])
           (request-data (gptel--request-data backend prompts)))
      (message "Request data (parts format):")
      (message "%S" request-data))
    
    ;; Test with conversation
    (let* ((prompts [(:role "user" :content "What is 2+2?")
                     (:role "assistant" :content "2+2 equals 4.")
                     (:role "user" :content "What about 3+3?")])
           (request-data (gptel--request-data backend prompts)))
      (message "Request data (conversation):")
      (message "%S" request-data))
    
    backend))

;; Test 4: Verify the request body structure
(defun test-verify-structure ()
  "Verify the request body has the required fields."
  (let* ((backend (gptel-make-vertex "TestBackend"
                   :project-id "test-project"
                   :location "us-east5"))
         (gptel-backend backend)
         (gptel-max-tokens 1000)
         (gptel-stream nil)
         (prompts [(:role "user" :content "Test message")])
         (request-data (gptel--request-data backend prompts)))
    
    (message "Checking request structure...")
    
    ;; Check for required fields
    (if (plist-get request-data :anthropic_version)
        (message "✓ Has anthropic_version: %s" (plist-get request-data :anthropic_version))
      (message "✗ Missing anthropic_version"))
    
    (if (plist-get request-data :messages)
        (message "✓ Has messages field with %d messages" 
                 (length (plist-get request-data :messages)))
      (message "✗ Missing messages field"))
    
    (if (plist-get request-data :max_tokens)
        (message "✓ Has max_tokens: %d" (plist-get request-data :max_tokens))
      (message "✗ Missing max_tokens"))
    
    ;; Show the actual messages content
    (when-let ((messages (plist-get request-data :messages)))
      (message "Messages content:")
      (seq-doseq (msg messages)
        (message "  - Role: %s, Content: %s" 
                 (plist-get msg :role)
                 (substring (plist-get msg :content) 0 
                           (min 50 (length (plist-get msg :content)))))))
    
    request-data))

;; Run all tests
(defun run-all-tests ()
  "Run all tests."
  (interactive)
  (message "=====================================")
  (message "Running gptel-vertex tests...")
  (message "=====================================")
  
  (test-create-backend)
  (message "=====================================")
  
  (test-url-generation)
  (message "=====================================")
  
  (test-request-data)
  (message "=====================================")
  
  (test-verify-structure)
  (message "=====================================")
  
  (message "All tests completed!"))

;; Display test information
(message "gptel-vertex test suite loaded!")
(message "Run M-x run-all-tests to execute all tests")
(message "Or run individual tests:")
(message "  M-x test-create-backend")
(message "  M-x test-url-generation")
(message "  M-x test-request-data")
(message "  M-x test-verify-structure")

(provide 'test-simple)
;;; test-simple.el ends here
