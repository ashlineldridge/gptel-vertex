;;; init-example.el --- Example configuration for gptel-vertex

;; Add this to your Emacs configuration to use Claude and Gemini via Vertex AI

;; Load gptel and gptel-vertex
(require 'gptel)
(add-to-list 'load-path "~/dev/home/gptel-vertex")  ; Adjust path as needed
(require 'gptel-vertex)

;; Setup Vertex AI backend with Claude and Gemini
(gptel-vertex-setup-backend
 :project-id "YOUR-GCP-PROJECT-ID"  ; Replace with your actual project ID
 :location "us-east5"               ; Currently the main region for Claude
 :models '((gemini-2.0-flash-exp :description "Gemini 2.5 Flash")
           (gemini-2.0-pro-exp :description "Gemini 2.5 Pro")
           (claude-3-5-sonnet-v2@20241022 :description "Claude Sonnet 4.5")
           (claude-3-opus@20240229 :description "Claude Opus 4.1"))
 :default-model 'claude-3-5-sonnet-v2@20241022)

;; That's it! Now you can use:
;; M-x gptel - to start a chat
;; C-c RET - to send messages in the chat buffer

;;; init-example.el ends here
