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
 :models '((gemini-2.5-flash :description "Gemini 2.5 Flash")
           (gemini-2.5-pro :description "Gemini 2.5 Pro")
           (claude-sonnet-4-5@20250929 :description "Claude Sonnet 4.5")
           (claude-opus-4-1@20250805 :description "Claude Opus 4.1"))
 :default-model 'claude-sonnet-4-5@20250929)

;; That's it! Now you can use:
;; M-x gptel - to start a chat
;; C-c RET - to send messages in the chat buffer

;;; init-example.el ends here
