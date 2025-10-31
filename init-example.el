;;; init-example.el --- Example configuration for gptel-vertex

;; Add this to your Emacs configuration to use Claude via Vertex AI

;; Load gptel and gptel-vertex
(require 'gptel)
(add-to-list 'load-path "~/dev/home/gptel-vertex")  ; Adjust path as needed
(require 'gptel-vertex)

;; Setup Vertex AI backend with Claude
(gptel-vertex-setup-backend
 :project-id "YOUR-GCP-PROJECT-ID"  ; Replace with your actual project ID
 :location "us-east5"               ; Currently only us-east5 supports Claude
 :models '((claude-3-5-sonnet@20240620 :description "Most capable")
           (claude-3-opus@20240229 :description "Powerful") 
           (claude-3-haiku@20240307 :description "Fast"))
 :default-model 'claude-3-5-sonnet@20240620)

;; That's it! Now you can use:
;; M-x gptel - to start a chat
;; C-c RET - to send messages in the chat buffer

;;; init-example.el ends here
