;;; example-config.el --- Example configuration for gptel-vertex

;; This file shows example configurations for using gptel-vertex
;; Copy relevant sections to your Emacs init file

;;; Basic Configuration (Recommended)

;; Load gptel and gptel-vertex
(require 'gptel)
(require 'gptel-vertex)

;; Simple setup with Claude models and global endpoint
(gptel-vertex-setup-backend
 :project-id "your-gcp-project-id"  ; Replace with your actual project ID
 :location "global"                 ; Use global endpoint for best availability
 :models 'claude)                   ; Claude models only

;;; Alternative Configurations

;; 1. All models (Claude + Gemini)
(gptel-vertex-setup-backend
 :project-id "your-gcp-project-id"
 :models 'all)

;; 2. Regional endpoint (for data residency requirements)
;; Note: 10% pricing premium for Claude models
(gptel-vertex-setup-backend
 :project-id "your-gcp-project-id"
 :location "us-east1"  ; or "europe-west1", "asia-northeast1", etc.
 :models 'claude)

;; 3. Custom model selection
(gptel-vertex-setup-backend
 :project-id "your-gcp-project-id"
 :location "global"
 :models '((claude-sonnet-4-5@20250929 :description "Most capable")
           (claude-haiku-4-5@20251001 :description "Fast")
           gemini-2.5-flash))

;;; Advanced Configuration

(defun my/setup-vertex-ai ()
  "My custom Vertex AI setup."
  (let ((backend (gptel-make-vertex "MyVertexAI"
                  :project-id "your-gcp-project-id"
                  :location "global"
                  :models '((claude-sonnet-4-5@20250929)
                           (claude-haiku-4-5@20251001)
                           (gemini-2.5-pro))
                  :stream t
                  :curl-args '("--max-time" "120"))))
    
    ;; Set as default backend
    (setq-default gptel-backend backend)
    
    ;; Set default model
    (setq-default gptel-model 'claude-sonnet-4-5@20250929)
    
    ;; Optional: Configure other gptel settings
    (setq gptel-stream t)           ; Enable streaming
    (setq gptel-max-tokens 4096)    ; Max tokens for response
    (setq gptel-temperature 0.7)    ; Temperature (0-1)
    
    backend))

;; Call the setup function
(my/setup-vertex-ai)

;;; Useful Helper Functions

(defun my/switch-to-claude-haiku ()
  "Switch to Claude Haiku for faster responses."
  (interactive)
  (setq gptel-model 'claude-haiku-4-5@20251001)
  (message "Switched to Claude Haiku 4.5"))

(defun my/switch-to-claude-sonnet ()
  "Switch to Claude Sonnet for balanced performance."
  (interactive)
  (setq gptel-model 'claude-sonnet-4-5@20250929)
  (message "Switched to Claude Sonnet 4.5"))

(defun my/switch-to-gemini ()
  "Switch to Gemini Pro."
  (interactive)
  (setq gptel-model 'gemini-2.5-pro)
  (message "Switched to Gemini 2.5 Pro"))

;; Optional key bindings
(global-set-key (kbd "C-c g c") 'gptel)              ; Open chat
(global-set-key (kbd "C-c g s") 'gptel-send)         ; Send query
(global-set-key (kbd "C-c g h") 'my/switch-to-claude-haiku)
(global-set-key (kbd "C-c g o") 'my/switch-to-claude-sonnet)
(global-set-key (kbd "C-c g g") 'my/switch-to-gemini)

;;; Troubleshooting

;; If you get authentication errors, ensure you're logged in:
;; Run in terminal: gcloud auth login
;; Run in terminal: gcloud auth application-default login

;; To test your setup:
;; 1. M-x gptel to open a chat buffer
;; 2. Type a message and press C-c RET to send

;;; example-config.el ends here
