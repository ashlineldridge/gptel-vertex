# gptel-vertex

Google Cloud Vertex AI backend for [gptel](https://github.com/karthink/gptel).

## Features

- Support for all Gemini models (1.5 Pro, 1.5 Flash, 2.0 Flash Experimental)
- Support for Claude models via Vertex AI
- Automatic token refresh using gcloud CLI
- Streaming support
- Tool/function calling support
- Multi-modal support (images, documents) for Gemini models

## Prerequisites

1. Install Google Cloud SDK:
```bash
# macOS
brew install google-cloud-sdk

# Or download from
# https://cloud.google.com/sdk/docs/install
```

2. Authenticate with Google Cloud:
```bash
gcloud auth login
gcloud auth application-default login
```

3. Set your GCP project:
```bash
gcloud config set project YOUR_PROJECT_ID
```

4. Enable Vertex AI API:
```bash
gcloud services enable aiplatform.googleapis.com
```

## Installation

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/gptel-vertex.git ~/dev/home/gptel-vertex
```

2. Add to your Emacs configuration:
```elisp
(use-package gptel-vertex
  :after gptel
  :load-path "~/dev/home/gptel-vertex"
  :config
  ;; Configure with your project ID
  (gptel-vertex-setup-backend
    :project-id "your-project-id"
    :location "us-central1"
    :models 'gemini  ; or 'claude or 'both
    :stream t))
```

## Configuration Examples

### Basic Gemini Setup
```elisp
(gptel-vertex-setup-backend
  :project-id "my-gcp-project"
  :location "us-central1"
  :models 'gemini
  :stream t)
```

### Claude Models Setup
```elisp
(gptel-vertex-setup-backend
  :name "Vertex-Claude"
  :project-id "my-gcp-project"
  :location "us-east5"  ; Required for Claude
  :models 'claude
  :stream t)
```

### Both Gemini and Claude
```elisp
(gptel-vertex-setup-backend
  :name "Vertex-All"
  :project-id "my-gcp-project"
  :location "us-central1"
  :models 'both
  :stream t)
```

### Advanced Configuration
```elisp
(gptel-make-vertex "Vertex-Custom"
  :project-id "my-gcp-project"
  :location "us-central1"
  :models '((gemini-1.5-pro-002
             :description "Best for complex reasoning"
             :capabilities (tool-use json media))
            (gemini-1.5-flash-002
             :description "Fast and efficient"))
  :stream t
  :request-params '(:temperature 0.7
                    :maxOutputTokens 2000))
```

## Available Models

### Gemini Models
- `gemini-1.5-pro-002` - Most capable Gemini 1.5 model
- `gemini-1.5-flash-002` - Fast and efficient
- `gemini-2.0-flash-exp` - Experimental Gemini 2.0

### Claude Models (via Vertex AI)
- `claude-3-5-sonnet-v2@20241022` - Latest Claude 3.5 Sonnet
- `claude-3-5-sonnet@20240620` - Claude 3.5 Sonnet
- `claude-3-opus@20240229` - Most capable Claude 3
- `claude-3-sonnet@20240229` - Balanced Claude 3
- `claude-3-haiku@20240307` - Fast Claude 3

## Supported Regions

Not all models are available in all regions:

- **Gemini**: Most regions (us-central1, europe-west4, asia-northeast1, etc.)
- **Claude**: Limited regions (us-east5 primarily)

See `gptel-vertex-regions` for a complete list.

## Usage

After configuration, use gptel as normal:

```elisp
;; Start a chat
M-x gptel

;; Send a query programmatically
(gptel-request
  "Explain quantum computing"
  :system "You are a helpful assistant"
  :callback (lambda (response info)
              (when response
                (insert response))))
```

## Troubleshooting

### Authentication Issues
```bash
# Refresh authentication
gcloud auth application-default login

# Verify credentials
gcloud auth list

# Check project
gcloud config get-value project
```

### Token Expiry
Tokens are automatically refreshed. The default refresh is 100 seconds before expiry (configurable via `gptel-vertex-token-refresh-seconds`).

### Region Issues
If a model isn't available in your region, try:
- us-central1 for Gemini models
- us-east5 for Claude models

## License

GPL-3.0-or-later
