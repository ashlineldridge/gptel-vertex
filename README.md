# gptel-vertex

Google Cloud Vertex AI backend for [gptel](https://github.com/karthink/gptel), supporting both Gemini and Claude models through Vertex AI.

## Features

- **Dual Model Support**: Access both Google's Gemini and Anthropic's Claude models through a single interface
- **Automatic Authentication**: Uses `gcloud` CLI authentication with automatic token refresh
- **Global and Regional Endpoints**: Support for both endpoint types
  - **Global endpoints** (recommended): Maximum availability with dynamic routing
  - **Regional endpoints**: Data residency compliance with 10% pricing premium for Claude
- **Streaming Support**: Real-time streaming responses for both model families
- **Latest Models**: Includes all current Claude models with version identifiers
  - Claude Sonnet 4.5, 4, 3.7
  - Claude Opus 4.1, 4, 3
  - Claude Haiku 4.5, 3.5, 3

## Prerequisites

1. Google Cloud SDK installed and configured
2. Authenticated with `gcloud auth login`
3. A GCP project with Vertex AI API enabled
4. Access to Claude models in your region (check [Model Garden](https://cloud.google.com/model-garden))

## Installation

### Using straight.el

```elisp
(straight-use-package
  '(gptel-vertex :type git
                 :host github
                 :repo "yourusername/gptel-vertex"))
```

### Manual Installation

```bash
git clone https://github.com/yourusername/gptel-vertex
```

Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/gptel-vertex")
(require 'gptel-vertex)
```

## Configuration

### Quick Start

```elisp
;; Basic setup with all models and global endpoint
(gptel-vertex-setup-backend
  :project-id "your-project-id")
```

### Claude Models Only (Recommended for Best Performance)

```elisp
;; Claude models with global endpoint for maximum availability
(gptel-vertex-setup-backend
  :project-id "your-project-id"
  :location "global"  ; Recommended for Claude
  :models 'claude)
```

### Regional Configuration (Data Residency)

```elisp
;; Use specific region for data residency requirements
;; Note: 10% pricing premium for Claude models
(gptel-vertex-setup-backend
  :project-id "your-project-id"
  :location "us-east1"  ; or "europe-west1", etc.
  :models 'claude)
```

### Advanced Configuration

```elisp
(setq my-vertex-backend
  (gptel-make-vertex "MyVertex"
    :project-id "my-project-id"
    :location "global"
    :models '((claude-sonnet-4-5@20250929 :description "Most capable")
              (claude-haiku-4-5@20251001 :description "Fast")
              gemini-2.5-pro)
    :stream t
    :curl-args '("--max-time" "100")))

;; Set as default
(setq-default gptel-backend my-vertex-backend)
(setq-default gptel-model 'claude-sonnet-4-5@20250929)
```

## Available Models

### Claude Models (via Anthropic publisher)

| Model ID | Description | Notes |
|----------|-------------|-------|
| `claude-sonnet-4-5@20250929` | Claude Sonnet 4.5 | Most capable balanced model |
| `claude-sonnet-4@20250514` | Claude Sonnet 4 | Previous generation |
| `claude-opus-4-1@20250805` | Claude Opus 4.1 | Most capable model |
| `claude-opus-4@20250514` | Claude Opus 4 | Previous generation |
| `claude-haiku-4-5@20251001` | Claude Haiku 4.5 | Fast and efficient |
| `claude-3-5-haiku@20241022` | Claude Haiku 3.5 | Previous generation |
| `claude-3-haiku@20240307` | Claude Haiku 3 | Base Haiku model |
| `claude-3-7-sonnet@20250219` | Claude Sonnet 3.7 | ⚠️ Deprecated Oct 28, 2025 |
| `claude-3-opus@20240229` | Claude Opus 3 | ⚠️ Deprecated Jun 30, 2025 |

### Gemini Models (via Google publisher)

- `gemini-2.5-flash` - Fast, efficient model
- `gemini-2.5-pro` - More capable model

## Usage

After configuration, use gptel as normal:

```elisp
;; Open a chat buffer
M-x gptel

;; Send a query from any buffer
M-x gptel-send

;; Switch models
(setq gptel-model 'claude-haiku-4-5@20251001)

;; Switch between streaming and non-streaming
(setq gptel-stream t)  ; or nil
```

## Best Practices (Following Anthropic Recommendations)

### 1. Use Global Endpoints for Claude

The global endpoint provides maximum availability and no pricing premium:

```elisp
(gptel-vertex-setup-backend
  :project-id "your-project-id"
  :location "global"  ; Recommended
  :models 'claude)
```

### 2. Model Selection

- **Claude Sonnet 4.5**: Best balance of capability and speed
- **Claude Haiku 4.5**: Fast responses for simpler tasks
- **Claude Opus 4.1**: Maximum capability for complex tasks

### 3. API Compatibility

This implementation follows Anthropic's Vertex AI integration exactly:
- Uses `anthropic_version: "vertex-2023-10-16"` in request body
- Proper endpoint structure: `/publishers/anthropic/models/{model}:rawPredict`
- Streaming uses `:streamRawPredict` for Claude models

### 4. Authentication

Token refresh happens automatically before expiry (default 3500 seconds):

```elisp
;; Customize refresh timing if needed
(setq gptel-vertex-token-refresh-seconds 3000)
```

## Troubleshooting

### Authentication Issues

```bash
# Ensure you're logged in
gcloud auth login

# Check application default credentials
gcloud auth application-default login

# Verify access token
gcloud auth print-access-token
```

### Model Access

```bash
# List available models in your region
gcloud ai models list --region=global --filter="claude"
```

### Region Selection

- Use `"global"` for Claude models (recommended)
- Use specific regions only when required for compliance
- Check [model availability](https://cloud.google.com/vertex-ai/generative-ai/docs/partner-models/use-claude) by region

## Implementation Notes

This package implements:
- Proper URL construction per Anthropic's documentation
- Correct request/response format for both Claude and Gemini
- Automatic publisher detection based on model name
- Support for both streaming endpoints (streamRawPredict for Claude, streamGenerateContent for Gemini)
- Token management with automatic refresh

## License

GPL-3.0

## Contributing

Contributions welcome! Please ensure any changes maintain compatibility with both Claude and Gemini models.
