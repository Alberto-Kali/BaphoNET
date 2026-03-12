# BaphoNET
### Async knowledge extraction API for links and text-first documents

BaphoNET now targets a text-first pipeline:

- ingest URLs and local files
- normalize `.html`, `.md`, `.txt`, `.epub`
- group sources by topic
- produce cleaned Markdown knowledge bundles
- persist each job as filesystem artifacts
- optionally refine semantic output through an external inference service

---

## Runtime

Current HTTP API:

- `POST /jobs`
- `GET /jobs/:jobId`
- `GET /jobs/:jobId/result`
- `GET /jobs/:jobId/artifacts/:name`
- `POST /transform` as compatibility endpoint for the old single-source flow

Environment variables:

- `BAPHONET_STORAGE_ROOT`
- `BAPHONET_MAX_JOB_SIZE`
- `BAPHONET_WORKER_CONCURRENCY`
- `BAPHONET_INFERENCE_BASE_URL`
- `BAPHONET_INFERENCE_MODEL`
- `BAPHONET_INFERENCE_CONTEXT_TOKENS`
- `BAPHONET_INFERENCE_TIMEOUT_SECONDS`

## Building

The repository currently expects a Haskell toolchain with project dependencies installed. In this environment `stack` and `cabal` are missing, so dependency bootstrap is required before building.

---

## Storage Layout

Each processed job is stored under the configured storage root:

- `job-<n>/job.json`
- `job-<n>/artifacts/manifest.json`
- `job-<n>/groups/<group-id>/knowledge.md`
- `job-<n>/groups/<group-id>/metadata.json`

## Next Phases

- OCR/media extraction adapters
- audio/STT pipeline
- template rendering for `docx` and `pptx`

---

## Author

Copyright Alberto-Kali 2026
