## Purpose

This file gives concise, actionable guidance for AI coding agents (and humans) working on the GXPAinterface R package. Focus areas: how the code is organized, the key integration points with the GXPA web API, project-specific conventions, and common developer workflows.

## Big-picture architecture

- Language & packaging: an R package (see `DESCRIPTION`). Primary code lives in `R/` and the package uses roxygen2 for documentation.
- Core role: a thin client for the GXPA web application. Functions assemble HTTP requests (GET/POST), handle CSRF cookies, and parse JSON or CSV responses.
- Key files:
  - `R/read_GXPA.R` — GET helpers and parsers (functions: `get_data_from_gxpa`, `get_series_info_from_gxpa`, `get_info_from_url`).
  - `R/auth_config.R` — login flow and CSRF/cookie extraction (`login_and_get_user_cookie`, `get_cookie_value`).
  - `R/uploadfile.R` — session lifecycle and upload: `send_file_to_session`, `begin_new_session`, `dry_run_session`, `load_session`.
  - `R/prepare_data.R` — file prep helpers: `make_series_info_file`, `check_files_for_GXPA` (validates expression + metadata files and expected naming conventions).
  - `inst/rmarkdown/templates/xpress_gxpa_workflow/` — RMarkdown template used for Xpress-to-GXPA workflows.

## Authentication & configuration

- Environment variables the package reads (used throughout): `GXPA_SERVER`, `GXPA_TOKEN`, `GXPA_USERNAME`, `GXPA_PASSWORD`.
- Default server fallback: `https://geneatlas.redda.bms.com/` is used when `GXPA_SERVER` is unset.
- Two auth paths:
  1. Token-based API access (header `Authorization: Token <GXPA_TOKEN>`) — used by GET endpoints via `get_info_from_url()`.
  2. Cookie-based login for session actions (POSTs that require CSRF token and `sessionid` cookie) — handled in `login_and_get_user_cookie()`.

## Common request/response patterns to follow

- Use `httr::RETRY()` (already present) for transient requests; prefer existing patterns (timeout, retries).
- Inspect response text for sentinel phrases (these exact strings are used to determine success/failure):
  - "CSRF token missing or incorrect"
  - "Not logged in"
  - "Uploaded files have been saved"
  - "Made new session [<name>]"
  - "Select a valid choice"
- API endpoints are assembled as string concatenation (see `get_data_from_gxpa()` building `series_api/series_data_view/` URLs). When modifying endpoints keep consistent query param patterns (e.g., `?sample_labels=1&transpose=1&no_feat_info=1&name=`).

## File/upload conventions

- Common upload filenames the server expects (see `send_file_to_session()`):
  - Expression: `expr.expr.txt`
  - Samples/metadata: `expr.samples.csv`
  - Series info: `expr.series_info.csv` / `GEO.series_info.csv`
- `check_files_for_GXPA()` enforces strict rules: first metadata column must be `_id` or blank; no duplicate sample names; metadata column names must be valid R variable names.

## Example scripts

- Example RMarkdown scripts are provided in the `inst/examples/` folder. These are not tests, but user-facing workflow examples (e.g., `upload_via_interface.Rmd`, `local_processing.Rmd`).
- The `README.Rmd` and rendered README reference these examples and describe their use:
  - See the "More Generic Examples" section in the README for a list and descriptions.
  - Users can copy example scripts to their own folder with `GXPAinterface::use_example_rmd(to = "my_examples_folder", overwrite = FALSE)`.

When adding new example scripts, place them in `inst/examples/` and update the README as needed.

## Tests & developer workflow

- Tests are in `tests/testthat/`. DESCRIPTION sets `Config/testthat/edition: 3`.
- Useful local commands (PowerShell / Windows):

```powershell
# run package tests
Rscript -e "devtools::test()"

# run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-read_GXPA.R')"

# run R CMD check (full package check)
R CMD check .
```

- Use `remotes::install_github('wfulp/GXPAinterface')` for installing the package as users would.

## Debugging tips (project-specific)

- To inspect failing requests, print `httr::content(resp, as = 'text')` — many functions already pattern-match plain text in responses for errors.
- CSRF/cookie logic: `login_and_get_user_cookie()` does a GET to `register/login` to extract `csrftoken`. When you change login flow, keep `get_cookie_value()` behavior intact.
- For uploads and session flows, test using the demo endpoints (the default `GXPA_SERVER` is populated in code). Look for the exact server response messages listed above; code branches depend on them.

## Conventions & style points to preserve

- Functions typically use `stop()` for hard errors and `message()`/`warning()` for recoverable issues — tests and callers depend on these semantics.
- Roxygen documentation is present and used for exports; keep `@export` tags and examples updated when changing signatures.

## Where to change things

- API URL changes: update `R/read_GXPA.R` and `R/uploadfile.R` where strings like `series_api/` and `sessions/` are assembled.
- Auth flow: `R/auth_config.R` for cookie-based login; `R/read_GXPA.R` for token-based GETs.

---
If any of the above is unclear or you want more examples (e.g., sample test to add that asserts a specific server error string), tell me which area to expand and I will iterate.
