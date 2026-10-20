# CLAUDE.md

## About

`arcgisutils` is the developer-oriented foundation of the R-ArcGIS bridge. It provides authentication, EsriJSON construction/parsing, geoprocessing service support, portal API wrappers, and shared utilities consumed by downstream packages (`arcgislayers`, `arcgisgeocode`, `arcgisplaces`, etc.).

The package uses Rust (via `extendr`) for performance-critical geometry serialization. The Rust source lives in `src/`.
## Coding Standards

### Use the s7x skill
**Always read and follow the s7x skill before writing code:** https://github.com/JosiahParry/s7x/blob/main/inst/skills/SKILL.md. Build classes, enums, and JSON params with s7x and the existing GP param types. Never write custom serializers or JSON helpers.

Closed value sets are s7x enums, not `rlang::arg_match()`. Use the enum directly, as in `Enum(x)@value`, with no wrapper function.

### No helpers
- Don't write internal helper functions. Inline the logic in the exported function.
- Never create a helper with only one caller. Inline it.
- Don't wrap a request in a thin helper. Each function builds its own `arc_base_req()` call.
- Don't put endpoint-specific parsing in a shared helper. Parse inline per endpoint, as `search_items()` does.
- Before writing anything, check whether arcgisutils already has it, such as `rbind_results()`, `data_frame()` or `infer_item_type()`.
- An asynchronous job (submit, poll, results) uses the `arc_gp_job` structure. Never write a second polling loop.

### Function signatures
- A function that requires a signed-in user takes no `host` argument. It uses `token[["arcgis_host"]]`.
- Every function takes `error_call = rlang::caller_env()` and passes it to every `check_*()`, `cli::cli_abort()`, `arc_base_req()` and `detect_errors()`.
- Never use `invisible()`. Return the parsed response, not `TRUE` or an ID.

### Style
- Never write a one-line `if`. Always use braces, including `x = if (cond) { ... }` inside a list.
- No code comments.

### Documentation voice
- Write docs in active voice, with a named subject doing the action. No middle voice such as "carries" or "receives", and no subjectless "Deletes ...".
- Never edit docs the maintainer wrote.

### Process
- A question is not an instruction. Answer it and stop.
- Never change core classes such as `arc_gp_job` without explicit approval of the exact change.
- Verify API parameter and response names against the Esri docs. Don't guess and add a fallback such as `x %||% y`.
- Implement only what was asked. No extra functions.

### The em dash rule

**` – ` (space–en/em dash–space) is banned in all code, strings, and documentation.**

### Parameter documentation

**Never duplicate parameter docs.** Before writing any `@param`, grep `R/` for `@param <name>`. If it exists anywhere, use `@inheritParams` from that function. Write a new `@param` only when nothing documents it, or when the inherited text would be wrong for that function. Never reword an existing param:

```r
# Correct
#' @inheritParams arc_base_req

# Wrong: do not copy-paste param descriptions
#' @param token an object of class `httr2_token`...
```

`arc_base_req()` is the canonical source for `url`, `token`, `path`, `query`, and `error_call`. Other common inheritance sources: `auth_user` (for `host`, `expiration`), `arc_item` (for `host`, `token`), `cli::cli_abort` (for `call`).

### Parameter validation
**Always use rlang standalone checks** for every parameter in a function. These are imported via `R/import-standalone-types-check.R`:

```r
check_string(x, allow_empty = FALSE)
check_bool(x)
check_number_whole(x, min = 1, max = 100)
check_number_decimal(x)
check_character(x, allow_null = TRUE)
check_data_frame(x)
check_function(x)
obj_check_token(token)        # validates httr2_token with arcgis_host
check_token_has_user(token)   # additionally checks for username field
```

### Error messages
Use `cli::cli_abort()` with `call = error_call` (or `call = rlang::caller_env()`). Use cli inline markup: `{.arg x}`, `{.cls ClassName}`, `{.fn function_name}`, `{.val value}`, `{.code expr}`.

### Changelog
**Always update `NEWS.md`** with a bullet describing what changed. Follow the existing format (version headers, plain bullet points).

### Version bumping
Use `usethis::use_version()` to increment the version. Do not edit `DESCRIPTION` manually.

### Bumping a Rust dependency
The package builds offline from `src/rust/vendor.tar.xz`, so editing `src/rust/Cargo.toml` is not enough. Run all four steps or the build fails with `failed to select a version for the requirement`, which never mentions the vendor:

```r
# 1. edit src/rust/Cargo.toml, then from src/rust/: cargo update
rextendr::vendor_crates()   # 2. regenerate src/rust/vendor.tar.xz
devtools::load_all()        # 3. rebuild src/arcgisutils.so
devtools::test()            # 4. confirm
```

`src/vendor/` is gitignored scratch unpacked from the tarball; the tarball is the committed source of truth.

### R types across the extendr boundary
`extendr` maps `u32`, `u64`, `usize` and `i64` to `REALSXP` (double) because they can exceed `i32::MAX`; only `i32` and smaller map to `INTSXP`. A Rust struct field typed `u32` therefore reaches R as a double and serializes as `4326.0`. The `*_string` conversions go through `serde_json` and are unaffected, so a type discrepancy between the list and string variants points here.

## Architecture

### Request pattern
All HTTP requests flow through `arc_base_req()` which sets the `User-Agent` header via `arc_agent()` and adds `X-Esri-Authorization: Bearer <token>`. Downstream callers chain httr2 verbs onto the result, then call `httr2::req_perform()`. Responses are parsed with `RcppSimdJson::fparse()` and errors detected via `detect_errors()`.

### Authentication
Tokens are `httr2_token` objects that must carry an `arcgis_host` field (set by all `auth_*` functions). Flows:

- `auth_code()` — interactive OAuth2 (recommended for interactive sessions)
- `auth_client()` — client credentials (non-interactive/server)
- `auth_user()` — legacy `generateToken` endpoint
- `auth_key()` — static API key
- `auth_binding()` — delegates to `arcgisbinding`
- `auth_shiny()` — OAuth2 for Shiny apps via `shinyOAuth`

Tokens are stored in a package-level environment via `set_arc_token()` / `arc_token()`. The default token name is `"ARCGIS_TOKEN"`. Multiple named tokens are supported.

The token carries both the host and the username. `obj_check_token()` guarantees `token[["arcgis_host"]]` and `check_token_has_user()` guarantees `token[["username"]]`. Never add a `host` or `user` argument to a function that already requires an authenticated user. Read them off the token instead, or the function will use the `ARCGIS_HOST` env var and silently disagree with the token it is authenticating with.

### EsriJSON serialization
- `as_esri_geometry(sfg)` → geometry JSON string (single geometry)
- `as_features(sf/data.frame/sfc)` → list of feature objects
- `as_esri_features(sfc)` → JSON string of feature array
- `as_featureset(sf/data.frame/sfc)` → JSON string (full featureSet)
- `as_esri_featureset(sf/data.frame/sfc)` → JSON string (full featureSet)

Geometry conversion is implemented in Rust. `parse_esri_json()` is the inverse: converts a featureSet JSON string back to `data.frame` or `sf`.

### Field type mapping
`as_fields()` infers Esri field types from R column types. `fields_as_ptype_df()` creates empty prototype `data.frame`s from a fields list. The bidirectional mapping lives in `vec_mapping` in `R/esri-field-mapping.R`.

### Geoprocessing services
`arc_gp_job` is an R6 class wrapping async GP service workflows. Create with `new_gp_job(url, params)`, start with `$start()`, poll with `$status`, wait with `$await()`, retrieve with `$results`. Parameters must be `arc_form_params` (S7 class ensuring all values are scalar strings). Typed GP parameter helpers follow the pattern `gp_linear_unit()` / `as_gp_linear_unit()` / `parse_gp_linear_unit()`.

### Portal API
Functions in `R/sharing.R`, `R/portal-self.R`, `R/portal-types.R`, `R/search.R`, and `R/user-*.R` wrap the ArcGIS sharing REST API. Most accept `host = arc_host()` and `token = arc_token()` with the token fetched from the environment by default. Pagination is handled by `arc_paginate_req()` which wraps `httr2::req_perform_iterative()`.

### S7 / R6 classes
- `arc_gp_job` — R6, wraps a GP service job lifecycle
- `arc_form_params` — S7, validated named list of scalar strings for GP form bodies
- `arc_job_status` — S7, validated GP job status string
- `gp_linear_unit` (`GPLinearUnit`) — S7, distance + units
- `gp_areal_unit` (`GPArealUnit`) — S7, area + units
- `item_type` / `item_keyword` — S7 wrappers validating portal item type/keyword strings

### Environment variables
| Variable | Purpose |
|---|---|
| `ARCGIS_HOST` | Portal host (default: `https://www.arcgis.com`) |
| `ARCGIS_CLIENT` | OAuth2 client ID |
| `ARCGIS_SECRET` | OAuth2 client secret |
| `ARCGIS_API_KEY` | API key for `auth_key()` |
| `ARCGIS_USER` | Username for `auth_user()` |
| `ARCGIS_PASSWORD` | Password for `auth_user()` |
| `ARCGIS_REDIRECT_URI` | Redirect URI for `auth_shiny()` |
