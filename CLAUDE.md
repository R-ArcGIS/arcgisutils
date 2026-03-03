# CLAUDE.md

## About

`arcgisutils` is the developer-oriented foundation of the R-ArcGIS
bridge. It provides authentication, EsriJSON construction/parsing,
geoprocessing service support, portal API wrappers, and shared utilities
consumed by downstream packages (`arcgislayers`, `arcgisgeocode`,
`arcgisplaces`, etc.).

The package uses Rust (via `extendr`) for performance-critical geometry
serialization. The Rust source lives in `src/`. \## Coding Standards

### The em dash rule

**`–` (space–en/em dash–space) is banned in all code, strings, and
documentation.**

### Parameter documentation

**Never duplicate parameter docs.** Always use `@inheritParams` pointing
to a function that already documents those parameters:

``` r
# Correct
#' @inheritParams arc_base_req

# Wrong: do not copy-paste param descriptions
#' @param token an object of class `httr2_token`...
```

[`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
is the canonical source for `url`, `token`, `path`, `query`, and
`error_call`. Other common inheritance sources: `auth_user` (for `host`,
`expiration`), `arc_item` (for `host`, `token`),
[`cli::cli_abort`](https://cli.r-lib.org/reference/cli_abort.html) (for
`call`).

### Parameter validation

**Always use rlang standalone checks** for every parameter in a
function. These are imported via `R/import-standalone-types-check.R`:

``` r
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

Use [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
with `call = error_call` (or `call = rlang::caller_env()`). Use cli
inline markup: `{.arg x}`, `{.cls ClassName}`, `{.fn function_name}`,
`{.val value}`, `{.code expr}`.

### Changelog

**Always update `NEWS.md`** with a bullet describing what changed.
Follow the existing format (version headers, plain bullet points).

### Version bumping

Use `usethis::use_version()` to increment the version. Do not edit
`DESCRIPTION` manually.

## Architecture

### Request pattern

All HTTP requests flow through
[`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
which sets the `User-Agent` header via
[`arc_agent()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_agent.md)
and adds `X-Esri-Authorization: Bearer <token>`. Downstream callers
chain httr2 verbs onto the result, then call
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html).
Responses are parsed with
[`RcppSimdJson::fparse()`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html)
and errors detected via
[`detect_errors()`](https://github.com/R-ArcGIS/arcgisutils/reference/detect_errors.md).

### Authentication

Tokens are `httr2_token` objects that must carry an `arcgis_host` field
(set by all `auth_*` functions). Flows:

- [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  — interactive OAuth2 (recommended for interactive sessions)
- [`auth_client()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  — client credentials (non-interactive/server)
- [`auth_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  — legacy `generateToken` endpoint
- [`auth_key()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  — static API key
- [`auth_binding()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  — delegates to `arcgisbinding`
- [`auth_shiny()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md)
  — OAuth2 for Shiny apps via `shinyOAuth`

Tokens are stored in a package-level environment via
[`set_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
/
[`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md).
The default token name is `"ARCGIS_TOKEN"`. Multiple named tokens are
supported.

### EsriJSON serialization

- `as_esri_geometry(sfg)` → geometry JSON string (single geometry)
- `as_features(sf/data.frame/sfc)` → list of feature objects
- `as_esri_features(sfc)` → JSON string of feature array
- `as_featureset(sf/data.frame/sfc)` → JSON string (full featureSet)
- `as_esri_featureset(sf/data.frame/sfc)` → JSON string (full
  featureSet)

Geometry conversion is implemented in Rust.
[`parse_esri_json()`](https://github.com/R-ArcGIS/arcgisutils/reference/parse_esri_json.md)
is the inverse: converts a featureSet JSON string back to `data.frame`
or `sf`.

### Field type mapping

[`as_fields()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
infers Esri field types from R column types.
[`fields_as_ptype_df()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
creates empty prototype `data.frame`s from a fields list. The
bidirectional mapping lives in `vec_mapping` in
`R/esri-field-mapping.R`.

### Geoprocessing services

`arc_gp_job` is an R6 class wrapping async GP service workflows. Create
with `new_gp_job(url, params)`, start with `$start()`, poll with
`$status`, wait with `$await()`, retrieve with `$results`. Parameters
must be `arc_form_params` (S7 class ensuring all values are scalar
strings). Typed GP parameter helpers follow the pattern
[`gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
/
[`as_gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
/
[`parse_gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md).

### Portal API

Functions in `R/sharing.R`, `R/portal-self.R`, `R/portal-types.R`,
`R/search.R`, and `R/user-*.R` wrap the ArcGIS sharing REST API. Most
accept `host = arc_host()` and `token = arc_token()` with the token
fetched from the environment by default. Pagination is handled by
[`arc_paginate_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_paginate_req.md)
which wraps
[`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

### S7 / R6 classes

- `arc_gp_job` — R6, wraps a GP service job lifecycle
- `arc_form_params` — S7, validated named list of scalar strings for GP
  form bodies
- `arc_job_status` — S7, validated GP job status string
- `gp_linear_unit` (`GPLinearUnit`) — S7, distance + units
- `gp_areal_unit` (`GPArealUnit`) — S7, area + units
- `item_type` / `item_keyword` — S7 wrappers validating portal item
  type/keyword strings

### Environment variables

| Variable              | Purpose                                                                                            |
|-----------------------|----------------------------------------------------------------------------------------------------|
| `ARCGIS_HOST`         | Portal host (default: `https://www.arcgis.com`)                                                    |
| `ARCGIS_CLIENT`       | OAuth2 client ID                                                                                   |
| `ARCGIS_SECRET`       | OAuth2 client secret                                                                               |
| `ARCGIS_API_KEY`      | API key for [`auth_key()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)              |
| `ARCGIS_USER`         | Username for [`auth_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)            |
| `ARCGIS_PASSWORD`     | Password for [`auth_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)            |
| `ARCGIS_REDIRECT_URI` | Redirect URI for [`auth_shiny()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md) |
