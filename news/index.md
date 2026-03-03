# Changelog

## arcgisutils 0.5.0

- Adds
  [`from_envelope()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  and
  [`from_spatial_reference()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  to handle processing lists that represent an Esri Envelope and Esri
  SpatialReference object converting them into sf `bbox` objects
- Adds
  [`auth_shiny()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md)
  and
  [`oauth_provider_arcgis()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md)
  to support authentication in a Shiny application.
  [`{shinyOAuth}`](https://github.com/lukakoning/shinyOAuth/) is now a
  suggested package <https://github.com/R-ArcGIS/arcgisutils/pull/82>
- Adds
  [`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md)
  which creates a new `arc_gp_job` from a given URL

## arcgisutils 0.4.0

CRAN release: 2025-09-18

- Adds
  [`arc_portal_servers()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_servers.md)
  to list all federated servers in your ArcGIS Enterprise portal
- Adds
  [`arc_portal_resources()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_resources.md)
  to list all file resources for a given portal ID
- Adds
  [`arc_portal_users()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_users.md)
  to list all users in a provided portal
- Adds `search_item()` to search for content items with automatic
  pagination.
- Adds
  [`arc_user_self()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user_self.md)
  to return metadata for the authenticated user
- Adds
  [`arc_group_content()`](https://github.com/R-ArcGIS/arcgisutils/reference/content.md)
  and
  [`arc_user_content()`](https://github.com/R-ArcGIS/arcgisutils/reference/content.md)
  to fetch user and group content listings
- Adds
  [`arc_paginate_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_paginate_req.md)
  to automatically apply pagination to requests
- Adds support for parsing and creating geoprocessing service json see
  [`?gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
- Adds Geoprocessing Service support via new S7 classes:
  - `arc_gp_job`, `arc_job_status`, and `arc_form_params`
- Adds S7 and R6 as dependencies
- Adds
  [`data_frame()`](https://github.com/R-ArcGIS/arcgisutils/reference/utilities.md)
  utility function which adds the `tbl` class to a `data.frame` for
  pretty tibble printing without requiring the tibble dependency.
- Adds new experimental functions for parsing urls
  [`arc_url_parse()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md),
  [`arc_url_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md),
  and
  [`is_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md)
  h/t [@elipousson](https://github.com/elipousson)
- Adds new experimental functions for working with a portal’s sharing
  API
  [`arc_item()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item.md),
  [`arc_group()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_group.md),
  [`arc_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user.md),
  [`arc_item_data()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item_data.md),
  [`arc_portal_urls()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_urls.md)
- Validate `token` in
  [`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)

#### Deprecations

- Deprecates
  [`arc_self_meta()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)
  in favor of
  [`arc_portal_self()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)—the
  functions are identical.
- [`ptype_tbl()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  has been deprecated in favor of
  [`fields_as_ptype_df()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md).
- [`infer_esri_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  has been deprecated in favor of
  [`as_fields()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md).

### Breaking changes:

- `get_ptype()` has been removed from the public API.
- `remote_ptype_tbl()` has been removed removing the `dbplyr`
  dependency.

## arcgisutils 0.3.2

- Addresses a bug where NA values were being returned incorrectly
  <https://github.com/R-ArcGIS/arcgisutils/issues/56>
- Addresses a bug when row-binding inconsistent columns with collapse
  <https://github.com/R-ArcGIS/arcgisutils/issues/54>

## arcgisutils 0.3.1

CRAN release: 2024-09-26

- addresses a bug where integers were encoded as floats. This caused a
  problem for using `update_features()` and specifying the OID field

## arcgisutils 0.3.0

CRAN release: 2024-05-09

- All geometry conversion functions:
  [`as_esri_geometry()`](https://github.com/R-ArcGIS/arcgisutils/reference/as_esri_geometry.md),
  [`as_esri_features()`](https://github.com/R-ArcGIS/arcgisutils/reference/features.md),
  [`as_esri_featureset()`](https://github.com/R-ArcGIS/arcgisutils/reference/featureset.md),
  [`as_features()`](https://github.com/R-ArcGIS/arcgisutils/reference/features.md)
  and
  [`as_featureset()`](https://github.com/R-ArcGIS/arcgisutils/reference/featureset.md)
  have been rewritten from the ground up using Rust and extendr.
  - `arcgisutils` now requires Rust to build from source
  - `jsonify` is moved to Suggests
  - `as_geometry()` is no longer exported
  - `...` argument is removed
- [`auth_key()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  is added to support authorization with an API key for ArcGIS
  Developers accounts
- [`catch_error()`](https://github.com/R-ArcGIS/arcgisutils/reference/detect_errors.md)
  is a new function which parses a string and catches the error as an
  object. This is useful when processing multiple responses at once.
- [`rbind_results()`](https://github.com/R-ArcGIS/arcgisutils/reference/rbind_results.md)
  is a new helper function that combines a list of results as
  efficiently as possible.
- [`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
  gains two new arguments `path` and `query` which allows you to add
  query parameters and paths to the generated base request
- [`arc_self_meta()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)
  is a new function to provide access to the
  [`/self`](https://developers.arcgis.com/rest/users-groups-and-items/portal-self.htm)
  endpoint. Closes
  [\#32](https://github.com/R-ArcGIS/arcgisutils/issues/32)
- Null geometries are parsed into empty Geometry Collections using
  [`sf::st_geometrycollection()`](https://r-spatial.github.io/sf/reference/st.html)
  Fixed [\#168](https://github.com/R-ArcGIS/arcgislayers/issues/168)
- When Esri JSON contains 0 features,
  [`parse_esri_json()`](https://github.com/R-ArcGIS/arcgisutils/reference/parse_esri_json.md)
  will create an empty `data.frame` with the fields that are returned
  with the appropriate R type.

## arcgisutils 0.2.0

CRAN release: 2024-02-22

- [`parse_esri_json()`](https://github.com/R-ArcGIS/arcgisutils/reference/parse_esri_json.md)
  will return an empty `data.frame` in the presence of empty results an
  error. If an error is present, the error is reported
- Breaking change to how authorization tokens are handled
  - Tokens are now stored in internal environment `token_env`
  - `set_auth_token()` removed in favor of
    [`set_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  - [`set_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
    allows for multiple named keys which are set to the `token_env`
  - [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
    fetches tokens directly from the `token_env`
  - [`unset_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
    removes tokens from `token_env`
  - intended to be used with
    [`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
- [`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
  is introduce creating a standardized way to making base httr2 request
  objects.
  - <https://github.com/R-ArcGIS/arcgisutils/pull/19>
- httr2 must be \>= 1.0.0 now
- New function
  [`arc_agent()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_agent.md)
  is added to set a package specific user agent
- [`fetch_layer_metadata()`](https://github.com/R-ArcGIS/arcgisutils/reference/fetch_layer_metadata.md)
  now puts `f=json` in the url instead of the request body
  - accepts `NULL` tokens
  - uses `req_auth_bearer_token()` to include token in header
  - <https://github.com/R-ArcGIS/arcgisutils/pull/8>
- Define
  [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  to get “ARCGIS_TOKEN” environment variable. This ensures that empty
  strings do not cause HTTP 498 “invalid token” error by returning
  `NULL` in stead of an empty string.
  ([\#6](https://github.com/R-ArcGIS/arcgisutils/pull/6))
  [@kbvernon](https://github.com/kbvernon)

## arcgisutils 0.1.1

CRAN release: 2024-01-17

- fix failing tests on oldrel. Use as.POSIXct.character instead of
  numeric
- fix typo in description

## arcgisutils 0.1.0

CRAN release: 2024-01-11

- Initial release
