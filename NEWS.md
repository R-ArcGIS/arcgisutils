# arcgisutils (development version)

- Adds `auth_shiny()` and `oauth_provider_arcgis()` to support authentication in a Shiny application <https://github.com/R-ArcGIS/arcgisutils/pull/82>
- Adds `gp_job_from_url()` which creates a new `arc_gp_job` from a given URL

# arcgisutils 0.4.0

- Adds `arc_portal_servers()` to list all federated servers in your ArcGIS Enterprise portal
- Adds `arc_portal_resources()` to list all file resources for a given portal ID
- Adds `arc_portal_users()` to list all users in a provided portal
- Adds `search_item()` to search for content items with automatic pagination.
- Adds `arc_user_self()` to return metadata for the authenticated user
- Adds `arc_group_content()` and `arc_user_content()` to fetch user and group content listings
- Adds `arc_paginate_req()` to automatically apply pagination to requests
- Adds support for parsing and creating geoprocessing service json see `?gp_params`
- Adds Geoprocessing Service support via new S7 classes:
  - `arc_gp_job`, `arc_job_status`, and `arc_form_params` 
- Adds S7 and R6 as dependencies
- Adds `data_frame()` utility function which adds the `tbl` class to a `data.frame` for pretty tibble printing without requiring the tibble dependency.
- Adds new experimental functions for parsing urls `arc_url_parse()`, `arc_url_type()`, and `is_url()` h/t [@elipousson](https://github.com/elipousson)
- Adds new experimental functions for working with a portal's sharing API `arc_item()`, `arc_group()`, `arc_user()`, `arc_item_data()`, `arc_portal_urls()`
- Validate `token` in `arc_base_req()`

### Deprecations 

- Deprecates `arc_self_meta()` in favor of `arc_portal_self()`—the functions are identical.
- `ptype_tbl()` has been deprecated in favor of `fields_as_ptype_df()`.
- `infer_esri_type()` has been deprecated in favor of `as_fields()`.


## Breaking changes: 

- `get_ptype()` has been removed from the public API.
- `remote_ptype_tbl()` has been removed removing the `dbplyr` dependency.

# arcgisutils 0.3.2

- Addresses a bug where NA values were being returned incorrectly <https://github.com/R-ArcGIS/arcgisutils/issues/56>
- Addresses a bug when row-binding inconsistent columns with collapse <https://github.com/R-ArcGIS/arcgisutils/issues/54>

# arcgisutils 0.3.1

- addresses a bug where integers were encoded as floats. This caused a problem for using `update_features()` and specifying the OID field

# arcgisutils 0.3.0

- All geometry conversion functions: `as_esri_geometry()`, `as_esri_features()`, `as_esri_featureset()`, `as_features()` and `as_featureset()` have been rewritten from the ground up using Rust and extendr. 
  - `arcgisutils` now requires Rust to build from source
  - `jsonify` is moved to Suggests
  - `as_geometry()` is no longer exported
  - `...` argument is removed
- `auth_key()` is added to support authorization with an API key for ArcGIS Developers accounts
- `catch_error()` is a new function which parses a string and catches the error as an object. This is useful when processing multiple responses at once. 
- `rbind_results()` is a new helper function that combines a list of results as efficiently as possible.
- `arc_base_req()` gains two new arguments `path` and `query` which allows you to add query parameters and paths to the generated base request
- `arc_self_meta()` is a new function to provide access to the [`/self`](https://developers.arcgis.com/rest/users-groups-and-items/portal-self.htm) endpoint. Closes [#32](https://github.com/R-ArcGIS/arcgisutils/issues/32)
- Null geometries are parsed into empty Geometry Collections using `sf::st_geometrycollection()` Fixed [#168](https://github.com/R-ArcGIS/arcgislayers/issues/168)
- When Esri JSON contains 0 features, `parse_esri_json()` will create an empty `data.frame` with the fields that are returned with the appropriate R type.


# arcgisutils 0.2.0

- `parse_esri_json()` will return an empty `data.frame` in the presence of empty results an error. If an error is present, the error is reported
- Breaking change to how authorization tokens are handled
  - Tokens are now stored in internal environment `token_env`
  - `set_auth_token()` removed in favor of `set_arc_token()` 
  - `set_arc_token()` allows for multiple named keys which are set to the `token_env`
  - `arc_token()` fetches tokens directly from the `token_env` 
  - `unset_arc_token()` removes tokens from `token_env`
  - intended to be used with `arc_base_req()` 
- `arc_base_req()` is introduce creating a standardized way to making base httr2 request objects. 
  - <https://github.com/R-ArcGIS/arcgisutils/pull/19>
- httr2 must be >= 1.0.0 now
* New function `arc_agent()` is added to set a package specific user agent 
* `fetch_layer_metadata()` now puts `f=json` in the url instead of the request body
  - accepts `NULL` tokens 
  - uses `req_auth_bearer_token()` to include token in header
  - <https://github.com/R-ArcGIS/arcgisutils/pull/8>
* Define `arc_token()` to get "ARCGIS_TOKEN" environment variable. This ensures that empty strings do not cause HTTP 498 "invalid token" error by returning `NULL` in stead of an empty string. ([#6](https://github.com/R-ArcGIS/arcgisutils/pull/6)) [@kbvernon](https://github.com/kbvernon)

# arcgisutils 0.1.1

* fix failing tests on oldrel. Use as.POSIXct.character instead of numeric
* fix typo in description 

# arcgisutils 0.1.0

* Initial release
