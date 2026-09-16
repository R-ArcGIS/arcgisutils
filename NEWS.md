# arcgisutils 0.6.1 (development)

- Shiny authentication now identifies the signed-in user by their user ID instead of the organization ID.
- Shiny login and refresh accept ArcGIS token responses that omit `token_type`. This integration now requires shinyOAuth 0.6.0 or later.

- `arc_gp_job` gains `submit_path` and `results_path`, so services that are not `GPServer` endpoints can use it. A tile export submits to `exportTiles` and reads its download URL off the job resource, and no longer needs a parallel job class of its own.

- `arc_gp_job$await()` gains a `timeout` argument, defaulting to `Inf`.

- `arc_gp_job$token` returns the token the job was created with, for follow up requests such as downloading a result.

- `rbind_results()` now binds the geometry column separately from the attribute columns when the inputs are `sf`. This fixes two bugs that only appeared with the `collapse` backend: the result kept the first element's bounding box rather than the bounding box of the combined geometry, and binding failed outright when the pieces' geometry classes differed, for example `LINESTRING` with `MULTILINESTRING`.

- Adds `as_bbox()` which coerces an extent to a [`wk::rct()`]. It accepts a `wk_rct`, an `sf` `bbox`, a length four numeric, or any geometry `wk::wk_bbox()` understands, and reprojects when a different `crs` is requested.
- `fetch_layer_metadata()` gains `path` and `query` arguments, passed through to `arc_base_req()`, so sub-resources can be fetched without assembling a URL by hand.

- Adds `upload_file()` which uploads a local file to a portal as a new content item.
- Adds `delete_items()` which deletes portal items.
- `portal_item_types()` no longer returns `"Layer"` and `"Pro Map"` twice.
- `arc_base_req()` retries transient failures. Both HTTP status codes and Esri error codes in a `200` body of `429`, `500`, `502`, `503`, and `504` are retried, as are connection failures. Controlled by `max_tries` and the `arcgis.retries` option <https://github.com/R-ArcGIS/arcgisutils/issues/43>

- Replaces deprecated functions.
- Fixes a bug where `sf` objects were assumed to have geometry in the last column position.
- Adds `arcgis.req_headers` option. If provided, the headers will be injected into all requested from `arc_base_req()` <https://github.com/R-ArcGIS/arcgisutils/issues/94>
- `from_spatial_reference()` gracefully handles `NA` and `NULL` <https://github.com/R-ArcGIS/arcgisutils/issues/95>
- Fixes a bug where feature collections generated an invalid Object ID field for 0-row inputs
- Fixes a duplicated `joined` query parameter in `arc_group_users()`
- Exports `report_errors()`, the warning-level counterpart to `detect_errors()` <https://github.com/R-ArcGIS/arcgisutils/issues/39>
- `obj_check_token()` names the `auth_*` functions and `set_arc_token()` when given an invalid token <https://github.com/R-ArcGIS/arcgisutils/issues/37>
- `arc_user_content()` and `arc_group_content()` gain `page_size`, `max_pages`, and `.progress` <https://github.com/R-ArcGIS/arcgisutils/issues/77>
- `arc_paginate_req()` now enforces the documented `page_size` bounds. They were passed positionally into `...` and silently ignored
- `is_url()` reports errors against its caller rather than an unused positional argument
- `arc_user()` parses `lastLogin` and `emailStatusDate`, and returns `groups` as a tbl with parsed dates, matching `arc_user_self()` <https://github.com/R-ArcGIS/arcgisutils/issues/75>
- Unsupported columns now report `cannot be converted into EsriJSON` naming the column and its type. `as_fields()` aborted first with `row names contain missing values`, making that message unreachable <https://github.com/R-ArcGIS/arcgisutils/issues/49>
- `rbind_results()` combines zero column `data.frame`s, which arise when a query asks for no fields <https://github.com/R-ArcGIS/arcgislayers/issues/248>
- `as_layer_definition()` derives a simple renderer from the geometry type when `drawing_info` is not supplied. Layers published without one had data and a correct extent but drew nothing <https://github.com/R-ArcGIS/arcgislayers/issues/278>
- Adds `share_item()` and `unshare_item()` to set an item's access level and share it with groups. A published item previously could not be shared from R
- Adds `arc_related_items()` to fetch items related to a portal item <https://github.com/R-ArcGIS/arcgislayers/issues/204>
- Adds `s7x` backed enumerations for portal value sets: `ItemAccess`, `GroupAccess`, `RelationshipType`, `RelationshipDirection`, `SortOrder`, `GroupRole`, and `ItemSortField`
- `as_fields()` maps `logical` columns to `esriFieldTypeSmallInteger`. An all-`NA` column is `logical` in R and previously aborted with `row names contain missing values` <https://github.com/R-ArcGIS/arcgisutils/issues/83>
- `arc_gp_job$await()` reports failed and cancelled jobs instead of erroring with `unused argument` <https://github.com/R-ArcGIS/arcgisutils/issues/88>
- `arc_gp_job$cancel()` no longer assigns to the read-only `status` active binding
- `as_featureset()` returns an integer `wkid` instead of a double that serialized as `4326.0`. Requires serde_esri 1.0 <https://github.com/R-ArcGIS/arcgisutils/issues/87>

# arcgisutils 0.6.0

## New Features

- `arc_user_content()` now defaults `user` to the authenticated user via `arc_user_self()`
- `arc_user_self()` returns a `PortalUser` object (subclass of `list`)
- `arc_gp_job` R6 object gains a `messages()` method to return the geoprocessing messages

## Fixed

- Fixes `as_esri_features()` to retain `null` values closes <https://github.com/R-ArcGIS/arcgisutils/issues/91>
- Fixes `as_form_params()` double-encoding string scalars; elements that are already a single character string are passed through as-is instead of being wrapped in `yyjsonr::write_json_str()`

## Changed

- Bumped extendr to v0.9.0

# arcgisutils 0.5.0

- `from_spatial_reference()` handles CRS WKIDs from the ESRI authority
- Adds `from_envelope()` and `from_spatial_reference()` to handle processing lists that represent an Esri Envelope and Esri SpatialReference object converting them into sf `bbox` objects
- Adds `auth_shiny()` and `oauth_provider_arcgis()` to support authentication in a Shiny application. [`{shinyOAuth}`](https://github.com/lukakoning/shinyOAuth/) is now a suggested package <https://github.com/R-ArcGIS/arcgisutils/pull/82>
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

- fix failing tests on oldrel. Use as.POSIXct.character instead of numeric
- fix typo in description

# arcgisutils 0.1.0

- Initial release
