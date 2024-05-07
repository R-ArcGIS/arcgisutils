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
