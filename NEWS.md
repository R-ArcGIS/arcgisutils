# arcgisutils (development version)

* New function `arc_agent()` is added to set a package sepcific user agent 
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
