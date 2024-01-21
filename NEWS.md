# arcgisutils (development version)

* Define `arc_token()` to get "ARCGIS_TOKEN" environment variable. This ensures that empty strings do not cause HTTP 498 "invalid token" error. (#6)

# arcgisutils 0.1.1

* fix failing tests on oldrel. Use as.POSIXct.character instead of numeric
* fix typo in description 

# arcgisutils 0.1.0

* Initial release
