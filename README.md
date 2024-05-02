
<!-- badges: start -->

[![R-CMD-check](https://github.com/R-ArcGIS/arcgisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-ArcGIS/arcgisutils/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/arcgisutils.png)](https://CRAN.R-project.org/package=arcgisutils)
<!-- badges: end -->

# arcgisutils

arcgisutils is designed as the backbone of the
[`{arcgis}`](https://github.com/r-arcgis/arcgis) meta-package.

arcgisutils is a developer oriented package that provides the basic
functions to build R packages that work with ArcGIS Location Services.
It provides functionality for authorization, Esri JSON construction and
parsing, as well as other utilities pertaining to geometry and Esri type
conversions.

## Installation

Install arcgisutils from CRAN.

``` r
install.packages("arcgisutils", repos = "https://r-arcgis.r-universe.dev")
```

Or, you can install the development version of arcgisutils
[r-universe](https://r-arcgis.r-universe.dev/) with:

``` r
install.packages("arcgisutils", repos = "https://r-arcgis.r-universe.dev")
```

### Authorization

Authorization tokens are provided through the functions `auth_code()`,
`auth_client()`, `auth_user()`, and `auth_binding()`. Additional token
validation functions are provided via `refresh_token()` and
`validate_or_refresh_token()`.

Tokens are managed in a session based cache using `set_arc_token()` and
`unset_arc_token()`. They are fetched using `arc_token()`. Here is a
minimal example:

``` r
library(arcgisutils)
#> 
#> Attaching package: 'arcgisutils'
#> The following object is masked from 'package:base':
#> 
#>     %||%

tkn <- auth_client()

set_arc_token(tkn)

arc_token()
#> <httr2_token>
#> token_type: bearer
#> access_token: <REDACTED>
#> expires_at: 2024-05-02 14:22:45
#> arcgis_host: https://www.arcgis.com
```

Alternatively, tokens can be set based on a key-value pair.

``` r
set_arc_token("A" = tkn, "B" = tkn)
#> ✔ Named tokens set: `A` and `B`
#> ℹ Access named tokens with `arc_token("name")`
```

And fetched based on their name via

``` r
arc_token("A")
#> <httr2_token>
#> token_type: bearer
#> access_token: <REDACTED>
#> expires_at: 2024-05-02 14:22:45
#> arcgis_host: https://www.arcgis.com
```

### Standardized Requests

The function `arc_base_req()` is used to create a standardized `httr2`
request object. It handles authorization tokens and sets a user agent.

``` r
host <- arc_host() # use arcgis.com by default

arc_base_req(host)
#> <httr2_request>
#> GET https://www.arcgis.com
#> Body: empty
#> Options:
#> • useragent: 'arcgisutils v0.2.0.9002'
```

### Esri JSON

There are also a number of utility functions for creating and parsing
Esri JSON. For example we can create a list that represent an Esri
`FeatureSet` using `as_featurset()` directly from an `sf` object. To
convert to json, it is recommended to use
`jsonify::to_json(x, unbox = TRUE)`.

``` r
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc_json <- as_featureset(nc)

str(nc_json, 1)
#> List of 3
#>  $ geometryType    : chr "esriGeometryPolygon"
#>  $ spatialReference:List of 1
#>  $ features        :List of 100
```

Alternatively, you can use the set of functions with the `_esri_` infix
to directly create the json. See the [Esri geometry reference
page](https://r.esri.com/arcgisutils/reference/esri_geometry.html) for
more on how the conversion functions work.

Additionally, sf’s `crs` object can be converted to a
[`spatialReference`](https://developers.arcgis.com/documentation/common-data-types/geometry-objects.htm#GUID-DFF0E738-5A42-40BC-A811-ACCB5814BABC)
JSON object using `validate_crs()`.

``` r
validate_crs(27700)
#> $spatialReference
#> $spatialReference$wkid
#> [1] 27700
```
