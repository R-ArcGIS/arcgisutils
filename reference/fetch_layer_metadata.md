# Retrieve metadata

Utility functions for feature service metadata.

## Usage

``` r
fetch_layer_metadata(url, token = NULL, call = rlang::caller_env())
```

## Arguments

- url:

  the url of the item.

- token:

  an `httr2_token` from one of the provided `auth_` functions

- call:

  default
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).
  The calling environment passed to
  [`detect_errors()`](https://github.com/R-ArcGIS/arcgisutils/reference/detect_errors.md).

## Value

returns a list object

## Details

- `fetch_layer_metadata()` given a request, fetches the metadata by
  setting the query parameter `f=json`

## Examples

``` r
# url is broken into parts to fit within 100 characters to avoid CRAN notes
url_parts <- c(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services",
  "/USA_Counties_Generalized_Boundaries/FeatureServer/0"
)

furl <- paste0(url_parts, collapse = "")
meta <- fetch_layer_metadata(furl)
head(names(meta))
#> [1] "currentVersion"     "id"                 "name"              
#> [4] "inDedicatedHosting" "preferredHost"      "type"              
```
