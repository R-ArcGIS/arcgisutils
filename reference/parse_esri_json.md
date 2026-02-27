# Parse Esri JSON

Parses an Esri FeatureSet JSON object into an R object. If there is no
geometry present, a data.frame is returned. If there is geometry, an sf
object is returned.

## Usage

``` r
parse_esri_json(string, ..., call = rlang::caller_env())
```

## Arguments

- string:

  the raw Esri JSON string.

- ...:

  additional arguments passed to
  [`RcppSimdJson::fparse`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html)

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

## Value

A data.frame. If geometry is found, returns an sf object.

## Examples

``` r
esri_json <- '{
    "geometryType": "esriGeometryPolygon",
    "spatialReference": {
        "wkid": 4326
    },
    "hasZ": false,
    "hasM": false,
    "features": [
        {
            "attributes": {
                "id": 1
            },
            "geometry": {
                "rings": [
                    [
                        [0.0, 0.0],
                        [1.0, 0.0],
                        [1.0, 1.0],
                        [0.0, 1.0],
                        [0.0, 0.0]
                    ]
                ]
            }
        }
    ]
}'

parse_esri_json(esri_json)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> Geodetic CRS:  WGS 84
#>   id                       geometry
#> 1  1 MULTIPOLYGON (((0 0, 1 0, 1...
```
