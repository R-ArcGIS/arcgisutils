# Determine Esri Geometry type

Takes an `sf` or `sfc` object and returns the appropriate Esri geometry
type.

## Usage

``` r
determine_esri_geo_type(x, call = rlang::caller_env())
```

## Arguments

- x:

  an object of class `data.frame`, `sf`, `sfc`, or `sfg`.

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

returns a character scalar of the corresponding Esri geometry type

## Details

### Geometry type mapping

- `POINT`: `esriGeometryPoint`

- `MULTIPOINT`: `esriGeometryMultipoint`

- `LINESTRING`: `esriGeometryPolyline`

- `MULTILINESTRING`: `esriGeometryPolyline`

- `POLYGON`: `esriGeometryPolygon`

- `MULTIPOLYGON`: `esriGeometryPolygon`

## Examples

``` r
determine_esri_geo_type(sf::st_point(c(0, 0)))
#> [1] "esriGeometryPoint"
```
