# Convert an object to an extent

Given an sf or sfc object create a list that represents the extent of
the object. The result of this function can be parsed directly into json
using `jsonify::to_json(x, unbox = TRUE)` or included into a list as the
extent component that will be eventually converted into json using the
above function.

## Usage

``` r
as_extent(x, crs = sf::st_crs(x), call = rlang::caller_env())
```

## Arguments

- x:

  an sf or sfc object

- crs:

  the CRS of the object. Must be parsable by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)

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

An extent json object. Use `jsonify::to_json(x, unbox = TRUE)` to
convert to json.

## Examples

``` r
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
as_extent(nc)
#> $xmin
#> [1] -84.32385
#> 
#> $ymin
#> [1] 33.88199
#> 
#> $xmax
#> [1] -75.45698
#> 
#> $ymax
#> [1] 36.58965
#> 
#> $spatialReference
#> $spatialReference$wkid
#> [1] 4267
#> 
#> 
```
