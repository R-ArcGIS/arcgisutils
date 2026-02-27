# Validate CRS object

Takes a representation of a CRS and ensures that it is a valid one. The
CRS is validated using
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
if it cannot be validated, a null CRS is returned.

## Usage

``` r
validate_crs(crs, arg = rlang::caller_arg(crs), call = rlang::caller_env())
```

## Arguments

- crs:

  a representation of a coordinate reference system.

- arg:

  An argument name in the current function.

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

Returns a list of length 1 with an element named `spatialReference`
which is itself a named list.

If the provided CRS returns a valid well-known ID (WKID)
`spatialReference` contains a named element called `wkid` which is the
integer value of the WKID. If the WKID is not known but the CRS returned
is a valid well-known text representation the `wkid` field is `NA` and
another field `wkt` contains the valid wkt.

## Details

See
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
for more details on valid representations.

## Examples

``` r
# using epsg code integer or string representation
validate_crs(3857)
#> $spatialReference
#> $spatialReference$wkid
#> [1] 3857
#> 
#> 
validate_crs("EPSG:4326")
#> $spatialReference
#> $spatialReference$wkid
#> [1] 4326
#> 
#> 

# using a custom proj4 string
proj4string <- "+proj=longlat +datum=WGS84 +no_defs"

crs <- validate_crs(proj4string)

# using wkt2 (from above result)
crs <- validate_crs(crs$spatialReference$wkt)
```
