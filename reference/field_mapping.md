# Esri Field Type Mapping

Infers Esri field types from R objects. Use `as_fields()` to create a
data.frame of valid [Esri Field
Types](https://developers.arcgis.com/web-map-specification/objects/field/)
from an `sf` object or `data.frame`.

## Usage

``` r
as_fields(.data, arg = rlang::caller_arg(.data), call = rlang::caller_env())

infer_esri_type(
  .data,
  arg = rlang::caller_arg(.data),
  call = rlang::caller_env()
)

fields_as_ptype_df(fields, n = 0, call = rlang::caller_env())

ptype_tbl(fields, n = 0, call = rlang::caller_env())
```

## Arguments

- .data:

  an object of class `data.frame`.

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

- fields:

  a list or data.frame of field types. Requires the fields `type` and
  `name` to be present.

- n:

  the number of rows to create in the prototype table

## Value

- `fields_as_ptype_df()` takes a `data.frame` with columns `name` and
  `type` and creates an empty `data.frame` with the corresponding
  columns and R types

- `as_fields()` returns a `data.frame` with columns `name`, `type`,
  `alias`, `nullable`, and `editable` columns

  - This resembles that of the `fields` returned by a FeatureService

## Details

**\[experimental\]**

- `as_fields()` takes a data frame-like object and infers the Esri field
  type from it.

- `fields_as_pytpe_df()` takes a list with `type` and `name` and creates
  an empty `data.frame` with the corresponding column names and types.

- `get_ptype()` takes a scalar character containing the Esri field type
  and returns a prototype of the pertinent R type

### Field type mapping:

Esri field types are mapped as

- `esriFieldTypeSmallInteger`: integer

- `esriFieldTypeSingle`: double

- `esriFieldTypeGUID`: integer

- `esriFieldTypeOID`: integer

- `esriFieldTypeInteger`: integer

- `esriFieldTypeBigInteger`: double

- `esriFieldTypeDouble`: double

- `esriFieldTypeString`: character

- `esriFieldTypeDate`: date

R types are mapped as

- `double`: esriFieldTypeDouble

- `integer`: esriFieldTypeInteger

- `character`: esriFieldTypeString

- `date`: esriFieldTypeDate

- `raw`: esriFieldTypeBlob

## Examples

``` r
inferred <- as_fields(iris)
inferred
#>                      name                type        alias length nullable
#> Sepal.Length Sepal.Length esriFieldTypeDouble Sepal.Length     NA     TRUE
#> Sepal.Width   Sepal.Width esriFieldTypeDouble  Sepal.Width     NA     TRUE
#> Petal.Length Petal.Length esriFieldTypeDouble Petal.Length     NA     TRUE
#> Petal.Width   Petal.Width esriFieldTypeDouble  Petal.Width     NA     TRUE
#> Species           Species esriFieldTypeString      Species    255     TRUE
#>              editable
#> Sepal.Length     TRUE
#> Sepal.Width      TRUE
#> Petal.Length     TRUE
#> Petal.Width      TRUE
#> Species          TRUE

fields_as_ptype_df(inferred)
#> # A data frame: 0 × 5
#> # ℹ 5 variables: Sepal.Length <dbl>, Sepal.Width <dbl>, Petal.Length <dbl>,
#> #   Petal.Width <dbl>, Species <chr>
```
