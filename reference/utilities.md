# General utility functions

General utility functions

## Usage

``` r
compact(.x)

a %||% b

check_dots_named(dots, call = rlang::caller_env())

data_frame(x, call = rlang::caller_call())
```

## Arguments

- .x:

  a list

- a:

  an R object

- b:

  an R object

- dots:

  a list collected from dots via `rlang::list2(...)`

- call:

  default
  [`rlang::caller_call()`](https://rlang.r-lib.org/reference/stack.html).

- x:

  a data.frame

## Value

- `compact()` a list

- `%||%` the first non-null item or `NULL` if both are `NULL`

## Details

- `compact()` removes any `NULL` list elements

- `%||%` is a special pipe operator that returns `b` if `a` is `NULL`

## Examples

``` r
# remove null elements
compact(list(a = NULL, b = 1))
#> $b
#> [1] 1
#> 

# if NULL return rhs
NULL %||% 123
#> [1] 123

# if not NULL return lhs
123 %||% NULL
#> [1] 123
```
