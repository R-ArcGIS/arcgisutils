# Combine multiple data.frames

A general function that takes a list of `data.frame`s and returns a
single and combines them into a single object. It will use the fastest
method available. In order this is `collapse::rowbind()`,
`data.table::rbindlist()`,
[`vctrs::list_unchop()`](https://vctrs.r-lib.org/reference/vec_chop.html),
then `do.call(rbind.data.frame, x)`.

## Usage

``` r
rbind_results(x, call = rlang::current_env(), .ptype = data.frame())
```

## Arguments

- x:

  a list where each element is a `data.frame` or `NULL`.

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

- .ptype:

  currently unused. Reserved for a future release.

## Value

see details.

## Details

If all items in the list are `data.frame`s, then the result will be a
`data.frame`. If all elements are an `sf` object, then the result will
be an `sf` object. If the items are mixed, the result will be a
`data.frame`.

If any items are `NULL`, then an attribute `null_elements` will be
attached to the result. The attribute is an integer vector of the
indices that were `NULL`.

## Examples

``` r
x <- head(iris)
res <- rbind_results(list(x, NULL, x))
attr(res, "null_elements")
#> [1] 2
```
