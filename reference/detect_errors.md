# Detect errors in parsed json response

The requests responses from ArcGIS don't return the status code in the
response itself but rather from the body in the json. This function
checks for the existence of an error. If an error is found, the contents
of the error message are bubbled up.

## Usage

``` r
detect_errors(response, error_call = rlang::caller_env())

catch_error(response, error_call = rlang::caller_env())
```

## Arguments

- response:

  for `detect_errors()`, a list typically from
  `RcppSimdJson::fparse(httr2::resp_body_string(resp))`. For
  `catch_error()`, the string from `httr2::resp_body_string(resp)`.

- error_call:

  default
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).
  The environment from which to throw the error from.

## Value

Nothing. Used for it's side effect. If an error code is encountered in
the response an error is thrown with the error code and the error
message.

## Examples

``` r
if (FALSE) { # \dontrun{
response <- list(
  error = list(
    code = 400L,
    message = "Unable to generate token.",
    details = "Invalid username or password."
  )
)

detect_errors(response)
} # }
```
