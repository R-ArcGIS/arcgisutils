# Manage authorization tokens

These functions are used to set, fetch, and check authorization tokens.

## Usage

``` r
arc_token(token = "ARCGIS_TOKEN")

set_arc_token(token, ...)

unset_arc_token(token = NULL)

obj_check_token(token, call = rlang::caller_env())

check_token_has_user(token, call = rlang::caller_env())
```

## Arguments

- token:

  for `arc_token()`, the name of a token to fetch. For
  `set_arc_token()`, it is an `httr2_token` that will be set. For
  `unset_arc_token()`, a character vector of token names to be unset.

- ...:

  named arguments to set `httr2_token`. Must be valid names and must be
  an `httr2_token`.

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

## Details

It is possible to have multiple authorization tokens in one session.
These functions assist you in managing them.

`arc_token()` is used to fetch tokens by name. The default token is
`ARCGIS_TOKEN`. However, they can be any valid character scalar.
`set_arc_token()` will create store a token with the name
`ARCGIS_TOKEN`. However, you can alternatively set the tokens by name
using a key-value pair. The key is what you would pass to `arc_token()`
to fetch the `httr2_token` object. To remove a token that has been set,
use `unset_arc_token()`.

`obj_check_token()` is a developer oriented function that can be used to
check if an object is indeed an `httr2_token`. To check if a token has
expired,
[`validate_or_refresh_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
will do so.

`check_token_has_user()` is a developer oriented function that checks to
see if a token has a `username` field associated with it.

For developers:

`set_arc_token()` uses a package level environment to store the tokens.
The tokens are fetched from the environment using `arc_token()`.

## Examples

``` r
# create fake tokens
token_a <- httr2::oauth_token("1234", arcgis_host = arc_host())
token_b <- httr2::oauth_token("abcd", arcgis_host = arc_host())

# set token to the default location
set_arc_token(token_a)

# fetch token from the default location
arc_token()
#> <httr2_token>
#> * token_type  : "bearer"
#> * access_token: <REDACTED>
#> * arcgis_host : "https://www.arcgis.com"

# set token by name
set_arc_token(org_a = token_a, org_b = token_b)
#> ✔ Named tokens set: `org_a` and `org_b`
#> ℹ Access named tokens with `arc_token("name")`

# fetch token by name
arc_token("org_a")
#> <httr2_token>
#> * token_type  : "bearer"
#> * access_token: <REDACTED>
#> * arcgis_host : "https://www.arcgis.com"
arc_token("org_b")
#> <httr2_token>
#> * token_type  : "bearer"
#> * access_token: <REDACTED>
#> * arcgis_host : "https://www.arcgis.com"

# unset tokens
unset_arc_token()
#> ✔ Token `ARCGIS_TOKEN` has been unset.
unset_arc_token(c("org_a", "org_b"))
#> ✔ Tokens `org_a` and `org_b` have been unset.
```
