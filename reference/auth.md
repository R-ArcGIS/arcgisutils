# Authorization

Authorize your R session to connect to an ArcGIS Portal. See details.

## Usage

``` r
auth_code(client = Sys.getenv("ARCGIS_CLIENT"), host = arc_host())

auth_client(
  client = Sys.getenv("ARCGIS_CLIENT"),
  secret = Sys.getenv("ARCGIS_SECRET"),
  host = arc_host(),
  expiration = 120
)

auth_binding()

auth_user(
  username = Sys.getenv("ARCGIS_USER"),
  password = Sys.getenv("ARCGIS_PASSWORD"),
  host = arc_host(),
  expiration = 60
)

auth_key(api_key = Sys.getenv("ARCGIS_API_KEY"), host = arc_host())

refresh_token(token, client = Sys.getenv("ARCGIS_CLIENT"), host = arc_host())

validate_or_refresh_token(
  token,
  client = Sys.getenv("ARCGIS_CLIENT"),
  host = arc_host(),
  refresh_threshold = 10,
  call = rlang::caller_env()
)
```

## Arguments

- client:

  an OAuth 2.0 developer application client ID. By default uses the
  environment variable `ARCGIS_CLIENT`.

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- secret:

  an OAuth 2.0 developer application secret. By default uses the
  environment variable `ARCGIS_SECRET`.

- expiration:

  the duration of the token in minutes.

- username:

  default `Sys.getenv("ARCGIS_USER")`. Your username to login. **Do
  not** hard code this value.

- password:

  default `Sys.getenv("ARCGIS_PASSWORD")`. Your password to login. **Do
  not** hard code this value.

- api_key:

  default `Sys.getenv("ARCGIS_API_KEY")`. A character scalar of an
  ArcGIS Developer API key.

- token:

  an `httr2_token` as created by `auth_code()` or similar

- refresh_threshold:

  default `10`. If token expiry is within this threshold (in seconds)
  the token will be refreshed only if a `refresh_token` is available.
  Token refreshing is only possible with `auth_code()` flow.

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

an `httr2_token`

## Details

ArcGIS Online and Enterprise Portals utilize OAuth2 authorization via
their REST APIs.

- `auth_code()` is the recommend OAuth2 workflow for interactive
  sessions

- `auth_client()` is the recommended OAuth2 workflow for non-interactive
  sessions

- `auth_user()` uses legacy username and password authorization using
  the `generateToken` endpoint. It is only recommended for legacy
  systems that do not implement OAuth2.

- `auth_binding()` fetches a token from the active portal set by
  `arcgisbinding`. Uses `arcgisbinding::arc.check_portal()` to extract
  the authorization token. Recommended if using arcgisbinding.

## Examples

``` r
if (FALSE) { # \dontrun{
auth_code()
auth_client()
auth_user()
auth_key()
auth_binding()
} # }
```
