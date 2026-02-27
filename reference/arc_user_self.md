# Discover Authenticated User Metadata

Given an authentication token, return a list of user-specfic information
such as the user ID, username, available credits, email, groups, last
login date and more.

## Usage

``` r
arc_user_self(
  host = arc_host(),
  token = arc_token(),
  error_call = rlang::caller_call()
)
```

## Arguments

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

- error_call:

  the caller environment to be used when propagating errors.

## Value

a list of the authenticated user's metadata

## References

[API
Reference](https://developers.arcgis.com/rest/users-groups-and-items/self/)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  arc_user_self(token = auth_user())
}
} # }
```
