# User Information

Fetch a user's metadata based on username.

## Usage

``` r
arc_user(username, host = arc_host(), token = arc_token())
```

## Arguments

- username:

  the username to fetch. A scalar character.

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

## Value

a list of class `PortalUser`

## Details

**\[experimental\]**

## See also

Other portal organization:
[`arc_group()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_group.md)

## Examples

``` r
arc_user("esri_en")
#> <PortalUser<esri_en>>
#> id: eea3c410b1142e8e2baaf8423d130e23
#> fullName: Esri English
#> created: 2011-11-18 16:33:40
```
