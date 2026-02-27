# Fetch Group Information

Fetches metadata about a group based on a provided `group_id`.

## Usage

``` r
arc_group(group_id, host = arc_host(), token = arc_token())
```

## Arguments

- group_id:

  the unique group identifier. A scalar character.

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

## Value

a list with group metadata

## Details

**\[experimental\]**

## See also

Other portal organization:
[`arc_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user.md)

## Examples

``` r
arc_group("2f0ec8cb03574128bd673cefab106f39")
#> <PortalGroup<Web Application Templates>>
#> id: 2f0ec8cb03574128bd673cefab106f39
#> owner: esri_en
#> created: 2012-02-10 00:27:49
```
