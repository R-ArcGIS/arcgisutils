# Portal Content Items

For a given user or group, returns a data.frame of all content items
owned by them.

## Usage

``` r
arc_group_content(group, host = arc_host(), token = arc_token())

arc_user_content(user, host = arc_host(), token = arc_token())
```

## Arguments

- group:

  a scalar character of the group ID or a `PortalGroup` object created
  using
  [`arc_group()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_group.md)

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

- user:

  a scalar character of the username or a `PortalUser` object created
  using
  [`arc_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user.md)

## Value

a `data.frame` of content item metadata

## References

- [Group Content API
  Reference](https://developers.arcgis.com/rest/users-groups-and-items/group-content/)

- [User Content API
  Reference](https://developers.arcgis.com/rest/users-groups-and-items/user-content/)

## Examples

``` r
if (FALSE) { # \dontrun{
library(arcgis)

# authenticate
set_arc_token(auth_user())

# get your own content items
self <- arc_user_self()
arc_user_content(self$username)

# get a specific group's items
arc_group_content("2f0ec8cb03574128bd673cefab106f39")
} # }
```
