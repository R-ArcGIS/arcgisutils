# Parse an ArcGIS service or content URL into its components

`arc_url_parse()` uses
[`httr2::url_parse()`](https://httr2.r-lib.org/reference/url_parse.html)
to parse URL components and combine the components with a service or
content URL `type` and a `layer` number if applicable. A `layer`
component is only included if the `type` is `"MapServer"` or
`"FeatureServer"` and the URL includes a trailing digit. A full `url`
value is also included in the returned list. The `url`, `type`, and
`layer` components are not part of the `httr2_url` class object returned
by
[`httr2::url_parse()`](https://httr2.r-lib.org/reference/url_parse.html).

## Usage

``` r
arc_url_parse(url, base_url = NULL, error_call = rlang::caller_call())

arc_url_type(url, error_call = rlang::caller_call())

is_url(url, error_call = rlang::caller_call())
```

## Arguments

- url:

  A string containing the URL to parse.

- base_url:

  Use this as a parent, if `url` is a relative URL.

- error_call:

  the caller environment to be used when propagating errors.

## Value

A named list with the following components: scheme, hostname, username,
password, port, path, query, fragment, url, type, and layer.

## Details

**\[experimental\]**

## Examples

``` r
arc_url_parse(
  "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/0"
)
#> $scheme
#> [1] "https"
#> 
#> $hostname
#> [1] "services.arcgisonline.com"
#> 
#> $username
#> NULL
#> 
#> $password
#> NULL
#> 
#> $port
#> NULL
#> 
#> $path
#> [1] "/arcgis/rest/services/USA_Topo_Maps/MapServer/0"
#> 
#> $query
#> NULL
#> 
#> $fragment
#> NULL
#> 
#> $url
#> [1] "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/0"
#> 
#> $type
#> [1] "MapServer"
#> 
#> $layer
#> [1] "0"
#> 
arc_url_parse(
  "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer"
)
#> $scheme
#> [1] "https"
#> 
#> $hostname
#> [1] "geocode.arcgis.com"
#> 
#> $username
#> NULL
#> 
#> $password
#> NULL
#> 
#> $port
#> NULL
#> 
#> $path
#> [1] "/arcgis/rest/services/World/GeocodeServer"
#> 
#> $query
#> NULL
#> 
#> $fragment
#> NULL
#> 
#> $url
#> [1] "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer"
#> 
#> $type
#> [1] "GeocodeServer"
#> 
#> $layer
#> NULL
#> 
arc_url_parse(
  "https://services.arcgisonline.com/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer"
)
#> $scheme
#> [1] "https"
#> 
#> $hostname
#> [1] "services.arcgisonline.com"
#> 
#> $username
#> NULL
#> 
#> $password
#> NULL
#> 
#> $port
#> NULL
#> 
#> $path
#> [1] "/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer"
#> 
#> $query
#> NULL
#> 
#> $fragment
#> NULL
#> 
#> $url
#> [1] "https://services.arcgisonline.com/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer"
#> 
#> $type
#> [1] "ImageServer"
#> 
#> $layer
#> NULL
#> 
```
