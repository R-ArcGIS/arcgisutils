# Determines Portal Host

Returns a scalar character indicating the host to make requests to.

## Usage

``` r
arc_host()
```

## Value

A scalar character, `"https://www.arcgis.com"` by default.

## Details

By default, the host is ArcGIS Online \<`https://www.arcgis.com`\>. If
the environment variable `ARCGIS_HOST` is set, it will be returned.

## Examples

``` r
arc_host()
#> [1] "https://www.arcgis.com"
```
