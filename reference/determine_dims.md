# Determine the dimensions of a geometry object

Given an sfc or sfg object determine what dimensions are represented.

## Usage

``` r
determine_dims(x)

has_m(x)

has_z(x)
```

## Arguments

- x:

  an object of class `sfc` or `sfg`

## Value

`determine_dims()` returns a scalar character of the value `"xy"`,
`"xyz"`, or `"xyzm"` depending on what dimensions are represented.

`has_m()` and `has_z()` returns a logical scalar of `TRUE` or `FALSE` if
the geometry has a Z or M dimension.

## Examples

``` r
geo <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)[["geometry"]]

determine_dims(geo)
#> [1] "xy"
has_z(geo)
#> [1] FALSE
has_m(geo)
#> [1] FALSE
```
