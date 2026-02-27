# Create Esri JSON Geometry Objects

`as_esri_geometry()` converts an `sfg` object to a EsriJSON Geometry
object as a string.

## Usage

``` r
as_esri_geometry(x, crs = NULL, call = rlang::caller_env())
```

## Arguments

- x:

  an object of class `sfg`. Must be one of `"POINT"`, `"MULTIPOINT"`,
  `"LINESTRING"`, `"MULTILINESTRING"`, `"POLYGON"`, or `"MULTIPOLYGON"`.

- crs:

  the coordinate reference system. It must be interpretable by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

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

a scalar string

## Details

See
[`as_featureset()`](https://github.com/R-ArcGIS/arcgisutils/reference/featureset.md)
and
[`as_features()`](https://github.com/R-ArcGIS/arcgisutils/reference/features.md)
for converting `sfc` and `sf` objects into EsriJSON.

## References

[API
Reference](https://developers.arcgis.com/documentation/common-data-types/geometry-objects.htm)

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
# POINT
# create sfg points
xy <- st_point(c(1, 2))
xyz <- st_point(c(1, 2, 3))
xym <- st_point(c(1, 2, 3), dim = "XYM")
xyzm <- st_point(c(1, 2, 3, 4))

as_esri_geometry(xy)
#> [1] "{\"x\":1.0,\"y\":2.0}"
as_esri_geometry(xyz)
#> [1] "{\"x\":1.0,\"y\":2.0,\"z\":3.0}"
as_esri_geometry(xym)
#> [1] "{\"x\":1.0,\"y\":2.0,\"m\":3.0}"
as_esri_geometry(xyzm)
#> [1] "{\"x\":1.0,\"y\":2.0,\"z\":3.0,\"m\":4.0}"

# MULTIPOINT
# vector to create matrix points
set.seed(0)
x <- rnorm(12)

xy <- st_multipoint(matrix(x, ncol = 2))
xyz <- st_multipoint(matrix(x, ncol = 3))
xym <- st_multipoint(matrix(x, ncol = 3), dim = "XYM")
xyzm <- st_multipoint(matrix(x, ncol = 4), dim = "XYM")

as_esri_geometry(xy)
#> [1] "{\"hasZ\":false,\"hasM\":false,\"points\":[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]}"
as_esri_geometry(xyz)
#> [1] "{\"hasZ\":true,\"hasM\":false,\"points\":[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]}"
as_esri_geometry(xym)
#> [1] "{\"hasZ\":false,\"hasM\":true,\"points\":[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]}"
as_esri_geometry(xyzm)
#> [1] "{\"hasZ\":true,\"hasM\":true,\"points\":[[1.2629542848807933,1.2724293214294047,-0.9285670347135381,2.404653388857951],[-0.3262333607056494,0.4146414344564082,-0.2947204467905602,0.7635934611404596],[1.3297992629225006,-1.5399500419037095,-0.005767172747536955,-0.7990092489893682]]}"

# LINESTRING
xy <- st_linestring(matrix(x, ncol = 2))
xyz <- st_linestring(matrix(x, ncol = 3))
xym <- st_linestring(matrix(x, ncol = 3), dim = "XYM")
xyzm <- st_linestring(matrix(x, ncol = 4), dim = "XYM")

as_esri_geometry(xy)
#> [1] "{\"hasZ\":false,\"hasM\":false,\"paths\":[[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]]}"
as_esri_geometry(xyz)
#> [1] "{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]}"
as_esri_geometry(xym)
#> [1] "{\"hasZ\":false,\"hasM\":true,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]}"
as_esri_geometry(xyzm)
#> [1] "{\"hasZ\":true,\"hasM\":true,\"paths\":[[[1.2629542848807933,1.2724293214294047,-0.9285670347135381,2.404653388857951],[-0.3262333607056494,0.4146414344564082,-0.2947204467905602,0.7635934611404596],[1.3297992629225006,-1.5399500419037095,-0.005767172747536955,-0.7990092489893682]]]}"

# MULTILINESTRING
as_esri_geometry(st_multilinestring(list(xy, xy)))
#> [1] "{\"hasZ\":false,\"hasM\":false,\"paths\":[[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]],[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]]}"
as_esri_geometry(st_multilinestring(list(xyz, xyz)))
#> [1] "{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]],[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]}"
as_esri_geometry(st_multilinestring(list(xym, xym)))
#> [1] "{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]],[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]}"
as_esri_geometry(st_multilinestring(list(xyzm, xyzm)))
#> [1] "{\"hasZ\":true,\"hasM\":true,\"paths\":[[[1.2629542848807933,1.2724293214294047,-0.9285670347135381,2.404653388857951],[-0.3262333607056494,0.4146414344564082,-0.2947204467905602,0.7635934611404596],[1.3297992629225006,-1.5399500419037095,-0.005767172747536955,-0.7990092489893682]],[[1.2629542848807933,1.2724293214294047,-0.9285670347135381,2.404653388857951],[-0.3262333607056494,0.4146414344564082,-0.2947204467905602,0.7635934611404596],[1.3297992629225006,-1.5399500419037095,-0.005767172747536955,-0.7990092489893682]]]}"

# POLYGON
coords <- rbind(
  c(0, 0, 0, 1),
  c(0, 1, 0, 1),
  c(1, 1, 1, 1),
  c(1, 0, 1, 1),
  c(0, 0, 0, 1)
)

xy <- st_polygon(list(coords[, 1:2]))
xyz <- st_polygon(list(coords[, 1:3]))
xym <- st_polygon(list(coords[, 1:3]), dim = "XYM")
xyzm <- st_polygon(list(coords))

as_esri_geometry(xy)
#> [1] "{\"hasZ\":false,\"hasM\":false,\"rings\":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}"
as_esri_geometry(xyz)
#> [1] "{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]}"
as_esri_geometry(xym)
#> [1] "{\"hasZ\":false,\"hasM\":true,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]}"
as_esri_geometry(xyzm)
#> [1] "{\"hasZ\":true,\"hasM\":true,\"rings\":[[[0.0,0.0,0.0,1.0],[0.0,1.0,0.0,1.0],[1.0,1.0,1.0,1.0],[1.0,0.0,1.0,1.0],[0.0,0.0,0.0,1.0]]]}"

# MULTIPOLYGON
as_esri_geometry(st_multipolygon(list(xy, xy)))
#> [1] "{\"hasZ\":false,\"hasM\":false,\"rings\":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}"
as_esri_geometry(st_multipolygon(list(xyz, xyz)))
#> [1] "{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]],[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]}"
as_esri_geometry(st_multipolygon(list(xym, xym)))
#> [1] "{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]],[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]}"
as_esri_geometry(st_multipolygon(list(xyzm, xyzm)))
#> [1] "{\"hasZ\":true,\"hasM\":true,\"rings\":[[[0.0,0.0,0.0,1.0],[0.0,1.0,0.0,1.0],[1.0,1.0,1.0,1.0],[1.0,0.0,1.0,1.0],[0.0,0.0,0.0,1.0]],[[0.0,0.0,0.0,1.0],[0.0,1.0,0.0,1.0],[1.0,1.0,1.0,1.0],[1.0,0.0,1.0,1.0],[0.0,0.0,0.0,1.0]]]}"
```
