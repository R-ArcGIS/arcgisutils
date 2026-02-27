# Create Esri FeatureSet Objects

These functions create an Esri FeatureSet object. A FeatureSet contains
an inner array of features as well as additional metadata about the the
collection such as the geometry type, spatial reference, and object ID
field.

## Usage

``` r
as_featureset(x, crs = sf::st_crs(x), call = rlang::caller_env())

as_esri_featureset(x, crs = sf::st_crs(x), call = rlang::caller_env())
```

## Arguments

- x:

  an object of class `sf`, `data.frame`, or `sfc`.

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

a list or a json string

## References

[API
Reference](https://developers.arcgis.com/documentation/common-data-types/featureset-object.htm)

## Examples

``` r
library(sf)
# POINT
# create sfg points
xy <- st_sfc(st_point(c(1, 2)))
xyz <- st_sfc(st_point(c(1, 2, 3)))
xym <- st_sfc(st_point(c(1, 2, 3), dim = "XYM"))

as_esri_featureset(xy)
#> [1] "{\"geometryType\":\"esriGeometryPoint\",\"spatialReference\":{},\"hasZ\":false,\"hasM\":false,\"features\":[{\"geometry\":{\"x\":1.0,\"y\":2.0},\"attributes\":{}}]}"
as_esri_featureset(xyz)
#> [1] "{\"geometryType\":\"esriGeometryPoint\",\"spatialReference\":{},\"hasZ\":true,\"hasM\":false,\"features\":[{\"geometry\":{\"x\":1.0,\"y\":2.0,\"z\":3.0},\"attributes\":{}}]}"
as_esri_featureset(xym)
#> [1] "{\"geometryType\":\"esriGeometryPoint\",\"spatialReference\":{},\"hasZ\":true,\"hasM\":false,\"features\":[{\"geometry\":{\"x\":1.0,\"y\":2.0,\"m\":3.0},\"attributes\":{}}]}"

# MULTIPOINT
# vector to create matrix points
set.seed(0)
x <- rnorm(12)

xy <- st_sfc(st_multipoint(matrix(x, ncol = 2)))
xyz <- st_sfc(st_multipoint(matrix(x, ncol = 3)))
xym <- st_sfc(st_multipoint(matrix(x, ncol = 3), dim = "XYM"))

as_esri_featureset(xy)
#> [1] "{\"geometryType\":\"esriGeometryMultiPoint\",\"spatialReference\":{},\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":false,\"points\":[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]},\"attributes\":{}}]}"
as_esri_featureset(xyz)
#> [1] "{\"geometryType\":\"esriGeometryMultiPoint\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"points\":[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]},\"attributes\":{}}]}"
as_esri_featureset(xym)
#> [1] "{\"geometryType\":\"esriGeometryMultiPoint\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":true,\"points\":[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]},\"attributes\":{}}]}"

# LINESTRING
xy <- st_sfc(st_linestring(matrix(x, ncol = 2)))
xyz <- st_sfc(st_linestring(matrix(x, ncol = 3)))
xym <- st_sfc(st_linestring(matrix(x, ncol = 3), dim = "XYM"))

as_esri_featureset(xy)
#> [1] "{\"geometryType\":\"esriGeometryLineString\",\"spatialReference\":{},\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":false,\"paths\":[[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]]},\"attributes\":{}}]}"
as_esri_featureset(xyz)
#> [1] "{\"geometryType\":\"esriGeometryLineString\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]},\"attributes\":{}}]}"
as_esri_featureset(xym)
#> [1] "{\"geometryType\":\"esriGeometryLineString\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":true,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]},\"attributes\":{}}]}"

# MULTILINESTRING
as_esri_featureset(st_sfc(st_multilinestring(list(xy[[1]], xy[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolyline\",\"spatialReference\":{},\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":false,\"paths\":[[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]],[[1.2629542848807933,-0.9285670347135381],[-0.3262333607056494,-0.2947204467905602],[1.3297992629225006,-0.005767172747536955],[1.2724293214294047,2.404653388857951],[0.4146414344564082,0.7635934611404596],[-1.5399500419037095,-0.7990092489893682]]]},\"attributes\":{}}]}"
as_esri_featureset(st_sfc(st_multilinestring(list(xyz[[1]], xyz[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolyline\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]],[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]},\"attributes\":{}}]}"
as_esri_featureset(st_sfc(st_multilinestring(list(xym[[1]], xym[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolyline\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"paths\":[[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]],[[1.2629542848807933,0.4146414344564082,-0.005767172747536955],[-0.3262333607056494,-1.5399500419037095,2.404653388857951],[1.3297992629225006,-0.9285670347135381,0.7635934611404596],[1.2724293214294047,-0.2947204467905602,-0.7990092489893682]]]},\"attributes\":{}}]}"

# POLYGON
coords <- rbind(
  c(0, 0, 0, 1),
  c(0, 1, 0, 1),
  c(1, 1, 1, 1),
  c(1, 0, 1, 1),
  c(0, 0, 0, 1)
)

xy <- st_sfc(st_polygon(list(coords[, 1:2])))
xyz <- st_sfc(st_polygon(list(coords[, 1:3])))
xym <- st_sfc(st_polygon(list(coords[, 1:3]), dim = "XYM"))

as_esri_featureset(xy)
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":false,\"rings\":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]},\"attributes\":{}}]}"
as_esri_featureset(xyz)
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]},\"attributes\":{}}]}"
as_esri_featureset(xym)
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":true,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]},\"attributes\":{}}]}"

# MULTIPOLYGON
as_esri_featureset(st_sfc(st_multipolygon(list(xy[[1]], xy[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"features\":[{\"geometry\":{\"hasZ\":false,\"hasM\":false,\"rings\":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]},\"attributes\":{}}]}"
as_esri_featureset(st_sfc(st_multipolygon(list(xyz[[1]], xyz[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]],[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]},\"attributes\":{}}]}"
as_esri_featureset(st_sfc(st_multipolygon(list(xym[[1]], xym[[1]]))))
#> [1] "{\"geometryType\":\"esriGeometryPolygon\",\"spatialReference\":{},\"hasZ\":true,\"features\":[{\"geometry\":{\"hasZ\":true,\"hasM\":false,\"rings\":[[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]],[[0.0,0.0,0.0],[0.0,1.0,0.0],[1.0,1.0,1.0],[1.0,0.0,1.0],[0.0,0.0,0.0]]]},\"attributes\":{}}]}"
```
