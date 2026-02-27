# Geoprocessing Parameter Types

Functions for converting R objects to and from ArcGIS geoprocessing
parameter types. These functions handle the serialization and parsing of
various data types used in ArcGIS geoprocessing services.

## Usage

``` r
parse_gp_feature_record_set(json)

as_gp_feature_record_set(x)

parse_gp_record_set(json)

as_record_set(x)

as_gp_raster_layer(x)

gp_linear_unit(distance = integer(0), units = character(0))

as_gp_linear_unit(x)

parse_gp_linear_unit(json)

gp_areal_unit(area = integer(0), units = character(0))

as_gp_areal_unit(x)

parse_gp_areal_unit(json)

as_gp_date(x)

parse_gp_date(json)

as_spatial_reference(x)

from_spatial_reference(sr, error_call = rlang::caller_call())

parse_spatial_reference(json)

from_envelope(x, error_call = rlang::caller_call())
```

## Arguments

- json:

  raw json to parse

- x:

  the object to convert into json

- distance:

  a scalar number of the distance.

- units:

  the unit of the measurement. Must be one of "esriUnknownAreaUnits",
  "esriSquareInches", "esriSquareFeet", "esriSquareYards", "esriAcres",
  "esriSquareMiles", "esriSquareMillimeters", "esriSquareCentimeters",
  "esriSquareDecimeters", "esriSquareMeters", "esriAres",
  "esriHectares", "esriSquareKilometers", "esriSquareInchesUS",
  "esriSquareFeetUS", "esriSquareYardsUS", "esriAcresUS",
  "esriSquareMilesUS".

- area:

  a scalar number of the measurement.

## Details

**\[experimental\]**

This package provides support for the following geoprocessing parameter
types:

### Implemented Types

- **GPFeatureRecordSetLayer**: Feature collections with geometry and
  attributes

- **GPRecordSet**: Tabular data without geometry

- **GPRasterDataLayer**: Raster datasets from Portal items, Image
  Servers, or URLs

- **GPLinearUnit**: Linear distance measurements with units

- **GPArealUnit**: Area measurements with units

- **GPDate**: Date/time values in milliseconds since epoch

- **GPSpatialReference**: Coordinate reference systems

### Not Yet Implemented

The following types are planned for future implementation:

- **GPField**: Field definitions with name, type, and properties

- **GPMultiValue**: Arrays of values for a single data type

- **GPValueTable**: Flexible table-like objects with rows and columns

- **GPComposite**: Parameters that accept multiple data types

- **GPEnvelope**: Bounding box extents (use
  [`as_extent()`](https://github.com/R-ArcGIS/arcgisutils/reference/as_extent.md)
  for GPExtent)

## Usage Patterns

Most functions follow a consistent pattern:

- `as_gp_*()`: Convert R objects to geoprocessing parameter JSON

- `parse_gp_*()`: Parse geoprocessing response JSON to R objects

- Constructor functions (e.g., `gp_linear_unit()`, `gp_areal_unit()`)
  create typed S7 objects

## Examples

    # Create a linear unit
    distance <- gp_linear_unit(distance = 100, units = "esriMeters")

    # Convert spatial data to feature record set
    as_gp_feature_record_set(my_sf_data)

    # Parse a geoprocessing response
    parse_gp_feature_record_set(response_json)

## References

[API
Documentation](https://developers.arcgis.com/rest/services-reference/enterprise/gp-data-types/)

## See also

Other geoprocessing:
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md),
[`arc_gp_job`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md),
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md),
[`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md)

## Examples

``` r
# create a feature record set
fset <- as_gp_feature_record_set(iris[1,])
fset
#> [1] "{\"spatialReference\":{},\"features\":[{\"attributes\":{\"Petal.Length\":1.4,\"Petal.Width\":0.2,\"Sepal.Length\":5.1,\"Sepal.Width\":3.5,\"Species\":\"setosa\"}}]}"

# create fake gp feature record set to parse
fset_list <- list(
  list(
    dataType = "GPFeatureRecordSetLayer",
    paramName = "example",
    value = as_featureset(iris[1,])
  )
)

# create the json
json <- yyjsonr::write_json_str(fset_list, auto_unbox = TRUE)

# parse the record set json
parse_gp_feature_record_set(json)
#> $param_name
#> [1] "example"
#> 
#> $data_type
#> [1] "GPFeatureRecordSetLayer"
#> 
#> $geometry
#>   Petal.Length Petal.Width Sepal.Length Sepal.Width Species
#> 1          1.4         0.2          5.1         3.5  setosa
#> 

# linear units
lu <- gp_linear_unit(10, "esriMeters")
lu
#> <arcgisutils::GPLinearUnit>
#>  @ distance: num 10
#>  @ units   : chr "esriMeters"
as_gp_linear_unit(lu)
#> [1] "{\"distance\":10.0,\"units\":\"esriMeters\"}"

# areal units
au <- gp_areal_unit(10, "esriSquareMeters")
au
#> <arcgisutils::GPArealUnit>
#>  @ area : num 10
#>  @ units: chr "esriSquareMeters"

as_gp_areal_unit(au)
#> [1] "{\"area\":10.0,\"units\":\"esriSquareMeters\"}"

# dates
json <- r"({
  "paramName": "Output_Date",
  "dataType": "GPDate",
  "value": 1199145600000
})"

parse_gp_date(json)
#> $paramName
#> [1] "Output_Date"
#> 
#> $dataType
#> [1] "GPDate"
#> 
#> $value
#> [1] "2008-01-01 UTC"
#> 
sr <- list(wkid = 4326L)
from_spatial_reference(sr)
#> Coordinate Reference System:
#>   User input: EPSG:4326 
#>   wkt:
#> GEOGCRS["WGS 84",
#>     ENSEMBLE["World Geodetic System 1984 ensemble",
#>         MEMBER["World Geodetic System 1984 (Transit)"],
#>         MEMBER["World Geodetic System 1984 (G730)"],
#>         MEMBER["World Geodetic System 1984 (G873)"],
#>         MEMBER["World Geodetic System 1984 (G1150)"],
#>         MEMBER["World Geodetic System 1984 (G1674)"],
#>         MEMBER["World Geodetic System 1984 (G1762)"],
#>         MEMBER["World Geodetic System 1984 (G2139)"],
#>         ELLIPSOID["WGS 84",6378137,298.257223563,
#>             LENGTHUNIT["metre",1]],
#>         ENSEMBLEACCURACY[2.0]],
#>     PRIMEM["Greenwich",0,
#>         ANGLEUNIT["degree",0.0174532925199433]],
#>     CS[ellipsoidal,2],
#>         AXIS["geodetic latitude (Lat)",north,
#>             ORDER[1],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         AXIS["geodetic longitude (Lon)",east,
#>             ORDER[2],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>     USAGE[
#>         SCOPE["Horizontal component of 3D system."],
#>         AREA["World."],
#>         BBOX[-90,-180,90,180]],
#>     ID["EPSG",4326]]
x <- list(
  xmin = -122.4195,
  ymin = 37.330219000000056,
  xmax = -122.030757,
  ymax = 37.77650360000007,
  spatialReference = list(wkid = 4326L, latestWkid = 4326L)
)

from_envelope(x)
#>       xmin       ymin       xmax       ymax 
#> -122.41950   37.33022 -122.03076   37.77650 
```
