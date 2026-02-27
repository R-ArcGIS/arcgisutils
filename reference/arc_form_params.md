# Form request parameters

ArcGIS endpoints make extensive use of form encoded data for the body of
http requests. Form requests require that each element has a name and is
encoded as a single string—often as json.

## Usage

``` r
arc_form_params(params = list())

as_form_params(x)
```

## Arguments

- params:

  a named list with scalar character elements

- x:

  for `as_form_params()`, a named list to convert to form parameters

## Value

an object of class `arc_form_params`

## Details

The `arc_form_params` class provides validation of form body parameters
ensuring that each element is a scalar string. It uses a named list
internally to store the parameters.

The helper function `as_form_params()` converts a named list to form
parameters by automatically JSON-encoding each element using
[`yyjsonr::write_json_str()`](https://coolbutuseless.github.io/package/yyjsonr/reference/write_json_str.html)
with `auto_unbox = TRUE`.

## See also

Other geoprocessing:
[`arc_gp_job`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md),
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md),
[`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md),
[`gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)

Other geoprocessing:
[`arc_gp_job`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md),
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md),
[`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md),
[`gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)

## Examples

``` r
arc_form_params(
  list(f = "json", outFields = "*", where = "1 = 1")
)
#> <arcgisutils::arc_form_params>
#>  @ params:List of 3
#>  .. $ f        : chr "json"
#>  .. $ outFields: chr "*"
#>  .. $ where    : chr "1 = 1"
```
