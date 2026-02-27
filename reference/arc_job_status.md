# Geoprocessing Job Status

Represents the status of a geoprocessing job.

## Usage

``` r
arc_job_status(status = character(0))
```

## Arguments

- status:

  a scalar character. Must be one of `"esriJobSubmitted"`,
  `"esriJobWaiting"`, `"esriJobExecuting"`, `"esriJobSucceeded"`,
  `"esriJobFailed"`, `"esriJobTimedOut"`, `"esriJobCancelling"`, or
  `"esriJobCancelled`".

## Value

an object of class `arc_job_status`

## See also

Other geoprocessing:
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md),
[`arc_gp_job`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md),
[`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md),
[`gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)

## Examples

``` r
arc_job_status("esriJobSubmitted")
#> <arcgisutils::arc_job_status>
#>  @ status: chr "esriJobSubmitted"
```
