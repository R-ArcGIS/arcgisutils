# Create GP Job from existing URL

Create GP Job from existing URL

## Usage

``` r
gp_job_from_url(url, token = arc_token())
```

## Arguments

- url:

  the url of an existing geoprocessing job

- token:

  default
  [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md).
  The token to be used with the job.

## See also

Other geoprocessing:
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md),
[`arc_gp_job`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md),
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md),
[`gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)

## Examples

``` r
if (interactive()) {
job_url <- paste0(
  "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/",
  "GPServer/TraceDownstream/jobs/jfde67910074649e4a567f0adbb8af870"
)

gp_job_from_url(
  job_url,
  token = auth_user()
)
}
```
