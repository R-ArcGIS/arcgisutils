# Create a Geoprocessing Service Job

The `arc_gp_job` class is used to interact with Geoprocessing Services
in ArcGIS Online and Enterprise.

## Usage

``` r
new_gp_job(base_url, params = list(), token = arc_token())
```

## Arguments

- base_url:

  the URL of the job service (without `/submitJob`)

- params:

  a named list where each element is a scalar character

- token:

  default
  [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md).
  The token to be used with the job.

## Value

An object of class `arc_gp_job`.

## Details

The `arc_gp_job` uses S7 classes for the job request parameters and job
status via
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md)
and
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md)
respectively. Importantly,
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md)
ensures that parameters provided to a geoprocessing service are all
character scalars as required by the form body.

## Associated Functions

- `from_url(url, token = arc_token())`:

  Create a GP Job object from an existing job URL

## See also

Other geoprocessing:
[`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md),
[`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md),
[`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md),
[`gp_params`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)

## Public fields

- `base_url`:

  the URL of the job service (without `/submitJob`)

- `id`:

  the ID of the started job. `NULL` `self$start()` has not been called.

## Active bindings

- `params`:

  returns an S7 object of class `arc_form_params` (see
  [`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md))
  the list can be accessed via `self$params@params`.

- `status`:

  returns the status of the geoprocessing job as an S7 object of class
  `gp_job_status` (see
  [`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md))
  by querying the `/jobs/{job-id}` endpoint.

- `results`:

  returns the current results of the job by querying the
  `/jobs/{job-id}/results` endpoint.

## Methods

### Public methods

- [`arc_gp_job$new()`](#method-arc_gp_job-new)

- [`arc_gp_job$start()`](#method-arc_gp_job-start)

- [`arc_gp_job$cancel()`](#method-arc_gp_job-cancel)

- [`arc_gp_job$await()`](#method-arc_gp_job-await)

- [`arc_gp_job$clone()`](#method-arc_gp_job-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

#### Usage

    arc_gp_job$new(
      base_url,
      params = list(),
      result_fn = NULL,
      token = arc_token(),
      error_call = rlang::caller_call()
    )

#### Arguments

- `base_url`:

  the URL of the job service (without `/submitJob`)

- `params`:

  a named list where each element is a scalar character

- `result_fn`:

  Default `NULL`. An optional function to apply to the results JSON. By
  default parses results using
  [`RcppSimdJson::fparse()`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html).

- `token`:

  default
  [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md).
  The token to be used with the job.

- `error_call`:

  default
  [`rlang::caller_call()`](https://rlang.r-lib.org/reference/stack.html)
  the calling environment.

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Starts the job by calling the `/submitJob` endpoint. This also sets the
public field `id`.

#### Usage

    arc_gp_job$start()

------------------------------------------------------------------------

### Method `cancel()`

Cancels a job by calling the `/cancel` endpoint.

#### Usage

    arc_gp_job$cancel()

------------------------------------------------------------------------

### Method `await()`

Waits for job completion and returns results.

#### Usage

    arc_gp_job$await(interval = 0.1, verbose = FALSE)

#### Arguments

- `interval`:

  polling interval in seconds (default 0.1)

- `verbose`:

  whether to print status messages (default FALSE)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    arc_gp_job$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
url <- paste0(
  "https://logistics.arcgis.com/arcgis/",
  "rest/services/World/ServiceAreas/",
  "GPServer/GenerateServiceAreas"
)
job <- new_gp_job(url, list(f = "json"))
job
#> <arc_gp_job>
#> Job ID: not initiated
#> Status: not started
#> Resource: /GenerateServiceAreas
#> Params:
#> • f

# extract params S7 class
params <- job$params
params
#> <arcgisutils::arc_form_params>
#>  @ params:List of 1
#>  .. $ f: chr "json"

# view underlying list
params@params
#> $f
#> [1] "json"
#> 
```
