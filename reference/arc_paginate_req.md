# Paginate ArcGIS Requests

Many API endpoints provide common [pagination
properties](https://developers.arcgis.com/rest/users-groups-and-items/common-parameters/#paging-properties).
`arc_paginate_request()` automatically applies pagination to an input
request.

## Usage

``` r
arc_paginate_req(req, page_size = 10, max_pages = Inf, .progress = TRUE)
```

## Arguments

- req:

  an `httr2_request` ideally created with `arc_base_req`

- page_size:

  a scalar integer between 1 and 100 indicating the number of responses
  per page.

- max_pages:

  the maximum number of pages to fetch. By default fetches all pages.

- .progress:

  default `TRUE`. Whether to display a progress bar for requests.

## Value

a list of `httr2_response`.

## References

[API
Documentation](https://developers.arcgis.com/rest/users-groups-and-items/common-parameters/#paging-properties)

## See also

[`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
