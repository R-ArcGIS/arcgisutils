# Set user-agent for arcgisutils

Override the default user-agent set by httr2 to indicate that a request
came from arcgisutils.

## Usage

``` r
arc_agent(req)
```

## Arguments

- req:

  an httr2 request

## Value

an httr2 request object

## Examples

``` r
req <- httr2::request("http://example.com")
arc_agent(req)
#> <httr2_request>
#> GET http://example.com
#> Body: empty
#> Options:
#> * useragent: "arcgisutils v0.5.0.9000"
```
