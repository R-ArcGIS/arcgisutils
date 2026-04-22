# Date handling

Esri date fields are represented as milliseconds from the Unix Epoch.

## Usage

``` r
is_date(x, tz)

date_to_ms(x, tz = "UTC")

from_esri_date(x)
```

## Arguments

- x:

  an object of class `Date` or `POSIXt`. In the case of `is_date()`, any
  R object.

- tz:

  a character string. The time zone specification to be used for the
  conversion, *if one is required*. System-specific (see [time
  zones](https://rdrr.io/r/base/timezones.html)), but `""` is the
  current time zone, and `"GMT"` is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

- `is_date()` returns a logical scalar

- `date_to_ms()` returns a numeric vector of times in milliseconds from
  the Unix Epoch in the specified time zone.

## Details

- `is_date()`: checks if an object is a `Date` or `POSIXt` class object.

- `date_to_ms()` converts a date object to milliseconds from the Unix
  Epoch in the specified time zone.

## Examples

``` r
today <- Sys.Date()

is_date(today)
#> [1] TRUE

date_to_ms(today)
#> [1] 1.776816e+12
```
