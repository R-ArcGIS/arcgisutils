# Date handling -----------------------------------------------------------

#' Date handling
#'
#' Esri date fields are represented as milliseconds from the Unix Epoch.
#'
#' - `is_date()`: checks if an object is a `Date` or `POSIXt` class object.
#' - `date_to_ms()` converts a date object to milliseconds from the Unix Epoch in the specified time zone.
#'
#' @param x an object of class `Date` or `POSIXt`. In the case of `is_date()`, any R object.
#' @inheritParams base::as.POSIXlt
#'
#' @rdname dates
#' @export
#' @returns
#' - `is_date()` returns a logical scalar
#' - `date_to_ms()` returns a numeric vector of times in milliseconds from the Unix Epoch in the specified time zone.
#' @examples
#'
#' today <- Sys.Date()
#'
#' is_date(today)
#'
#' date_to_ms(today)
#'
is_date <- function(x, tz) inherits(x, c("Date", "POSIXt"))

# a function to convert dates to ms
#' @rdname dates
#' @export
date_to_ms <- function(x, tz = "UTC") {
  if (!is_date(x)) {
    stop("`x` must inherit the `Date` or `POSIXt` class")
  }
  as.numeric(as.POSIXlt(x, tz = tz)) * 1000
}

#' @export
#' @rdname dates
from_esri_date <- function(x) {
  is_na <- x == -1
  x[is_na] <- NA_real_
  as.POSIXct(
    x / 1000,
    tz = "UTC",
    origin = "1970-01-01"
  )
}
