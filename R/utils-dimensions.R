#' Determine the dimensions of a geometry object
#'
#' Given an sfc or sfg object determine what dimensions are represented.
#'
#' @returns
#'
#' `determine_dims()` returns a scalar character of the value `"xy"`, `"xyz"`, or `"xyzm"` depending
#' on what dimensions are represented.
#'
#' `has_m()` and `has_z()` returns a logical scalar of `TRUE` or `FALSE` if the
#' geometry has a Z or M dimension.
#'
#'
#' @param x an object of class `sfc` or `sfg`
#' @export
#'
#' @examples
#'
#' geo <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)[["geometry"]]
#'
#' determine_dims(geo)
#' has_z(geo)
#' has_m(geo)
determine_dims <- function(x) UseMethod("determine_dims")

#' @export
determine_dims.sfc <- function(x) {
  c("xyzm", "xyz", "xy")[c(has_m(x), has_z(x), TRUE)][1]
}

#' @export
determine_dims.sfg <- function(x) {
  tolower(class(x)[1])
}


# helpers to check for z or m dimension

#' @export
#'@rdname determine_dims
has_m <- function(x) UseMethod("has_m")
#' @export
has_m.sfc <- function(x) !is.null(attr(x, "m_range"))
#' @export
has_m.sfg <- function(x) grepl("M", class(x)[1])

#' @export
#' @rdname determine_dims
has_z <- function(x) UseMethod("has_z")
#' @export
has_z.sfc <- function(x) !is.null(attr(x, "z_range"))
#' @export
has_z.sfg <- function(x) grepl("Z", class(x)[1])
