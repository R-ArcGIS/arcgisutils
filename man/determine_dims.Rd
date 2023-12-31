% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dimensions.R
\name{determine_dims}
\alias{determine_dims}
\alias{has_m}
\alias{has_z}
\title{Determine the dimensions of a geometry object}
\usage{
determine_dims(x)

has_m(x)

has_z(x)
}
\arguments{
\item{x}{an object of class \code{sfc} or \code{sfg}}
}
\value{
\code{determine_dims()} returns a scalar character of the value \code{"xy"}, \code{"xyz"}, or \code{"xyzm"} depending
on what dimensions are represented.

\code{has_m()} and \code{has_z()} returns a logical scalar of \code{TRUE} or \code{FALSE} if the
geometry has a Z or M dimension.
}
\description{
Given an sfc or sfg object determine what dimensions are represented.
}
\examples{

geo <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)[["geometry"]]

determine_dims(geo)
has_z(geo)
has_m(geo)
}
