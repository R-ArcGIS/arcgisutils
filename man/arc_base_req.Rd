% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arc-base-req.R
\name{arc_base_req}
\alias{arc_base_req}
\title{Generate base request}
\usage{
arc_base_req(
  url,
  token = NULL,
  path = NULL,
  query = NULL,
  error_call = rlang::caller_env()
)
}
\arguments{
\item{url}{a valid url that is passed to \code{\link[httr2:request]{httr2::request()}}}

\item{token}{an object of class \code{httr2_token} as generated by \code{\link[=auth_code]{auth_code()}}
or related function}

\item{path}{a character vector of paths to be appended to url using \code{\link[httr2:req_url]{httr2::req_url_path_append()}}}

\item{query}{a named vector or named list of query parameters to be appended to the url using \code{\link[httr2:req_url]{httr2::req_url_query()}}}

\item{error_call}{the caller environment to be used when propagating errors.}
}
\value{
an \code{httr2_request} with the \code{X-Esri-Authorization} header and \code{User-Agent} set.
}
\description{
This function takes a url and creates a basic httr2 request that
adds the user-agent and adds an authorization token to the
\code{X-Esri-Authorization} header.
}
\examples{
arc_base_req("https://arcgis.com")
}
