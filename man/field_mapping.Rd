% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esri-field-mapping.R
\name{infer_esri_type}
\alias{infer_esri_type}
\alias{get_ptype}
\alias{ptype_tbl}
\alias{remote_ptype_tbl}
\title{Esri field type mapping}
\usage{
infer_esri_type(
  .data,
  arg = rlang::caller_arg(.data),
  call = rlang::caller_env()
)

get_ptype(field_type, n = 1, call = rlang::caller_env())

ptype_tbl(fields, n = 0, call = rlang::caller_env())

remote_ptype_tbl(fields, call = rlang::caller_env())
}
\arguments{
\item{.data}{an object of class \code{data.frame}.}

\item{arg}{An argument name in the current function.}

\item{call}{The execution environment of a currently running
function, e.g. \code{call = caller_env()}. The corresponding function
call is retrieved and mentioned in error messages as the source
of the error.

You only need to supply \code{call} when throwing a condition from a
helper function which wouldn't be relevant to mention in the
message.

Can also be \code{NULL} or a \link[rlang:topic-defuse]{defused function call} to
respectively not display any call or hard-code a code to display.

For more information about error calls, see \ifelse{html}{\link[rlang:topic-error-call]{Including function calls in error messages}}{\link[rlang:topic-error-call]{Including function calls in error messages}}.}

\item{field_type}{a character of a desired Esri field type. See details for more.}

\item{n}{the number of rows to create in the prototype table}

\item{fields}{a data.frame containing, at least, the columns \code{type} and \code{name}.
Typically retrieved from the \code{field} metadata from a \code{FeatureLayer} or \code{Table}.
Also can use the output of \code{infer_esri_type()}.}
}
\value{
\itemize{
\item \code{get_pytpe()} returns an object of the class of the prototype.
\item \code{ptype_tbl()} takes a \code{data.frame} with columns \code{name} and \code{type} and creates an empty \code{data.frame} with the corresponding columns and R types
\item \code{remote_ptype_tbl()} provides the results of \code{ptype_tbl()} as a lazy data frame from the \code{dbplyr} package.
\item \code{infer_esri_ptype()} returns a \code{data.frame} with columns \code{name}, \code{type}, \code{alias}, \code{nullable}, and \code{editable} columns
\itemize{
\item This resembles that of the \code{fields} returned by a FeatureService
}
}
}
\description{
Infers Esri field types from R objects.
}
\details{
\itemize{
\item \code{get_ptype()} takes a scalar character containing the Esri field type and returns a prototype of the pertinent R type
\item \code{infer_esri_type()} takes a data frame-like object and infers the Esri field type from it.
\item \code{remote_ptype_tbl()} takes a data frame of fields as derived from \code{list_fields()} and
creates a lazy table proto type intended to be used with \code{dbplyr} integration
}
\subsection{Field type mapping:}{

Esri field types are mapped as
\itemize{
\item \code{esriFieldTypeSmallInteger}: integer
\item \code{esriFieldTypeSingle}: double
\item \code{esriFieldTypeGUID}: integer
\item \code{esriFieldTypeOID}: integer
\item \code{esriFieldTypeInteger}: integer
\item \code{esriFieldTypeBigInteger}: double
\item \code{esriFieldTypeDouble}: double
\item \code{esriFieldTypeString}: character
\item \code{esriFieldTypeDate}: date
}

R types are mapped as
\itemize{
\item \code{double}: esriFieldTypeDouble
\item \code{integer}: esriFieldTypeInteger
\item \code{character}: esriFieldTypeString
\item \code{date}: esriFieldTypeDate
\item \code{raw}: esriFieldTypeBlob
}
}
}
\examples{

get_ptype("esriFieldTypeDouble")
inferred <- infer_esri_type(iris)
ptype_tbl(inferred)

}
