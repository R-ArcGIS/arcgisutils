hydro_url <- "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer"

#' @references https://developers.arcgis.com/rest/elevation/api-reference/trace-downstream.htm
trace_downstream <- function(
  input_points,
  point_id_field = NULL,
  resolution = NULL,
  generalize = FALSE,
  token = arc_token()
) {
  # TODO validate number of rows
  check_string(point_id_field, allow_null = TRUE)
  check_bool(generalize)
  if (!rlang::is_null(resolution)) {
    resolution <- rlang::arg_match(resolution, c("finest", "10m", "30m", "90m"))
  }

  params <- compact(list(
    InputPoints = as_esri_featureset(input_points),
    PointIdField = point_id_field,
    DataSourceResolution = resolution,
    Generalize = as.character(generalize),
    f = "json"
  ))

  trace_downstream_job <- R6::R6Class("trace_downstream", inherit = arc_gp_job)
  trace_downstream_job$new(
    "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/TraceDownstream",
    params,
    token
  )
}

input_points <- sf::st_sfc(sf::st_point(c(-159.548936, 21.955888)), crs = 4326)
arcgisutils::as_esri_features(input_points)

set_arc_token(auth_user())

job <- trace_downstream(input_points)
job$start()
job$status

job$results
job$results
