esri_transient_codes <- c(429L, 500L, 502L, 503L, 504L)

esri_is_transient <- function(resp) {
  if (httr2::resp_status(resp) %in% esri_transient_codes) {
    return(TRUE)
  }

  isTRUE(esri_body_error_code(resp) %in% esri_transient_codes)
}

# FIXME rename, `esri_body_error_code()` is a code smell
esri_body_error_code <- function(resp) {
  if (!httr2::resp_has_body(resp)) {
    return(NULL)
  }

  body <- httr2::resp_body_raw(resp)
  head <- body[seq_len(min(32L, length(body)))]

  # a binary body cannot be an error body and cannot be coerced to a string
  if (any(head == as.raw(0L))) {
    return(NULL)
  }

  if (!grepl('^\\s*\\{\\s*"error"', rawToChar(head))) {
    return(NULL)
  }

  RcppSimdJson::fparse(
    rawToChar(body),
    query = "/error/code",
    query_error_ok = TRUE,
    on_query_error = NULL,
    parse_error_ok = TRUE,
    on_parse_error = NULL
  )
}
