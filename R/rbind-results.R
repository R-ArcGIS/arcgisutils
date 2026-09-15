#' Combine multiple data.frames
#'
#' A general function that takes a list of `data.frame`s and returns a single
#' and combines them into a single object. It will use the fastest method
#' available. In order this is [`collapse::rowbind()`], [`data.table::rbindlist()`],
#' [`vctrs::list_unchop()`], then `do.call(rbind.data.frame, x)`.
#'
#' If all items in the list are `data.frame`s, then the result will be a `data.frame`.
#' If all elements are an `sf` object, then the result will be an `sf` object.
#' If the items are mixed, the result will be a `data.frame`.
#'
#' If any items are `NULL`, then an attribute `null_elements` will be attached
#' to the result. The attribute is an integer vector of the indices that
#' were `NULL`.
#'
#' @param x a list where each element is a `data.frame` or `NULL`.
#' @param .ptype currently unused. Reserved for a future release.
#' @export
#' @returns see details.
#' @inheritParams parse_esri_json
#' @examples
#'
#' x <- head(iris)
#' res <- rbind_results(list(x, NULL, x))
#' attr(res, "null_elements")
rbind_results <- function(
  x,
  call = rlang::current_env(),
  .ptype = data.frame()
) {
  # use for loop for side effects
  # check that each element is a data.frame
  for (item in x) {
    check_data_frame(
      item,
      allow_null = TRUE,
      call = call,
      arg = rlang::caller_arg(x)
    )
  }

  # check if all results are sf, if so, we must return sf
  return_sf <- all(vapply(x, inherits_or_null, logical(1), class = "sf"))

  # identify which results are missing
  missing_elements <- vapply(x, is.null, logical(1))
  are_missing <- which(missing_elements)

  if (all(missing_elements)) {
    # return empty data.frame() if all missing
    # FIXME should we take a ptype here? to
    return(structure(data.frame(), null_elements = are_missing))
  }

  present <- x[!missing_elements]

  # a query for no fields yields rows without columns, which the backends below
  # cannot row-bind
  if (all(vapply(present, ncol, integer(1)) == 0L)) {
    n <- sum(vapply(present, nrow, integer(1)))
    res <- data.frame(row.names = seq_len(n))

    if (length(are_missing) > 0) {
      attr(res, "null_elements") <- are_missing
    }

    return(res)
  }

  # the row binding backends treat the geometry column as an ordinary column.
  # they bind it quickly but compare its class attribute, so pieces whose
  # geometry types differ have to be held back and concatenated separately.
  geometry_name <- NULL
  mixed_geometry <- FALSE

  if (return_sf) {
    geometry_name <- attr(present[[1L]], "sf_column")
    geometries <- lapply(present, function(p) p[[geometry_name]])
    filled <- geometries[lengths(geometries) > 0L]

    classes <- unique(vapply(filled, function(g) class(g)[1L], character(1)))
    mixed_geometry <- length(classes) > 1L

    if (mixed_geometry) {
      x <- lapply(x, function(p) {
        if (is.null(p)) {
          return(NULL)
        }

        p[[geometry_name]] <- NULL
        class(p) <- "data.frame"
        p
      })

      present <- x[!missing_elements]
    }
  }

  if (all(vapply(present, ncol, integer(1)) == 0L)) {
    # the geometry was the only column, so there is nothing left to bind
    x <- data.frame(row.names = seq_len(sum(vapply(present, nrow, integer(1)))))
  } else if (rlang::is_installed("collapse", version = "2.0.0")) {
    # ensure that a data.frame is always returned via return = 2L
    x <- collapse::rowbind(x, return = 2L, fill = TRUE)
  } else if (rlang::is_installed("data.table")) {
    x <- data.table::rbindlist(x)
    data.table::setDF(x)
  } else if (rlang::is_installed("vctrs")) {
    # vctrs::vec_rbind() doesn't handle NULL
    x <- vctrs::list_unchop(x, error_call = call)
  } else {
    x <- do.call(rbind.data.frame, x)
  }

  if (return_sf) {
    # results from one service share a crs object, so compare the attribute
    # directly and only fall back to the semantic comparison when it differs
    crs <- attr(geometries[[1L]], "crs")
    same <- vapply(geometries, function(g) identical(attr(g, "crs"), crs), logical(1))

    if (!all(same)) {
      equal <- vapply(
        geometries[!same],
        function(g) sf::st_crs(g) == sf::st_crs(crs),
        logical(1)
      )

      if (!all(equal)) {
        cli::cli_abort("All results must share a CRS.", call = call)
      }
    }

    geometry <- if (mixed_geometry) {
      out <- unlist(lapply(geometries, unclass), recursive = FALSE)
      attributes(out) <- attributes(geometries[[1L]])
      class(out) <- c("sfc_GEOMETRY", "sfc")
      out
    } else {
      # already bound by the backend, only its attributes are stale
      x[[geometry_name]]
    }

    # every piece already carries a correct bbox, so the combined one is the
    # bbox of those bboxes. that is O(pieces), where rescanning every geometry
    # the way c.sfc() does is O(features)
    corners <- vapply(filled, function(g) as.double(attr(g, "bbox")), double(4))

    # an sfc stores its bbox without a crs attribute, the crs lives on the sfc
    attr(geometry, "bbox") <- structure(
      c(
        xmin = min(corners[1L, ]),
        ymin = min(corners[2L, ]),
        xmax = max(corners[3L, ]),
        ymax = max(corners[4L, ])
      ),
      class = "bbox"
    )

    attr(geometry, "n_empty") <- sum(vapply(
      geometries,
      function(g) as.integer(attr(g, "n_empty")),
      integer(1)
    ))

    if (inherits(geometry, "sfc_GEOMETRY")) {
      attr(geometry, "classes") <- vapply(geometry, class, character(3))[2L, ]
    }

    x[[geometry_name]] <- geometry
    x <- sf::st_sf(x, sf_column_name = geometry_name)
  }

  if (length(are_missing) > 0) {
    attr(x, "null_elements") <- are_missing
  }

  x
}

#' Check if an object is NULL or inherits a class
#'
#' Uses [`rlang::inherits_any()`] for the class check.
#' @keywords internal
#' @noRd
inherits_or_null <- function(x, class) {
  if (is.null(x)) {
    return(TRUE)
  } else {
    rlang::inherits_any(x, class)
  }
}
