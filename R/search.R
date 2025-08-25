# Item search fields
#   - title
#   - tags
#   - snippet
#   - description
#   - type
#   - typekeywords

# Groups search fields
#  - id
#  - title
#  - description
#  - snippet
#  - tags
#  - owner

# Filter

# Parameters:

# q
# bbox
# filter
# categories
# categoryFilter
# sortField: (title, created, type, owner, avgrating, numratings, numcomments, numviews, scorecompleteness)
# sortOrder: asc, desc
# countFields: type, access, contentstatus, categories,
# countSize: range 10:200
# exclude: max len is 20. comma separated
# displaySublayers: true / false
# MeanPixelSize: integer
# BandCount: integer
# displayServiceProperties: bool

search_term <- function(term, value, error_call = rlang::caller_call()) {
  check_string(term)
  if (!rlang::has_length(value, 1)) {
    cli::cli_abort("{.arg value} must be a scalar")
  }

  sprintf('%s:"%s"', term, value)
}

search_bool <- function(query, bool = c("AND", "OR", "NOT"), term, value) {
  bool <- rlang::arg_match(bool)
  sprintf("%s %s %s", query, bool, search_term(term, value))
}

search_and <- function(query, term, value) {
  search_bool(query, "AND", term, value)
}

search_or <- function(query, term, value) {
  search_bool(query, "OR", term, value)
}

# Helper to combine many items into one OR term
search_vals_or <- function(term, vals) {
  if (length(vals) == 0) {
    return("")
  }
  if (length(vals) == 1) {
    return(search_term(term, vals[1]))
  }

  first_term <- search_term(term, vals[1])
  remaining_vals <- vals[-1]

  Reduce(
    function(.prev, .next) {
      search_or(.prev, term, .next)
    },
    remaining_vals,
    init = first_term
  )
}

# Helper to combine many items into one AND term
search_vals_and <- function(term, vals) {
  if (length(vals) == 0) {
    return("")
  }
  if (length(vals) == 1) {
    return(search_term(term, vals[1]))
  }

  first_term <- search_term(term, vals[1])
  remaining_vals <- vals[-1]

  Reduce(
    function(.prev, .next) {
      search_and(.prev, term, .next)
    },
    remaining_vals,
    init = first_term
  )
}

search_not <- function(query, term, value) {
  search_bool(query, "NOT", term, value)
}


item_search_terms <- c(
  "id",
  "owner",
  "created",
  "modified",
  "title",
  "type",
  "typekeywords",
  "description",
  "tags",
  "snippet",
  "accessinformation",
  "access",
  "group",
  "numratings",
  "numcomments",
  "avgrating",
  "culture",
  "orgid",
  "categories",
  "contentstatus",
  "classification"
)

search_range <- function(
  param,
  from,
  to = Sys.Date(),
  error_call = rlang::caller_call()
) {
  check_string(param, call = error_call)

  from <- rlang::try_fetch(as.POSIXct(from), error = function(cnd) {
    cli::cli_abort(
      "Unable to coerce {.arg {param}} to a date time",
      call = error_call
    )
  })

  to <- rlang::try_fetch(as.POSIXct(to), error = function(cnd) {
    cli::cli_abort(
      "Unable to coerce {.arg {param}} to a date time",
      call = error_call
    )
  })

  sprintf(
    "%s: [%s TO %s]",
    param,
    format(date_to_ms(from), scientific = FALSE),
    format(date_to_ms(to), scientific = FALSE)
  )
}


#' @param query a scalar character for free text search or a valid query string as defined by the REST API.
#' @param title optional character vector of content item titles.
#' @param tags optional character vector of tags to search for.
#' @param owner optional character vector of owner usernames to search for.
#' @param item_type optional character vector of content item types. Validated with [`item_type()`].
#' @param type_keywords optional character vector of content tpye keywords. Validated with [`item_keyword()`].
#' @param created,modified optional length two vector which must be coercible to a date time vector. Converted using [`as.POSIXct()`]. Returns only items within this range.
#' @param categories optional character vector of up to 8 organization content categories.
#' @param category_filters optional character vector of up to 3 category terms. Items that have matching categories are returned. Exclusive with `categories`.
#' @param sort_field optional character vector of fields to sort by. Can sort by `title`, `created`, `type`, `owner`, `modified`, `avgrating`, `numratings`, `numcomments`, `numviews`, and `scorecompleteness`.
#' @param sort_order optional string. One of either `asc` or `desc` for ascending or descending order respectively.
#' @param count_fields optional character vector of up to 3 fields to count. Must be one of `c("type", "access", "contentstatus", "categories")`.
#' @param count_size optional integer determines the maximum number of field values to count for each counted field in `count_fields`. Maximum of 200.
#' @param display_sublayers default `FALSE`. Returns feature layers inside of feature services.
#' @param bbox unimplemented.
#'
#' @details
#'`r lifecycle::badge("experimental")`
#' ## Item Type
search_items <- function(
  query,
  title = NULL, #
  tags = NULL, #
  owner = NULL, #
  item_type = NULL, #
  type_keywords = NULL, #
  created = NULL,
  modified = NULL,
  categories = NULL,
  category_filters = NULL,
  sort_field = NULL,
  sort_order = NULL,
  count_fields = NULL,
  count_size = NULL,
  display_sublayers = FALSE,
  bbox = NULL,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  check_string(query)
  check_bool(display_sublayers)
  check_character(tags, allow_null = TRUE)
  check_character(categories, allow_null = TRUE)
  check_character(title, allow_null = TRUE)
  check_character(owner, allow_null = TRUE)
  check_character(item_type, allow_null = TRUE)
  check_character(type_keywords, allow_null = TRUE)

  # make title filter
  if (!is.null(title)) {
    title <- search_vals_or("title", title)
  }

  # make tags filter
  if (!is.null(tags)) {
    tags <- search_vals_or("tag", tags)
  }

  # make owner filter query
  if (!is.null(owner)) {
    owner <- search_vals_or("owner", owner)
  }

  # make item type filter
  if (!is.null(item_type)) {
    # validate the item types
    for (ty in item_type) {
      item_type(ty)
    }
    item_type <- search_vals_or("type", item_type)
  }

  # make typekeyword filter
  if (!is.null(type_keywords)) {
    # validate the item types
    for (ty in type_keywords) {
      item_keyword(ty)
    }

    type_keywords <- search_vals_or("typekeywords", type_keywords)
  }

  # handle created & modified ranges
  if (!is.null(created)) {
    if (!length(created) %in% c(1, 2)) {
      cli::cli_abort("{.arg created} must be a length 2 vector of dates")
    }
    created <- search_range(
      "created",
      created[[1]],
      created[[2]] %||% Sys.time()
    )
  }

  # handle created & modified ranges
  if (!is.null(modified)) {
    if (!length(modified) %in% c(1, 2)) {
      cli::cli_abort("{.arg modified} must be a length 2 vector of dates")
    }

    # we will allow a lenght 1 vector here and let it be the current time
    modified <- search_range(
      "modified",
      modified[[1]],
      modified[[2]] %||% Sys.time()
    )
  }

  if (!is.null(categories)) {
    if (length(categories) > 8) {
      cli::cli_abort("Only up to {.num 8} {.arg categories} are supported")
    }
    categories <- yyjsonr::write_json_str(categories, auto_unbox = TRUE)
  }

  if (!is.null(category_filters)) {
    if (length(category_filters) > 2) {
      cli::cli_abort(
        "Only up to {.num 2} {.arg category_filters} are supported"
      )
    }
    yyjsonr::write_json_str(category_filters, auto_unbox = TRUE)
  }

  if (!is.null(categories) && !is.null(category_filters)) {
    cli::cli_abort(c(
      "Only {.arg categories} or {.arg category_filters} can be set.",
      "i" = "They are mutually exclusive."
    ))
  }

  check_character(sort_field, allow_null = TRUE)

  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
      c(
        "title",
        "created",
        "type",
        "owner",
        "modified",
        "avgrating",
        "numratings",
        "numcomments",
        "numviews",
        "scorecompleteness"
      ),
      multiple = TRUE
    )
    sort_field <- yyjsonr::write_json_str(sort_field, auto_unbox = TRUE)
  }

  check_string(sort_order, allow_null = TRUE, allow_empty = FALSE)

  if (!is.null(sort_order)) {
    rlang::arg_match(sort_order, c("asc", "desc"))
  }

  check_character(count_fields, allow_null = TRUE)

  if (!is.null(count_fields)) {
    if (length(count_fields) > 3) {
      cli::cli_abort("Only up to 3 {.arg count_fields} are permitted")
    }
    count_fields <- yyjsonr::write_json_str(count_fields, auto_unbox = TRUE)
  }

  check_number_whole(count_size, allow_null = TRUE, min = 10, max = 200)
  if (!is.null(count_size)) {
    count_size <- as.integer(count_size)
  }

  all_params <- compact(
    list(
      title,
      tags,
      owner,
      item_type,
      type_keywords,
      created,
      modified,
      categories,
      category_filters,
      sort_field,
      sort_order,
      count_fields,
      count_size,
      search_term("displaySublayers", tolower(as.character(display_sublayers)))
    )
  )

  filter_query <- Reduce(
    \(.prev, .next) sprintf("%s AND %s", .prev, .next),
    all_params
  )
  filter_query

  all_resps <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "search"),
    query = c(q = query, filter = filter_query, f = "json")
  ) |>
    arc_paginate_req(
      page_size = page_size,
      max_pages = max_pages,
      .progress = .progress
    )

  results <- lapply(all_resps, function(.resp) {
    res <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors()

    res[["results"]]
  })

  data_frame(rbind_results(results))
}

# q <- r"{title:"Seven Natural Wonders of the World" OR description:"Seven Natural Wonders of the World"}"

# search_items(q)

# host = arc_host()
# token = arc_token()

# resp <- arc_base_req(
#   host,
#   token = token,
#   path = c("sharing", "rest", "search"),
#   query = c("f" = "json", q = 'owner:jparry_ANGP OR owner:NGiner_geosaurus')
# ) |>
#   httr2::req_perform()

# search_res <- resp |>
#   httr2::resp_body_string() |>
#   RcppSimdJson::fparse() |>
#   detect_errors()

# str(search_res, 1)
# res <- data_frame(search_res[["results"]])

# res$title

# arc_paginate_req()
