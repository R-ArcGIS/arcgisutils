# https://developers.arcgis.com/rest/users-groups-and-items/group-content/

library(arcgis)

# authenticate
set_arc_token(auth_user())

# fetch group based on ID
group <- arc_group("2f0ec8cb03574128bd673cefab106f39")

# get all items from the group
group_items <- arc_group_content(group)

# preview
dplyr::glimpse(group_items)

arc_group_content <- function(
  group,
  host = arc_host(),
  token = arc_token()
) {
  if (inherits(group, "PortalGroup")) {
    group <- group$id
  }

  if (!rlang::is_string(group)) {
    cli::cli_abort(
      "{.arg group} must be a string or {.cls PortalGroup} created with {.fn arc_group}"
    )
  }

  req <- arc_base_req(
    host,
    path = c("sharing", "rest", "content", "groups", group),
    query = c("f" = "json"),
    token = token
  )

  all_resps <- arc_paginate_req(req)

  results <- lapply(all_resps, function(.resp) {
    RcppSimdJson::fparse(httr2::resp_body_string(.resp))[["items"]]
  })

  res <- data_frame(rbind_results(results))

  res[["created"]] <- from_esri_date(res[["created"]])
  res[["modified"]] <- from_esri_date(res[["modified"]])
  res[["lastViewed"]] <- from_esri_date(res[["lastViewed"]])
  res
}


# https://developers.arcgis.com/rest/users-groups-and-items/user-content/
user <- arc_user("8e362f5fd7e1415aa648e9c3c14784dc")
arc_user_content <- function(
  user,
  host = arc_host(),
  token = arc_token()
) {
  if (inherits(user, "PortalUser")) {
    user <- user$username
  }

  if (!rlang::is_string(user)) {
    cli::cli_abort(
      "{.arg user} must be a string or {.cls PortalUser} created with {.fn arc_user}"
    )
  }

  req <- arc_base_req(
    host,
    path = c("sharing", "rest", "content", "users", user),
    query = c("f" = "json"),
    token = token
  )

  all_resps <- arc_paginate_req(req)

  results <- lapply(all_resps, function(.resp) {
    RcppSimdJson::fparse(httr2::resp_body_string(.resp))[["items"]]
  })

  res <- data_frame(rbind_results(results))

  res[["created"]] <- from_esri_date(res[["created"]])
  res[["modified"]] <- from_esri_date(res[["modified"]])
  res[["lastViewed"]] <- from_esri_date(res[["lastViewed"]])
  res
}

# https://organization.example.com/<context>/sharing/rest/content/groups/4774c1c2b79046f285b2e86e5a20319?f=pjson
# todo: support pagination https://developers.arcgis.com/rest/users-groups-and-items/common-parameters/#paging-properties

arc_paginate_req <- function(
  req,
  page_size = 10,
  max_pages = Inf,
  .progress = TRUE
) {
  check_number_whole(page_size, 1, 100)
  check_number_whole(max_pages, min = 1, allow_infinite = TRUE)
  httr2::req_perform_iterative(
    httr2::req_url_query(req, num = page_size),
    arc_next_req,
    max_reqs = max_pages,
    on_error = "return",
    progress = .progress
  )
}

arc_next_req <- function(resp, req) {
  if (httr2::resp_is_error(resp)) {
    return(NULL)
  }

  resp_type <- httr2::resp_content_type(resp)

  if (!resp_type %in% c("text/plain", "application/json")) {
    cli::cli_warn("Response returned {.val {resp_type}} but expected json")
    return(NULL)
  }

  # extract next page
  next_start <- rlang::try_fetch(
    {
      RcppSimdJson::fparse(
        httr2::resp_body_string(resp),
        query = "/nextStart"
      )
    },
    error = function(cnd) {
      cli::cli_warn("Failed to extract next page—ending pagination")
      NULL
    }
  )

  # if we didnt get an integer warn and return NULL
  if (!rlang::is_integerish(next_start)) {
    cli::cli_warn(c(
      "Error extracting next page.",
      "i" = "Expected an integer found {obj_type_friendly(next_start)}"
    ))
    return(NULL)
  }

  # -1 when pagination is over
  if (next_start < 1) {
    return(NULL)
  }

  # next response
  httr2::req_url_query(req, start = next_start)
}
