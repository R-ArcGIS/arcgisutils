#' Portal enumerations
#'
#' Closed value sets used by the Portal sharing API. Each is an
#' [`s7x::Enum`] which validates on construction and can be cast back to a
#' character with [`as.character()`].
#'
#' @param value a scalar character. One of the variants listed for the class.
#' @name portal_enums
#' @family portal item
#' @returns An object inheriting from [`s7x::Enum`].
#' @examples
#' ItemAccess("org")
#' as.character(ItemAccess("public"))
#'
#' RelationshipDirection("forward")
#'
#' try(ItemAccess("everyone"))
NULL

#' @rdname portal_enums
#' @export
ItemAccess <- s7x::new_enum(
  "ItemAccess",
  c("private", "org", "public"),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
GroupAccess <- s7x::new_enum(
  "GroupAccess",
  c("private", "org", "public"),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
RelationshipDirection <- s7x::new_enum(
  "RelationshipDirection",
  c("forward", "reverse"),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
SortOrder <- s7x::new_enum(
  "SortOrder",
  c("asc", "desc"),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
GroupRole <- s7x::new_enum(
  "GroupRole",
  c("member", "admin", "owner"),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
ItemSortField <- s7x::new_enum(
  "ItemSortField",
  c(
    "title",
    "created",
    "type",
    "owner",
    "modified",
    "avgRating",
    "numRatings",
    "numComments",
    "numViews"
  ),
  package = "arcgisutils",
  allow_na = FALSE
)

#' @rdname portal_enums
#' @export
RelationshipType <- s7x::new_enum(
  "RelationshipType",
  c(
    "APIKey2Item",
    "App2DependentApp",
    "Area2CustomPackage",
    "Area2Package",
    "Data2App",
    "Data2Map",
    "Data2Scene",
    "Data2Survey",
    "Data2SurveyAddIn",
    "FeatureService2WorkforceMap",
    "Item2Attachment",
    "Item2Mission",
    "Item2Report",
    "Item2Solution",
    "Listed2ImplicitlyListed",
    "Listed2Provisioned",
    "Map2App",
    "Map2AppConfig",
    "Map2Area",
    "Map2FeatureCollection",
    "Map2IndoorsConfig",
    "Map2Service",
    "Map2StoryMapTheme",
    "Mission2Item",
    "MobileApp2Code",
    "Notebook2WebTool",
    "Scene2App",
    "Service2Data",
    "Service2Layer",
    "Service2Report",
    "Service2Route",
    "Service2Service",
    "Service2Style",
    "Service2Survey",
    "Solution2Item",
    "Style2Style",
    "Survey2Data",
    "Survey2Service",
    "SurveyAddIn2Data",
    "Theme2Story",
    "TrackView2Map",
    "WebStyle2DesktopStyle",
    "Widget2App",
    "WMA2Code",
    "WorkforceMap2FeatureService"
  ),
  package = "arcgisutils",
  allow_na = FALSE
)

# s7x reports the failing property, not the argument it came from
enum_arg <- function(
  enum,
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  rlang::try_fetch(
    as.character(enum(value)),
    error = function(cnd) {
      cli::cli_abort("{.arg {arg}} is not valid.", parent = cnd, call = call)
    }
  )
}
