# Portal Sharing API: coverage plan

Reference: <https://developers.arcgis.com/rest/users-groups-and-items/working-with-users-groups-and-items/>

## The gap

The bridge can create items but cannot manage them.

`arcgislayers` provides `add_item()`, `publish_item()`, and `publish_layer()`. Nothing in `arcgisutils` or `arcgislayers` can then share that item, move it into a folder, edit its metadata, or delete it. A published item is stranded as private content owned by the running account, and the workflow has to be finished by hand in the web UI.

That defeats the automation use case outright. It is the same wall the reporter of arcgisutils#52 hit: they moved to `auth_client()` specifically to run unattended, and unattended publishing that cannot share is not useful.

Current read-only coverage is good: `arc_item()`, `arc_item_data()`, `arc_group()`, `arc_group_content()`, `arc_group_users()`, `arc_user()`, `arc_user_self()`, `arc_user_content()`, `arc_portal_*()`, `search_items()`. Everything below is additive.

## Proposed functions

### Tier 1, completes the publish workflow

| Function | Endpoint | Method |
|---|---|---|
| `share_item()` | `/content/users/{user}/items/{id}/share` | POST |
| `unshare_item()` | `/content/users/{user}/items/{id}/unshare` | POST |
| `update_item()` | `/content/users/{user}/items/{id}/update` | POST |
| `delete_item()` | `/content/users/{user}/items/{id}/delete` | POST |
| `arc_folders()` | `/content/users/{user}` | GET |
| `create_folder()` | `/content/users/{user}/createFolder` | POST |
| `delete_folder()` | `/content/users/{user}/{folder}/delete` | POST |
| `move_items()` | `/content/users/{user}/{folder}/moveItems` | POST |

`share_item()` is the single highest-value addition. Without it every other publish feature is incomplete.

### Tier 2, relationships and discovery

| Function | Endpoint | Method |
|---|---|---|
| `arc_related_items()` | `/content/items/{id}/relatedItems` | GET |
| `arc_item_dependencies()` | `/content/items/{id}/dependencies` | GET |

Closes R-ArcGIS/arcgislayers#204.

### Tier 3, group administration

`create_group()`, `update_group()`, `delete_group()`, `add_group_users()`, `remove_group_users()`, `invite_group_users()`. Lower priority: most analysts join groups they did not create, and sharing to an existing group only needs its id.

## Design conventions

Follow what the package already does, so nothing new has to be learned:

- `host = arc_host()`, `token = arc_token()` trailing, as in `arc_item()`.
- `...` after the first argument to force naming.
- Requests built with `arc_base_req()`, parsed with `RcppSimdJson::fparse()`, checked with `detect_errors()`.
- Anything paginated goes through `arc_paginate_req()` and exposes `page_size`, `max_pages`, `.progress`, matching every other paginated wrapper.
- Accept a `PortalItem` **or** a bare item id string, as `arc_group_content()` already does for `PortalGroup`. Item-owner endpoints need the owner's username, which is on the `PortalItem`, so accepting the object avoids a second lookup.

### `share_item()` should not mirror the REST parameters

The endpoint takes `everyone` and `org` as independent booleans, which lets you express `everyone=false, org=false` (private) and `everyone=true, org=false` (public) but also invites nonsense combinations. A single enum is safer and reads better:

```r
share_item(item, access = "org", groups = NULL, confirm_item_control = FALSE)
```

mapping to the wire format as:

| `access` | `everyone` | `org` |
|---|---|---|
| `"private"` | `false` | `false` |
| `"org"` | `false` | `true` |
| `"public"` | `true` | `false` |

`groups` stays orthogonal, since an item can be shared to groups at any access level. The response carries `notSharedWith`, which must be surfaced as a warning rather than swallowed. A partial success that looks like a success is how people end up with data they believe is shared and is not.

## Type layer with `s7x`

The Sharing API is full of closed value sets currently passed as bare strings. `s7x::new_enum()` turns each into a validated class with generated documentation, replacing hand-written `rlang::arg_match()` calls and the bespoke validators in `R/portal-types.R`.

```r
ItemAccess <- new_enum("ItemAccess", c("private", "org", "public"))
GroupAccess <- new_enum("GroupAccess", c("private", "org", "public"))
RelationshipDirection <- new_enum("RelationshipDirection", c("forward", "reverse"))
SortOrder <- new_enum("SortOrder", c("asc", "desc"))
GroupRole <- new_enum("GroupRole", c("member", "admin", "owner"))
MembershipAccess <- new_enum("MembershipAccess", c("org", "collaboration", "none"))

ItemSortField <- new_enum("ItemSortField", c(
  "title", "created", "type", "owner", "modified",
  "avgRating", "numRatings", "numComments", "numViews"
))
```

`RelationshipType` is the one that earns its keep. There are 45 valid values, they are easy to mistype, and a typo returns an empty result rather than an error:

```r
RelationshipType <- new_enum("RelationshipType", c(
  "APIKey2Item", "App2DependentApp", "Area2CustomPackage", "Area2Package",
  "Data2App", "Data2Map", "Data2Scene", "Data2Survey", "Data2SurveyAddIn",
  "FeatureService2WorkforceMap", "Item2Attachment", "Item2Mission",
  "Item2Report", "Item2Solution", "Listed2ImplicitlyListed",
  "Listed2Provisioned", "Map2App", "Map2AppConfig", "Map2Area",
  "Map2FeatureCollection", "Map2IndoorsConfig", "Map2Service",
  "Map2StoryMapTheme", "Mission2Item", "MobileApp2Code", "Notebook2WebTool",
  "Scene2App", "Service2Data", "Service2Layer", "Service2Report",
  "Service2Route", "Service2Service", "Service2Style", "Service2Survey",
  "Solution2Item", "Style2Style", "Survey2Data", "Survey2Service",
  "SurveyAddIn2Data", "Theme2Story", "TrackView2Map", "WebStyle2DesktopStyle",
  "Widget2App", "WMA2Code", "WorkforceMap2FeatureService"
))
```

Used in a signature:

```r
arc_related_items <- function(
  item,
  relationship_type,
  direction = "forward",
  ...,
  host = arc_host(),
  token = arc_token()
) {
  relationship_type <- as.character(RelationshipType(relationship_type))
  direction <- as.character(RelationshipDirection(direction))
  ...
}
```

The docs list `relationshipTypes` as required, but that is wrong on two counts: the parameter is spelled `relationshipType`, and omitting it returns every relationship type in one request. So `relationship_type = NULL` is a legitimate and useful default rather than 45 calls. See `dev/rest-api-doc-issues.md`.

`item_type` and `item_keyword` in `R/portal-types.R` are the same pattern written by hand, with `portal_item_types()` and `portal_item_keywords()` as the variant lists. They should migrate to `new_enum()` once the dependency is available, which deletes both validators.

## Sequencing

`s7x` is now a hard dependency, declared with `Remotes: RConsortium/S7, josiahparry/s7x`. It requires `S7 (>= 0.2.2.9000)`, a development version, so **`arcgisutils` cannot go to CRAN until S7's dev release lands and `s7x` is published**. That is the same chain already blocking `arcgisviz`, recorded in the roadmap, so it does not add a new blocker so much as join an existing one.

The tradeoff was taken deliberately: the enums replace hand-written validators everywhere, and writing them twice to avoid a dependency that is landing anyway is wasted work.

## Verification

Every endpoint here requires authentication and most of them write. They cannot be verified without a token and a scratch organization, and several are destructive. Unit tests can cover request construction, argument validation, and response parsing against recorded payloads; actual round-trips need a live account.

Before building, decide whether there is a throwaway org these can be exercised against, or whether they ship covered only by construction tests.

## Open questions

- Should `share_item()` accept a `PortalGroup` vector for `groups`, or ids only? Accepting objects is friendlier and matches `arc_group_content()`.
- `delete_item()` and `delete_folder()` are destructive. Does the package want a confirmation prompt when `rlang::is_interactive()`, as `add_features()` does?
- Are `PortalItem` / `PortalGroup` / `PortalUser` worth promoting from classed lists to S7 classes? It would make the API considerably more discoverable, but it is a breaking change for anyone indexing them as lists today.
