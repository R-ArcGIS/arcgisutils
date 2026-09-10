# ArcGIS REST API documentation issues

Discrepancies between the published docs and observed API behaviour, found while
building Sharing API coverage. Each entry records how it was verified so the docs
team can reproduce it.

---

## 1. `relatedItems`: parameter is named `relationshipType`, not `relationshipTypes`

**Page:** <https://developers.arcgis.com/rest/users-groups-and-items/related-items/>

**Documented:** the required parameter is listed as `relationshipTypes` (plural).

**Observed:** the API reads `relationshipType` (singular). The singular form is
validated; a bogus value returns `Invalid Relationship Type.` The plural form is
accepted but silently ignored, so a caller following the docs gets unfiltered
results and no error.

**Verified with:**

```r
u <- "https://www.arcgis.com/sharing/rest/content/items/9df5e769bfe8412b8de36a2e618c7672/relatedItems"

# singular is validated
httr2::request(u) |>
  httr2::req_url_query(f = "json", direction = "forward", relationshipType = "Bogus") |>
  httr2::req_perform() |>
  httr2::resp_body_string()
#> {"error":{... "message":"Invalid Relationship Type." ...}}
```

**Impact:** anyone following the documented spelling silently receives every
relationship type instead of the one they asked for. This is worse than an
error, because the result looks plausible.

---

## 2. `relatedItems`: `relationshipType` is documented as required but is optional

**Page:** <https://developers.arcgis.com/rest/users-groups-and-items/related-items/>

**Documented:** `relationshipTypes` is listed under required parameters.

**Observed:** omitting it entirely succeeds and returns relationships of every
type. This is genuinely useful behaviour and worth documenting rather than
removing, since the alternative is 46 requests to enumerate everything related
to an item.

**Verified with:**

```r
# no relationshipType at all
httr2::request(u) |>
  httr2::req_url_query(f = "json", direction = "forward") |>
  httr2::req_perform()
#> total = 100
```

---

## 3. `relatedItems`: `direction` is documented as one of two required parameters but behaves as strictly required

**Page:** <https://developers.arcgis.com/rest/users-groups-and-items/related-items/>

**Observed:** omitting `direction` returns `total = 0` rather than an error, on an
item that returns 100 results when `direction=forward` is supplied.

**Verified with:**

```r
httr2::request(u) |>
  httr2::req_url_query(f = "json", relationshipType = "Service2Data") |>
  httr2::req_perform()
#> total = 0
```

**Impact:** a missing `direction` is indistinguishable from an item that genuinely
has no relationships. An error would be far more useful than an empty result.

---

## 4. Relationship types are enumerated on one page but described as examples on another

**Pages:**
- <https://developers.arcgis.com/rest/users-groups-and-items/relationship-types/> gives the full table of 46 values.
- <https://developers.arcgis.com/rest/users-groups-and-items/related-items/> describes them as examples ("e.g., Map2Service, Service2Service, WMA2Code") without linking to the complete list.

**Suggestion:** link the `relatedItems` parameter description to the relationship
types page. Without it the parameter reads as open-ended when it is a closed set
of 46.
