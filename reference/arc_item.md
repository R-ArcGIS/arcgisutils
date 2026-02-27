# Portal Item Metadata

Given the unique ID of a content item, fetches the item's metadata from
a portal.

## Usage

``` r
arc_item(item_id, host = arc_host(), token = arc_token())
```

## Arguments

- item_id:

  the ID of the item to fetch. A scalar character.

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

## Value

an object of class `PortalItem` which is a list with the item's
metadata.

## Details

See [API
Reference](https://developers.arcgis.com/rest/users-groups-and-items/item/)
for more information.

**\[experimental\]**

## See also

Other portal item:
[`arc_item_data()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item_data.md)

## Examples

``` r
arc_item("9df5e769bfe8412b8de36a2e618c7672")
#> <PortalItem<Feature Service>>
#> id: 9df5e769bfe8412b8de36a2e618c7672
#> title: USA Major Cities
#> owner: esri_dm
```
