# Portal Item Types

Every portal item has an associated item type. Each of those item types
have keywords which cna be used to help narrow down search further.

## Usage

``` r
item_type(item_type = character(0))

item_keyword(keyword = character(0))

portal_item_keywords()

portal_item_types()
```

## Arguments

- item_type:

  a scalar character of the item type. See `portal_item_types()` for
  valid item types.

- keyword:

  a scalar character of the item type keyword. See
  `portal_item_keywords()`.

## References

[REST API
Documentation](https://developers.arcgis.com/rest/users-groups-and-items/items-and-item-types)
