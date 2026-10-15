# validate_crs() errors on invalid input

    Code
      validate_crs(4326.5)
    Condition
      Error:
      ! Invalid CRS supplied: `4326.5`
      i must be compatible with `sf::st_crs()`
    Code
      validate_crs(TRUE)
    Condition
      Error:
      ! Invalid CRS supplied: `TRUE`
      i must be compatible with `sf::st_crs()`
    Code
      validate_crs(list(4326))
    Condition
      Error:
      ! Invalid CRS supplied: `list(4326)`
      i must be compatible with `sf::st_crs()`

