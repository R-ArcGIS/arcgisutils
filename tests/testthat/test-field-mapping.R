test_that("field types are appropriately mapped", {
  fields <- data.frame(
    name = c(
      "ObjectID",
      "Name",
      "Locations",
      "BarrierType",
      "Attr_Minutes",
      "Attr_TimeAt1KPH",
      "Attr_TravelTime",
      "Attr_TruckMinutes",
      "Attr_TruckTravelTime",
      "Attr_WalkTime",
      "Attr_Kilometers",
      "Attr_Miles",
      "Shape_Length",
      "Shape_Area"
    ),
    type = rep(
      c(
        "esriFieldTypeOID",
        "esriFieldTypeString",
        "esriFieldTypeBlob",
        "esriFieldTypeInteger",
        "esriFieldTypeDouble"
      ),
      rep(c(1L, 10L), c(4L, 1L))
    ),
    alias = c(
      "ObjectID",
      "Name",
      "Locations",
      "BarrierType",
      "Attr_Minutes",
      "Attr_TimeAt1KPH",
      "Attr_TravelTime",
      "Attr_TruckMinutes",
      "Attr_TruckTravelTime",
      "Attr_WalkTime",
      "Attr_Kilometers",
      "Attr_Miles",
      "Shape_Length",
      "Shape_Area"
    ),
    length = rep(c(NA, 500L, NA), c(1L, 1L, 12L))
  )

  expected <- structure(
    list(
      ObjectID = integer(0),
      Name = character(0),
      Locations = list(),
      BarrierType = integer(0),
      Attr_Minutes = numeric(0),
      Attr_TimeAt1KPH = numeric(0),
      Attr_TravelTime = numeric(0),
      Attr_TruckMinutes = numeric(0),
      Attr_TruckTravelTime = numeric(0),
      Attr_WalkTime = numeric(0),
      Attr_Kilometers = numeric(0),
      Attr_Miles = numeric(0),
      Shape_Length = numeric(0),
      Shape_Area = numeric(0)
    ),
    class = c("tbl", "data.frame"),
    row.names = c(NA, 0L)
  )

  expect_identical(fields_as_ptype_df(fields), expected)
})
