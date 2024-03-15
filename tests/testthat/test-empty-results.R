test_that("parse_esri_json(): 0 feat. results", {
  jsn <- '{"displayFieldName":"GNIS_NAME","fieldAliases":{"OBJECTID":"OBJECTID","permanent_identifier":"Permanent_Identifier","fdate":"FDate","resolution":"Resolution","gnis_id":"GNIS_ID","gnis_name":"GNIS_Name","lengthkm":"LengthKm","ftype":"FType","fcode":"FCode","visibilityfilter":"VisibilityFilter","nhdplusid":"NHDPlusID","vpuid":"VPUID","Shape_Length":"Shape_Length"},"geometryType":"esriGeometryPolyline","spatialReference":{"wkid":102100,"latestWkid":3857,"vcsWkid":5703,"latestVcsWkid":5703},"fields":[{"name":"OBJECTID","type":"esriFieldTypeOID","alias":"OBJECTID"},{"name":"permanent_identifier","type":"esriFieldTypeString","alias":"Permanent_Identifier","length":40},{"name":"fdate","type":"esriFieldTypeDate","alias":"FDate","length":8},{"name":"resolution","type":"esriFieldTypeInteger","alias":"Resolution"},{"name":"gnis_id","type":"esriFieldTypeString","alias":"GNIS_ID","length":10},{"name":"gnis_name","type":"esriFieldTypeString","alias":"GNIS_Name","length":65},{"name":"lengthkm","type":"esriFieldTypeDouble","alias":"LengthKm"},{"name":"ftype","type":"esriFieldTypeInteger","alias":"FType"},{"name":"fcode","type":"esriFieldTypeInteger","alias":"FCode"},{"name":"visibilityfilter","type":"esriFieldTypeInteger","alias":"VisibilityFilter"},{"name":"nhdplusid","type":"esriFieldTypeDouble","alias":"NHDPlusID"},{"name":"vpuid","type":"esriFieldTypeString","alias":"VPUID","length":8},{"name":"Shape_Length","type":"esriFieldTypeDouble","alias":"Shape_Length"}],"features":[]}'
  res <- parse_esri_json(jsn)
  expect_true(is.data.frame(res))
  expect_true(identical(nrow(res), 0L))

  known_names <- c("OBJECTID", "permanent_identifier", "fdate", "resolution",
    "gnis_id", "gnis_name", "lengthkm", "ftype", "fcode", "visibilityfilter",
    "nhdplusid", "vpuid", "Shape_Length")

  expect_identical(names(res), known_names)

})
