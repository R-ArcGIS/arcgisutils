test_that("arc_url_parse works", {
  furl <- "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer"

  furl_parsed <- arc_url_parse(furl)

  expect_identical(furl_parsed[["type"]], "MapServer")
  expect_identical(furl_parsed[["url"]], furl)

  furl_alt <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/North%20Carolina%20SIDS%20sample/FeatureServer"

  expect_identical(arc_url_parse(furl_alt)[["type"]], "FeatureServer")
})


test_that("arc_url_type() is inferred correctly", {
  skip_if_offline()

  test_cases <- c(
    map_server = "https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2000_WM/MapServer",
    feature_layer = "https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2000_WM/MapServer/0",
    feature_server = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Major_Cities_/FeatureServer",
    scene = "https://tiles.arcgis.com/tiles/oPre3pOfRfefL8y0/arcgis/rest/services/3D_Buildings_Switzerland_wgs84/SceneServer",
    tile_imagery = "https://image.arcgisonline.nl/arcgis/rest/services/KEA/Maximale_overstromingsdiepte/ImageServer",
    elevation = "https://tiles.arcgis.com/tiles/qHLhLQrcvEnxjtPr/arcgis/rest/services/British_National_Grid_Terrain_3D/ImageServer",
    webmap_app = "https://esri2.maps.arcgis.com/apps/instant/media/index.html?appid=80eb92ffc89b4086abe8cedd58ab160c",
    storymap = "https://storymaps.arcgis.com/stories/ad791fda858c46fdbe79636aa5f35dd8",
    instant_app = "https://actgov.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=f2dfd67d29ed4cabbb91e742e0297955",
    dashboard = "https://www.arcgis.com/apps/dashboards/84ba9c03786e462d960e3172bc1b2204",
    datapipeline = "https://analysis-1.maps.arcgis.com/apps/datapipelines/editor?item=10db3e15b22948faabf8ecdf9af30065",
    experience = "https://experience.arcgis.com/experience/6e360741bfd84db79d5db774a1147815",
    webapp = "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=950b4eec577a4dc5b298a61adab41c06",
    notebook_item = "https://geosaurus.maps.arcgis.com/home/item.html?id=9a9fca3f09bb41dd856c9cd4239b8519",
    notebook = "https://geosaurus.maps.arcgis.com/home/notebook/notebook.html?id=9a9fca3f09bb41dd856c9cd4239b8519",
    webscene = "https://analysis-1.maps.arcgis.com/home/webscene/viewer.html?webscene=7b506043536246faa4194d4c3d4c921b",
    group = "https://analysis-1.maps.arcgis.com/home/group.html?id=2f0ec8cb03574128bd673cefab106f39#overview",
    user = "https://analysis-1.maps.arcgis.com/home/user.html?user=jparry_ANGP",
    item_db = "https://analysis-1.maps.arcgis.com/home/item.html?id=84ba9c03786e462d960e3172bc1b2204",
    item_mapserver = "https://analysis-1.maps.arcgis.com/home/item.html?id=1d150c40d9f642cb8bd691017bf22cee",
    feature_collection = "https://analysis-1.maps.arcgis.com/home/item.html?id=24aa36ce1d7747c2b5a6aa57711d03fb",
    gp_server = "https://gis.pikepa.org/arcgis/rest/services/Utilities/GeocodingTools/GPServer",
    geometry_server = "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Utilities/Geometry/GeometryServer"
  )

  expected_types <- c(
    "MapServer",
    "MapServer",
    "FeatureServer",
    "SceneServer",
    "ImageServer",
    "ImageServer",
    "app",
    "storymap",
    "app",
    "dashboard",
    "datapipeline",
    "experience",
    "webapp",
    "item",
    "notebook",
    "webscene",
    "group",
    "user",
    "item",
    "item",
    "item",
    "GPServer",
    "GeometryServer"
  )

  for (i in seq_along(test_cases)) {
    expect_identical(arc_url_type(test_cases[[i]]), expected_types[i])
  }
})
