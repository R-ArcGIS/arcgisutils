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

info <- arc_url_parse(test_cases["geometry_server"])

switch(
  info$type,
  "GeoDataServer" = NULL, # FIXME
  "WFSServer" = NULL, # FIXME
  "WFCServer" = NULL, # FIXME
  "webmap" = NULL, # FIXME
  "GeometryServer" = {
    res <- fetch_layer_metadata(info$url)
    res[["url"]] <- info$url
    res
  }, # FIXME, unclear how to use this...
  "GPServer" = {
    res <- fetch_layer_metadata(info$url)
    res[["url"]] <- info$url
    res
  },
  "GeocodeServer" = arcgisgeocode::geocode_server(info$url),
  "item" = {
    # if we have an item url, we fetch the item
    item <- arc_item(info$query$id)

    # if there is no associated url we return the item
    if (is.null(item[["url"]])) {
      return(item)
    }

    url_type <- arc_url_type(item[["url"]])

    if (is.null(url_type)) {
      return(item)
    }

    # if there is a URL we check if it is a known service type
    # if so we use arc_open on that
    if (url_type %in% arc_service_types) {
      return(arcgislayers::arc_open(item[["url"]]))
    }

    # otherwise, we return the item itself
    item
  },
  "SceneServer" = arcgislayers::arc_open(info$url),
  "user" = arc_user(info$query$user),
  "group" = arc_group(group_id),
  "webscene" = arc_item(info$query$webscene),
  "app" = arc_item(info$query$appid),
  "notebook" = arc_item(info$query$id),
  "experience" = {
    path_components <- strsplit(info$path, "/")[[1]]
    exp_id <- path_components[which(path_components == "experience") + 1]
    arc_item(exp_id)
  },
  "storymap" = {
    path_components <- strsplit(info$path, "/")[[1]]
    sm_id <- path_components[which(path_components == "stories") + 1]
    arc_item(sm_id)
  },
  "dashboard" = {
    path_components <- strsplit(info$path, "/")[[1]]
    db_id <- path_components[which(path_components == "dashboards") + 1]
    arc_item(db_id)
  },
  "datapipeline" = arc_item(info$query$item),
  "webapp" = arc_item(info$query$id),
  "FeatureServer" = arcgislayers::arc_open(info$url),
  "MapServer" = arcgislayers::arc_open(info$url),
  "ImageServer" = arcgislayers::arc_open(info$url),
)
