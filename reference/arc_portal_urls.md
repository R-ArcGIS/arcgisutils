# Organization's URLs

Returns the URLs of an organizations services.

## Usage

``` r
arc_portal_urls(host = arc_host(), token = arc_token())
```

## Arguments

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

## Value

a data.frame

## Details

See [API
Reference](https://developers.arcgis.com/rest/users-groups-and-items/urls/)
for more information. **\[experimental\]**

## See also

Other portal:
[`arc_portal_resources()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_resources.md),
[`arc_portal_users()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_users.md),
[`self`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)

## Examples

``` r
arc_portal_urls()
#> $`2DSketchStylesGroupQuery`
#> [1] "title:\"Esri Sketch Styles\" AND owner:esri_en"
#> 
#> $`2DStylesGroupQuery`
#> [1] "title:\"Esri 2D Styles\" AND owner:esri_en"
#> 
#> $`3DBasemapGalleryGroupQuery`
#> [1] "title:\"ArcGIS Online 3D Basemaps\" AND owner:esri_en"
#> 
#> $analysisLayersGroupQuery
#> [1] "title:\"Living Atlas Analysis Layers\" AND owner:esri"
#> 
#> $basemapGalleryGroupQuery
#> [1] "title:\"United States Basemaps\" AND owner:Esri_cy_US"
#> 
#> $cdnUrl
#> [1] "https://cdn.arcgis.com"
#> 
#> $colorSetsGroupQuery
#> [1] "title:\"Esri Colors\" AND owner:esri_en"
#> 
#> $contentCategorySetsGroupQuery
#> [1] "title:\"ArcGIS Online Content Category Sets\" AND owner:esri_en"
#> 
#> $customBaseUrl
#> [1] "maps.arcgis.com"
#> 
#> $default3DBasemapQuery
#> [1] "typekeywords:\"sourceId#topographic\" AND type:\"Web Scene\" AND owner:esri_en"
#> 
#> $defaultBasemap
#> $defaultBasemap$baseMapLayers
#>                                                                               url
#> 1 https://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer
#>                    layerType
#> 1 ArcGISTiledMapServiceLayer
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  resourceInfo
#> 1 10.3, Layers, FALSE, 0, Citations, -1, FALSE, NA, 0, 0, 102100, 3857, TRUE, 256, 256, 96, JPEG, 90, -20037508.342787, 20037508.342787, 102100, 3857, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 156543.033928, 78271.5169639999, 39135.7584820001, 19567.8792409999, 9783.93962049996, 4891.96981024998, 2445.98490512499, 1222.99245256249, 611.49622628138, 305.748113140558, 152.874056570411, 76.4370282850732, 38.2185141425366, 19.1092570712683, 9.55462853563415, 4.77731426794937, 2.38865713397468, 1.19432856685505, 0.597164283559817, 0.298582141647617, 0.149291070823808, 0.0746455354119042, 0.0373227677059521, 0.018661383852976, 591657527.591555, 295828763.795777, 147914381.897889, 73957190.948944, 36978595.474472, 18489297.737236, 9244648.868618, 4622324.434309, 2311162.217155, 1155581.108577, 577790.554289, 288895.277144, 144447.638572, 72223.819286, 36111.909643, 18055.954822, 9027.977411, 4513.988705, 2256.994353, 1128.497176, 564.248588, 282.124294, 141.062147, 70.5310735, -28848255.0494791, -2077452.08212287, 28848255.0494791, 16430757.3767901, 102100, 3857, -20037507.0671618, -19971868.8804086, 20037507.0671618, 19971868.8804086, 102100, 3857, 591657527.591555, 70.5310735, esriMeters, PNG32,PNG24,PNG,JPG,DIB,TIFF,EMF,PS,PDF,GIF,SVG,SVGZ,BMP, Map,Tilemap,Query,Data, JSON, AMF, FALSE, 100, 4096, 4096, KmlServer
#> 
#> $defaultBasemap$title
#> [1] "Topographic"
#> 
#> 
#> $defaultDevBasemap
#> $defaultDevBasemap$baseMapLayers
#>                     id                  layerType
#> 1 World_Hillshade_2559 ArcGISTiledMapServiceLayer
#> 2      VectorTile_9568            VectorTileLayer
#>                                                                                         url
#> 1 https://ibasemaps-api.arcgis.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer
#> 2                                                                                      <NA>
#>   visibility opacity                 title            type
#> 1       TRUE       1       World Hillshade            <NA>
#> 2       TRUE       1 World Topographic Map VectorTileLayer
#>                                                                                                        styleUrl
#> 1                                                                                                          <NA>
#> 2 https://cdn.arcgis.com/sharing/rest/content/items/42df0d22517e49ad84edcee4c093857d/resources/styles/root.json
#> 
#> $defaultDevBasemap$title
#> [1] "Topographic"
#> 
#> 
#> $defaultExtent
#> $defaultExtent$xmin
#> [1] -15000000
#> 
#> $defaultExtent$ymin
#> [1] 2700000
#> 
#> $defaultExtent$xmax
#> [1] -6200000
#> 
#> $defaultExtent$ymax
#> [1] 6500000
#> 
#> $defaultExtent$spatialReference
#> $defaultExtent$spatialReference$wkid
#> [1] 102100
#> 
#> 
#> 
#> $defaultVectorBasemap
#> $defaultVectorBasemap$baseMapLayers
#>                     id                  layerType
#> 1 World_Hillshade_3805 ArcGISTiledMapServiceLayer
#> 2  195012aeb31-layer-2            VectorTileLayer
#>                                                                                          url
#> 1 https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer
#> 2                                                                                       <NA>
#>   visibility opacity                 title            type
#> 1       TRUE       1       World Hillshade            <NA>
#> 2       TRUE       1 World Topographic Map VectorTileLayer
#>                                                                                                        styleUrl
#> 1                                                                                                          <NA>
#> 2 https://cdn.arcgis.com/sharing/rest/content/items/27e89eb03c1e4341a1d75e597f0291e6/resources/styles/root.json
#> 
#> $defaultVectorBasemap$title
#> [1] "Topographic"
#> 
#> 
#> $description
#> NULL
#> 
#> $dev3DBasemapGalleryGroupQuery
#> [1] "title:\"World 3D Basemaps for Developers\" AND owner:esri_en"
#> 
#> $devBasemapGalleryGroupQuery
#> [1] "title:\"World Basemaps for Developers\" AND owner:esri"
#> 
#> $featuredGroups
#>            owner                     title
#> 1        esri_en Web Application Templates
#> 2           esri      Esri vector basemaps
#> 3 esri_livefeeds                Live Feeds
#> 4   esri_imagery           Wayback imagery
#> 
#> $featuredItemsGroupQuery
#> [1] "title:\"Featured Maps and Apps for United States\" AND owner:Esri_cy_US"
#> 
#> $fontManifestUrl
#> [1] "https://static.arcgis.com/fonts/fontManifestUrl.json"
#> 
#> $g3DTilesGalleryGroupQuery
#> [1] "title:\"ArcGIS Online Google 3D Basemaps\" AND owner:esri_en"
#> 
#> $galleryTemplatesGroupQuery
#> [1] "title:\"Gallery Templates\" AND owner:esri_en"
#> 
#> $helpBase
#> [1] "https://doc.arcgis.com/en/arcgis-online/"
#> 
#> $helperServices
#> $helperServices$geocode
#>                                                                   url northLat
#> 1 https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer     Ymax
#>   southLat eastLon westLon                           name batch placefinding
#> 1     Ymin    Xmax    Xmin ArcGIS World Geocoding Service  TRUE         TRUE
#>   suggest
#> 1    TRUE
#> 
#> $helperServices$defaultElevationLayers
#>                                                                                          url
#> 1 https://elevation3d.arcgis.com/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer
#>                id                        layerType      units
#> 1 globalElevation ArcGISTiledElevationServiceLayer esriMeters
#> 
#> $helperServices$route
#> $helperServices$route$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/Route/NAServer/Route_World"
#> 
#> $helperServices$route$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$geometry
#> $helperServices$geometry$url
#> [1] "https://utility.arcgisonline.com/arcgis/rest/services/Geometry/GeometryServer"
#> 
#> 
#> $helperServices$printTask
#> $helperServices$printTask$url
#> [1] "https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task"
#> 
#> 
#> $helperServices$asyncPrintTask
#> $helperServices$asyncPrintTask$url
#> [1] "https://print.arcgis.com/arcgis/rest/services/PrintingToolsAsync/GPServer/Export%20Web%20Map%20Task"
#> 
#> 
#> $helperServices$layoutInfoTask
#> $helperServices$layoutInfoTask$url
#> [1] "https://print.arcgis.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Get%20Layout%20Templates%20Info%20Task"
#> 
#> 
#> $helperServices$closestFacility
#> $helperServices$closestFacility$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/ClosestFacility/NAServer/ClosestFacility_World"
#> 
#> $helperServices$closestFacility$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$asyncClosestFacility
#> $helperServices$asyncClosestFacility$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/ClosestFacility/GPServer/FindClosestFacilities"
#> 
#> $helperServices$asyncClosestFacility$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$traffic
#> $helperServices$traffic$url
#> [1] "https://traffic.arcgis.com/arcgis/rest/services/World/Traffic/MapServer"
#> 
#> 
#> $helperServices$trafficData
#> $helperServices$trafficData$url
#> [1] "https://traffic.arcgis.com/arcgis/rest/services/World/TrafficFeeds/GPServer"
#> 
#> 
#> $helperServices$serviceArea
#> $helperServices$serviceArea$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/ServiceAreas/NAServer/ServiceArea_World"
#> 
#> $helperServices$serviceArea$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$asyncServiceArea
#> $helperServices$asyncServiceArea$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/ServiceAreas/GPServer/GenerateServiceAreas"
#> 
#> $helperServices$asyncServiceArea$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$syncVRP
#> $helperServices$syncVRP$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/VehicleRoutingProblemSync/GPServer/EditVehicleRoutingProblem"
#> 
#> $helperServices$syncVRP$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$asyncVRP
#> $helperServices$asyncVRP$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/VehicleRoutingProblem/GPServer/SolveVehicleRoutingProblem"
#> 
#> $helperServices$asyncVRP$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$asyncLocationAllocation
#> $helperServices$asyncLocationAllocation$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/LocationAllocation/GPServer"
#> 
#> $helperServices$asyncLocationAllocation$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$elevation
#> $helperServices$elevation$url
#> [1] "https://elevation.arcgis.com/arcgis/rest/services/Tools/Elevation/GPServer"
#> 
#> 
#> $helperServices$hydrology
#> $helperServices$hydrology$url
#> [1] "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer"
#> 
#> 
#> $helperServices$elevationSync
#> $helperServices$elevationSync$url
#> [1] "https://elevation.arcgis.com/arcgis/rest/services/Tools/ElevationSync/GPServer"
#> 
#> 
#> $helperServices$asyncRoute
#> $helperServices$asyncRoute$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/Route/GPServer"
#> 
#> $helperServices$asyncRoute$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$routingUtilities
#> $helperServices$routingUtilities$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/Utilities/GPServer"
#> 
#> 
#> $helperServices$asyncODCostMatrix
#> $helperServices$asyncODCostMatrix$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/OriginDestinationCostMatrix/GPServer"
#> 
#> $helperServices$asyncODCostMatrix$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$odCostMatrix
#> $helperServices$odCostMatrix$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/OriginDestinationCostMatrix/NAServer/OriginDestinationCostMatrix_World"
#> 
#> $helperServices$odCostMatrix$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$asyncFleetRouting
#> $helperServices$asyncFleetRouting$url
#> [1] "https://logistics.arcgis.com/arcgis/rest/services/World/VehicleRoutingProblem/GPServer"
#> 
#> $helperServices$asyncFleetRouting$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$packaging
#> $helperServices$packaging$url
#> [1] "https://packaging.arcgis.com/arcgis/rest/services/OfflinePackaging/GPServer"
#> 
#> $helperServices$packaging$maxMapAreaItemsLimit
#> [1] 16
#> 
#> $helperServices$packaging$exportTilesMap
#>                              source                           export
#> 1 https://services.arcgisonline.com https://tiledbasemaps.arcgis.com
#> 2   https://basemaps-api.arcgis.com      https://basemaps.arcgis.com
#> 3  https://ibasemaps-api.arcgis.com https://tiledbasemaps.arcgis.com
#> 
#> 
#> $helperServices$symbols
#> $helperServices$symbols$url
#> [1] "https://utility.arcgisonline.com/arcgis/rest/services/Utilities/Symbols/SymbolServer"
#> 
#> 
#> $helperServices$orthomappingElevation
#> $helperServices$orthomappingElevation$url
#> [1] "https://elevation3d.arcgis.com/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer"
#> 
#> 
#> $helperServices$workflowManager
#> $helperServices$workflowManager$url
#> [1] "https://workflow.arcgis.com"
#> 
#> 
#> $helperServices$snapToRoads
#> $helperServices$snapToRoads$url
#> [1] "https://route.arcgis.com/arcgis/rest/services/World/SnapToRoadsSync/GPServer"
#> 
#> $helperServices$snapToRoads$defaultTravelMode
#> [1] "FEgifRtFndKNcJMJ"
#> 
#> 
#> $helperServices$dashboardsUtility
#> $helperServices$dashboardsUtility$url
#> [1] "https://dashboards-api.arcgis.com"
#> 
#> 
#> $helperServices$aiAssistantServices
#> $helperServices$aiAssistantServices$url
#> [1] "https://aiservices-beta.arcgis.com"
#> 
#> $helperServices$aiAssistantServices$docChatAssistant
#> [1] "/skills/doc_chat"
#> 
#> $helperServices$aiAssistantServices$sqlGenerationAssistant
#> [1] "/skills/sqlGeneration"
#> 
#> $helperServices$aiAssistantServices$graphQueryAssistant
#> [1] "/skills/graphQueryGeneration"
#> 
#> $helperServices$aiAssistantServices$arcadeGenerationAssistant
#> [1] "/skills/arcade_generation"
#> 
#> $helperServices$aiAssistantServices$discoveryAssistant
#> [1] "/skills/discovery"
#> 
#> $helperServices$aiAssistantServices$itemInformationAssistant
#> [1] "/skills/item_information_suggester"
#> 
#> $helperServices$aiAssistantServices$pythonAssistant
#> [1] "/skills/python_code_suggester/chat"
#> 
#> 
#> $helperServices$aiUtilityServices
#> $helperServices$aiUtilityServices$url
#> [1] "https://aiutils.arcgis.com"
#> 
#> $helperServices$aiUtilityServices$api
#> $helperServices$aiUtilityServices$api$text_translate
#> [1] "/api/text/translate"
#> 
#> $helperServices$aiUtilityServices$api$text_analyze
#> [1] "/api/text/analyze"
#> 
#> $helperServices$aiUtilityServices$api$image_analyze
#> [1] "/api/image/analyze"
#> 
#> $helperServices$aiUtilityServices$api$image_extract
#> [1] "/api/image/extractText"
#> 
#> $helperServices$aiUtilityServices$api$embeddings
#> [1] "/api/embeddings"
#> 
#> $helperServices$aiUtilityServices$api$audio_transcribe
#> [1] "/api/audio/transcribe"
#> 
#> 
#> $helperServices$aiUtilityServices$translateUtility
#> [1] "/api/translate"
#> 
#> $helperServices$aiUtilityServices$imageExtractTextUtility
#> [1] "/api/assistant/imageExtractText"
#> 
#> 
#> $helperServices$aiModels
#> $helperServices$aiModels$url
#> [1] "https://aimodels.arcgis.com"
#> 
#> 
#> $helperServices$geoenrichment
#> $helperServices$geoenrichment$url
#> [1] "https://geoenrich.arcgis.com/arcgis/rest/services/World/GeoenrichmentServer"
#> 
#> 
#> 
#> $homePageFeaturedContent
#> [1] "title:\"Featured Maps\" AND owner:Esri_cy_US"
#> 
#> $homePageFeaturedContentCount
#> [1] 12
#> 
#> $isPortal
#> [1] FALSE
#> 
#> $layerTemplatesGroupQuery
#> [1] "title:\"Esri Layer Templates\" AND owner:esri_en"
#> 
#> $livingAtlasGroupQuery
#> [1] "title:\"LAW Search\" AND owner:Esri_LivingAtlas"
#> 
#> $name
#> NULL
#> 
#> $portalHostname
#> [1] "www.arcgis.com"
#> 
#> $portalMode
#> [1] "multitenant"
#> 
#> $portalName
#> [1] "ArcGIS Online"
#> 
#> $portalThumbnail
#> NULL
#> 
#> $rasterFunctionTemplatesGroupQuery
#> [1] "title:\"Raster Function Templates\" AND owner:esri_en"
#> 
#> $rotatorPanels
#> NULL
#> 
#> $staticImagesUrl
#> [1] "https://static.arcgis.com/images"
#> 
#> $stylesGroupQuery
#> [1] "title:\"Esri Styles\" AND owner:esri_en"
#> 
#> $supports3DTilesServices
#> [1] TRUE
#> 
#> $supportsHostedServices
#> [1] TRUE
#> 
#> $symbolSetsGroupQuery
#> [1] "title:\"Esri Symbols\" AND owner:esri_en"
#> 
#> $templatesGroupQuery
#> [1] "title:\"Web Application Templates\" AND owner:esri_en"
#> 
#> $thumbnail
#> NULL
#> 
#> $use3dBasemaps
#> [1] TRUE
#> 
#> $useDefault3dBasemap
#> [1] TRUE
#> 
#> $useVectorBasemaps
#> [1] TRUE
#> 
#> $vectorBasemapGalleryGroupQuery
#> [1] "title:\"United States Vector Basemaps\" AND owner:Esri_cy_US"
#> 
#> $httpPort
#> [1] 80
#> 
#> $httpsPort
#> [1] 443
#> 
#> $ipCntryCode
#> [1] "US"
#> 
#> $supportsOAuth
#> [1] TRUE
#> 
#> $isReadOnly
#> [1] FALSE
#> 
#> $currentVersion
#> [1] "2026.1"
#> 
```
