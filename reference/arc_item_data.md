# Download an Item's Data

Download the data backing a portal item. This function always returns a
raw vector as the type of the data that is downloaded cannot always be
known.

## Usage

``` r
arc_item_data(item, host = arc_host(), token = arc_token())
```

## Arguments

- item:

  the item ID or the result of
  [`arc_item()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item.md).

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  or similar

## Value

a raw vector containing the bytes of the data associated with the item.
If the response is `application/json` then the json string is returned
without parsing.

## Details

**\[experimental\]**

## See also

Other portal item:
[`arc_item()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item.md)

## Examples

``` r
arc_item_data("9df5e769bfe8412b8de36a2e618c7672")
#> [1] "{\"layers\":[{\"layerDefinition\":{\"defaultVisibility\":true,\"drawingInfo\":{\"renderer\":{\"type\":\"uniqueValue\",\"visualVariables\":[{\"type\":\"sizeInfo\",\"valueExpression\":\"$view.scale\",\"stops\":[{\"size\":7.50656120111518,\"value\":144447.638572},{\"size\":6.005248960892144,\"value\":1155581.1085775},{\"size\":3.002624480446072,\"value\":9244648.868618},{\"size\":1.501312240223036,\"value\":7.39571909489445E7}]}],\"field1\":\"POP_CLASS\",\"uniqueValueGroups\":[{\"classes\":[{\"label\":\"10\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[203,6,13,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.25,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"10\"]]},{\"label\":\"9\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[230,91,41,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.25,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"9\"]]},{\"label\":\"8\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[245,141,5,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.863,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"8\"]]},{\"label\":\"7\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[242,178,48,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.31,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"7\"]]},{\"label\":\"6\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[224,188,87,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.86,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"6\"]]},{\"label\":\"5\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[239,236,164,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.86,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[120,119,119,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"values\":[[\"5\"]]}]}],\"uniqueValueInfos\":[{\"label\":\"10\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[203,6,13,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.25,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"10\"},{\"label\":\"9\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[230,91,41,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.25,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"9\"},{\"label\":\"8\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[245,141,5,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.863,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"8\"},{\"label\":\"7\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[242,178,48,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":5.31,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"7\"},{\"label\":\"6\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[224,188,87,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.86,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[153,153,153,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"6\"},{\"label\":\"5\",\"symbol\":{\"type\":\"esriSMS\",\"color\":[239,236,164,255],\"angle\":0,\"xoffset\":0,\"yoffset\":0,\"size\":6.86,\"style\":\"esriSMSCircle\",\"outline\":{\"type\":\"esriSLS\",\"color\":[120,119,119,64],\"width\":0.75,\"style\":\"esriSLSSolid\"}},\"value\":\"5\"}]}}},\"id\":0,\"popupInfo\":{\"popupElements\":[{\"type\":\"fields\"}],\"fieldInfos\":[{\"fieldName\":\"OBJECTID\",\"format\":{\"digitSeparator\":false,\"places\":0},\"isEditable\":false,\"label\":\"OBJECTID\",\"visible\":false},{\"fieldName\":\"NAME\",\"isEditable\":true,\"label\":\"Name\",\"visible\":true},{\"fieldName\":\"CLASS\",\"isEditable\":true,\"label\":\"Class\",\"visible\":true},{\"fieldName\":\"STATE_ABBR\",\"isEditable\":true,\"label\":\"State Abbreviation\",\"visible\":true},{\"fieldName\":\"STATE_FIPS\",\"isEditable\":true,\"label\":\"State FIPS\",\"visible\":true},{\"fieldName\":\"PLACE_FIPS\",\"isEditable\":true,\"label\":\"Place FIPS\",\"visible\":true},{\"fieldName\":\"POPULATION\",\"format\":{\"digitSeparator\":true,\"places\":0},\"isEditable\":true,\"label\":\"2020 Total Population\",\"visible\":true},{\"fieldName\":\"POP_CLASS\",\"format\":{\"digitSeparator\":false,\"places\":0},\"isEditable\":true,\"label\":\"Population Class\",\"visible\":true},{\"fieldName\":\"POP_SQMI\",\"format\":{\"digitSeparator\":true,\"places\":1},\"isEditable\":true,\"label\":\"People per square mile\",\"visible\":true},{\"fieldName\":\"SQMI\",\"format\":{\"digitSeparator\":true,\"places\":2},\"isEditable\":true,\"label\":\"Area in square miles\",\"visible\":true},{\"fieldName\":\"CAPITAL\",\"isEditable\":true,\"label\":\"Capital\",\"visible\":true}],\"title\":\"{NAME}, {STATE_ABBR}\"}}],\"tables\":[]}"
```
