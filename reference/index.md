# Package index

## Request Utilities

- [`arc_host()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_host.md)
  : Determines Portal Host
- [`arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  [`set_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  [`unset_arc_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  [`obj_check_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  [`check_token_has_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/token.md)
  : Manage authorization tokens
- [`arc_agent()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_agent.md)
  : Set user-agent for arcgisutils
- [`arc_base_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_base_req.md)
  : Generate base request
- [`arc_paginate_req()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_paginate_req.md)
  : Paginate ArcGIS Requests
- [`detect_errors()`](https://github.com/R-ArcGIS/arcgisutils/reference/detect_errors.md)
  [`catch_error()`](https://github.com/R-ArcGIS/arcgisutils/reference/detect_errors.md)
  : Detect errors in parsed json response

## Authorization

- [`auth_code()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`auth_client()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`auth_binding()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`auth_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`auth_key()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`refresh_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  [`validate_or_refresh_token()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth.md)
  : Authorization
- [`oauth_provider_arcgis()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md)
  [`auth_shiny()`](https://github.com/R-ArcGIS/arcgisutils/reference/auth_shiny.md)
  : Authenticate with Shiny

## Portal

- [`search_items()`](https://github.com/R-ArcGIS/arcgisutils/reference/search_items.md)
  : Search for Portal Items
- [`arc_item()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item.md)
  : Portal Item Metadata
- [`arc_user()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user.md)
  : User Information
- [`arc_group()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_group.md)
  : Fetch Group Information
- [`arc_group_users()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_group_users.md)
  : List users in a group
- [`arc_item_data()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_item_data.md)
  : Download an Item's Data
- [`arc_portal_urls()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_urls.md)
  : Organization's URLs
- [`arc_portal_users()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_users.md)
  : Portal Users
- [`arc_user_self()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_user_self.md)
  : Discover Authenticated User Metadata
- [`arc_self_meta()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)
  [`arc_portal_self()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md)
  : Access the Portal Self Resource
- [`arc_portal_resources()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_resources.md)
  : Portal File Resources
- [`arc_portal_servers()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_portal_servers.md)
  : List ArcGIS Enterprise Servers
- [`item_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/portal_types.md)
  [`item_keyword()`](https://github.com/R-ArcGIS/arcgisutils/reference/portal_types.md)
  [`portal_item_keywords()`](https://github.com/R-ArcGIS/arcgisutils/reference/portal_types.md)
  [`portal_item_types()`](https://github.com/R-ArcGIS/arcgisutils/reference/portal_types.md)
  : Portal Item Types
- [`arc_group_content()`](https://github.com/R-ArcGIS/arcgisutils/reference/content.md)
  [`arc_user_content()`](https://github.com/R-ArcGIS/arcgisutils/reference/content.md)
  : Portal Content Items

## Geometry

- [`determine_dims()`](https://github.com/R-ArcGIS/arcgisutils/reference/determine_dims.md)
  [`has_m()`](https://github.com/R-ArcGIS/arcgisutils/reference/determine_dims.md)
  [`has_z()`](https://github.com/R-ArcGIS/arcgisutils/reference/determine_dims.md)
  : Determine the dimensions of a geometry object
- [`determine_esri_geo_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/determine_esri_geo_type.md)
  : Determine Esri Geometry type
- [`validate_crs()`](https://github.com/R-ArcGIS/arcgisutils/reference/validate_crs.md)
  : Validate CRS object

## JSON

- [`as_esri_geometry()`](https://github.com/R-ArcGIS/arcgisutils/reference/as_esri_geometry.md)
  : Create Esri JSON Geometry Objects
- [`as_features()`](https://github.com/R-ArcGIS/arcgisutils/reference/features.md)
  [`as_esri_features()`](https://github.com/R-ArcGIS/arcgisutils/reference/features.md)
  : Create Esri Features
- [`as_featureset()`](https://github.com/R-ArcGIS/arcgisutils/reference/featureset.md)
  [`as_esri_featureset()`](https://github.com/R-ArcGIS/arcgisutils/reference/featureset.md)
  : Create Esri FeatureSet Objects
- [`as_layer()`](https://github.com/R-ArcGIS/arcgisutils/reference/layer_json.md)
  [`as_layer_definition()`](https://github.com/R-ArcGIS/arcgisutils/reference/layer_json.md)
  [`as_feature_collection()`](https://github.com/R-ArcGIS/arcgisutils/reference/layer_json.md)
  : Create Esri layer objects
- [`as_extent()`](https://github.com/R-ArcGIS/arcgisutils/reference/as_extent.md)
  : Convert an object to an extent
- [`parse_esri_json()`](https://github.com/R-ArcGIS/arcgisutils/reference/parse_esri_json.md)
  : Parse Esri JSON

## Geoprocessing Services

- [`arc_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md)
  [`as_form_params()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_form_params.md)
  : Form request parameters
- [`arc_job_status()`](https://github.com/R-ArcGIS/arcgisutils/reference/arc_job_status.md)
  : Geoprocessing Job Status
- [`new_gp_job()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job.md)
  : Create a Geoprocessing Service Job
- [`gp_job_from_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_job_from_url.md)
  : Create GP Job from existing URL
- [`parse_gp_feature_record_set()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_gp_feature_record_set()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`parse_gp_record_set()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_record_set()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_gp_raster_layer()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`parse_gp_linear_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`gp_areal_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_gp_areal_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`parse_gp_areal_unit()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_gp_date()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`parse_gp_date()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`as_spatial_reference()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`from_spatial_reference()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`parse_spatial_reference()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  [`from_envelope()`](https://github.com/R-ArcGIS/arcgisutils/reference/gp_params.md)
  : Geoprocessing Parameter Types

## Types

- [`as_fields()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  [`infer_esri_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  [`fields_as_ptype_df()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  [`ptype_tbl()`](https://github.com/R-ArcGIS/arcgisutils/reference/field_mapping.md)
  : Esri Field Type Mapping
- [`is_date()`](https://github.com/R-ArcGIS/arcgisutils/reference/dates.md)
  [`date_to_ms()`](https://github.com/R-ArcGIS/arcgisutils/reference/dates.md)
  [`from_esri_date()`](https://github.com/R-ArcGIS/arcgisutils/reference/dates.md)
  : Date handling

## Utilities

- [`compact()`](https://github.com/R-ArcGIS/arcgisutils/reference/utilities.md)
  [`` `%||%` ``](https://github.com/R-ArcGIS/arcgisutils/reference/utilities.md)
  [`check_dots_named()`](https://github.com/R-ArcGIS/arcgisutils/reference/utilities.md)
  [`data_frame()`](https://github.com/R-ArcGIS/arcgisutils/reference/utilities.md)
  : General utility functions
- [`rbind_results()`](https://github.com/R-ArcGIS/arcgisutils/reference/rbind_results.md)
  : Combine multiple data.frames
- [`arc_url_parse()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md)
  [`arc_url_type()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md)
  [`is_url()`](https://github.com/R-ArcGIS/arcgisutils/reference/url.md)
  : Parse an ArcGIS service or content URL into its components

## Requests

- [`fetch_layer_metadata()`](https://github.com/R-ArcGIS/arcgisutils/reference/fetch_layer_metadata.md)
  : Retrieve metadata
