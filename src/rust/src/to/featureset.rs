use super::attributes::df_to_attributes;
use crate::deserialize_sr;
use crate::sfg::*;
use crate::to::AsEsriGeometry;
use extendr_api::prelude::*;
use serde_esri::features::Feature;
use serde_esri::features::FeatureSet;
use serde_esri::{
    geometry::{EsriGeometry, EsriMultiPoint, EsriPoint, EsriPolygon, EsriPolyline},
    spatial_reference::SpatialReference,
};

pub fn as_featureset_2d_(attrs: List, geoms: List, n: i32, sr: Robj) -> FeatureSet<2> {
    let n = n as usize;
    let sr = deserialize_sr(&sr).map_or(SpatialReference::default(), |f| f);
    let attrs = df_to_attributes(attrs, n);

    let mut geom_type: Option<String> = None;

    let geoms = if geoms.inherits("sfc_POINT") {
        geom_type = Some("esriGeometryPoint".into());
        geoms
            .into_iter()
            .map(|(_, pi)| {
                let sfg = SfgPoint(Doubles::try_from(pi).unwrap());
                let pnt: EsriPoint = sfg.as_point(None).unwrap();
                Some(EsriGeometry::Point::<2>(pnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_LINESTRING") {
        geom_type = Some("esriGeometryPolyline".into());

        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = RMatrix::try_from(lstr).unwrap();
                let sfg = SfgLineString(lstr_list);
                let lstr: EsriPolyline<2> = sfg.as_polyline(None).unwrap();
                Some(EsriGeometry::Polyline::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_POLYGON") {
        geom_type = Some("esriGeometryPolygon".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgPolygon(lstr_list);
                let lstr: EsriPolygon<2> = sfg.as_polygon(None).unwrap();
                Some(EsriGeometry::Polygon::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOINT") {
        geom_type = Some("esriGeometryMultipoint".into());
        geoms
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt).unwrap();
                let sfg = SfgMultiPoint(pnt_mat);
                let mpnt: EsriMultiPoint<2> = sfg.as_multipoint(None).unwrap();
                Some(EsriGeometry::MultiPoint::<2>(mpnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTILINESTRING") {
        geom_type = Some("esriGeometryPolyline".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiLineString(lstr_list);
                let lstr: EsriPolyline<2> = sfg.as_polyline(None).unwrap();
                Some(EsriGeometry::Polyline::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOLYGON") {
        geom_type = Some("esriGeometryPolygon".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiPolygon(lstr_list);
                let lstr: EsriPolygon<2> = sfg.as_polygon(None).unwrap();
                Some(EsriGeometry::Polygon::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else {
        vec![None; n]
    };

    let feats = attrs
        .into_iter()
        .zip(geoms)
        .map(|(a, g)| Feature {
            geometry: g,
            attributes: Some(a),
        })
        .collect::<Vec<_>>();

    // TODO allow the `None`s to be specified by argument
    FeatureSet {
        objectIdFieldName: None,
        globalIdFieldName: None,
        displayFieldName: None,
        spatialReference: Some(sr),
        geometryType: geom_type,
        features: feats,
        fields: None,
        hasM: None,
        hasZ: None,
    }
}

#[extendr]
pub fn as_featureset_2d_string(attrs: List, geoms: List, n: i32, sr: Robj) -> String {
    let fset = as_featureset_2d_(attrs, geoms, n, sr);
    serde_json::to_string(&fset).unwrap()
}

#[extendr]
pub fn as_featureset_2d_list(attrs: List, geoms: List, n: i32, sr: Robj) -> Robj {
    let fset = as_featureset_2d_(attrs, geoms, n, sr);
    extendr_api::serializer::to_robj(&fset).unwrap()
}

pub fn as_featureset_3d_(attrs: List, geoms: List, n: i32, sr: Robj, has_z: bool) -> FeatureSet<3> {
    let n = n as usize;
    let sr = deserialize_sr(&sr).map_or(SpatialReference::default(), |f| f);
    let attrs = df_to_attributes(attrs, n);

    // Determine if Z or M geometries are provided
    let z = if has_z { Some(true) } else { None };

    let m = if !has_z { Some(true) } else { None };
    let mut geom_type: Option<String> = None;

    let geoms = if geoms.inherits("sfc_POINT") {
        geom_type = Some("esriGeometryPoint".into());
        geoms
            .into_iter()
            .map(|(_, pi)| {
                let sfg = SfgPoint(Doubles::try_from(pi).unwrap());
                let pnt: EsriPoint = sfg.as_point(None).unwrap();
                Some(EsriGeometry::Point::<3>(pnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_LINESTRING") {
        geom_type = Some("esriGeometryPolyline".into());

        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = RMatrix::try_from(lstr).unwrap();
                let sfg = SfgLineString(lstr_list);
                let lstr: EsriPolyline<3> = sfg.as_polyline(None).unwrap();
                Some(EsriGeometry::Polyline::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_POLYGON") {
        geom_type = Some("esriGeometryPolygon".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgPolygon(lstr_list);
                let lstr: EsriPolygon<3> = sfg.as_polygon(None).unwrap();
                Some(EsriGeometry::Polygon::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOINT") {
        geom_type = Some("esriGeometryMultipoint".into());
        geoms
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt).unwrap();
                let sfg = SfgMultiPoint(pnt_mat);
                let mpnt: EsriMultiPoint<3> = sfg.as_multipoint(None).unwrap();
                Some(EsriGeometry::MultiPoint::<3>(mpnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTILINESTRING") {
        geom_type = Some("esriGeometryPolyline".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiLineString(lstr_list);
                let lstr: EsriPolyline<3> = sfg.as_polyline(None).unwrap();
                Some(EsriGeometry::Polyline::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOLYGON") {
        geom_type = Some("esriGeometryPolygon".into());
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiPolygon(lstr_list);
                let lstr: EsriPolygon<3> = sfg.as_polygon(None).unwrap();
                Some(EsriGeometry::Polygon::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else {
        vec![None; n]
    };

    let feats = attrs
        .into_iter()
        .zip(geoms)
        .map(|(a, g)| Feature {
            geometry: g,
            attributes: Some(a),
        })
        .collect::<Vec<_>>();

    // TODO allow the `None`s to be specified by argument
    FeatureSet {
        objectIdFieldName: None,
        globalIdFieldName: None,
        displayFieldName: None,
        spatialReference: Some(sr),
        geometryType: geom_type,
        features: feats,
        fields: None,
        // TODO parameterize this??
        // how can we propagate the hasZ and M forward/
        hasM: m,
        hasZ: z,
    }
}

#[extendr]
pub fn as_featureset_3d_string(attrs: List, geoms: List, n: i32, sr: Robj, has_z: bool) -> String {
    let fset = as_featureset_3d_(attrs, geoms, n, sr, has_z);
    serde_json::to_string(&fset).unwrap()
}

#[extendr]
pub fn as_featureset_3d_list(attrs: List, geoms: List, n: i32, sr: Robj, has_z: bool) -> Robj {
    let fset = as_featureset_3d_(attrs, geoms, n, sr, has_z);
    extendr_api::serializer::to_robj(&fset).unwrap()
}

extendr_module! {
    mod featureset;
    fn as_featureset_2d_list;
    fn as_featureset_2d_string;
    fn as_featureset_3d_list;
    fn as_featureset_3d_string;
}
