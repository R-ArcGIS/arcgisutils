mod linestring;
mod multilinestring;
mod multipoint;
mod multipolygon;
mod point;
mod polygon;

use extendr_api::prelude::*;

use super::attributes::df_to_attributes;
use crate::deserialize_sr;
use crate::sfg::*;
use crate::to::AsEsriGeometry;
use serde_esri::features::Feature;
use serde_esri::geometry::{EsriGeometry, EsriMultiPoint, EsriPoint, EsriPolygon, EsriPolyline};

pub fn sf_as_features_2d_(attrs: List, geoms: List, n: i32, sr: Robj) -> Vec<Feature<2>> {
    let n = n as usize;
    let sr = deserialize_sr(&sr);
    let attrs = df_to_attributes(attrs, n);

    let geoms = if geoms.inherits("sfc_POINT") {
        geoms
            .into_iter()
            .map(|(_, pi)| {
                let sfg = SfgPoint(Doubles::try_from(pi).unwrap());
                let pnt: EsriPoint = sfg.as_point(sr.clone()).unwrap();
                Some(EsriGeometry::Point::<2>(pnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_LINESTRING") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = RMatrix::try_from(lstr).unwrap();
                let sfg = SfgLineString(lstr_list);
                let lstr: EsriPolyline<2> = sfg.as_polyline(sr.clone()).unwrap();
                Some(EsriGeometry::Polyline::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_POLYGON") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgPolygon(lstr_list);
                let lstr: EsriPolygon<2> = sfg.as_polygon(sr.clone()).unwrap();
                Some(EsriGeometry::Polygon::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOINT") {
        geoms
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt).unwrap();
                let sfg = SfgMultiPoint(pnt_mat);
                let mpnt: EsriMultiPoint<2> = sfg.as_multipoint(sr.clone()).unwrap();
                Some(EsriGeometry::MultiPoint::<2>(mpnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTILINESTRING") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiLineString(lstr_list);
                let lstr: EsriPolyline<2> = sfg.as_polyline(sr.clone()).unwrap();
                Some(EsriGeometry::Polyline::<2>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOLYGON") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiPolygon(lstr_list);
                let lstr: EsriPolygon<2> = sfg.as_polygon(sr.clone()).unwrap();
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

    feats
}

#[extendr]
pub fn sf_as_features_2d_string(attrs: List, geoms: List, n: i32, sr: Robj) -> String {
    let feats = sf_as_features_2d_(attrs, geoms, n, sr);
    serde_json::to_string(&feats).unwrap()
}

#[extendr]
pub fn sf_as_features_2d_list(attrs: List, geoms: List, n: i32, sr: Robj) -> Robj {
    let feats = sf_as_features_2d_(attrs, geoms, n, sr);
    extendr_api::serializer::to_robj(&feats).unwrap()
}

pub fn sf_as_features_3d_(attrs: List, geoms: List, n: i32, sr: Robj) -> Vec<Feature<3>> {
    let n = n as usize;
    let sr = deserialize_sr(&sr);
    let attrs = df_to_attributes(attrs, n);

    let geoms = if geoms.inherits("sfc_POINT") {
        geoms
            .into_iter()
            .map(|(_, pi)| {
                let sfg = SfgPoint(Doubles::try_from(pi).unwrap());
                let pnt: EsriPoint = sfg.as_point(sr.clone()).unwrap();
                Some(EsriGeometry::Point::<3>(pnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_LINESTRING") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = RMatrix::try_from(lstr).unwrap();
                let sfg = SfgLineString(lstr_list);
                let lstr: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
                Some(EsriGeometry::Polyline::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_POLYGON") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgPolygon(lstr_list);
                let lstr: EsriPolygon<3> = sfg.as_polygon(sr.clone()).unwrap();
                Some(EsriGeometry::Polygon::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOINT") {
        geoms
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt).unwrap();
                let sfg = SfgMultiPoint(pnt_mat);
                let mpnt: EsriMultiPoint<3> = sfg.as_multipoint(sr.clone()).unwrap();
                Some(EsriGeometry::MultiPoint::<3>(mpnt))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTILINESTRING") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiLineString(lstr_list);
                let lstr: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
                Some(EsriGeometry::Polyline::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else if geoms.inherits("sfc_MULTIPOLYGON") {
        geoms
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr).unwrap();
                let sfg = SfgMultiPolygon(lstr_list);
                let lstr: EsriPolygon<3> = sfg.as_polygon(sr.clone()).unwrap();
                Some(EsriGeometry::Polygon::<3>(lstr))
            })
            .collect::<Vec<_>>()
    } else {
        vec![None; n]
    };

    attrs
        .into_iter()
        .zip(geoms)
        .map(|(a, g)| Feature {
            geometry: g,
            attributes: Some(a),
        })
        .collect::<Vec<_>>()
}

#[extendr]
pub fn sf_as_features_3d_list(attrs: List, geoms: List, n: i32, sr: Robj) -> Robj {
    let feats = sf_as_features_3d_(attrs, geoms, n, sr);
    extendr_api::serializer::to_robj(&feats).unwrap()
}

#[extendr]
pub fn sf_as_features_3d_string(attrs: List, geoms: List, n: i32, sr: Robj) -> String {
    let feats = sf_as_features_3d_(attrs, geoms, n, sr);
    serde_json::to_string(&feats).unwrap()
}

extendr_module! {
    mod features;
    use linestring;
    use multilinestring;
    use multipoint;
    use multipolygon;
    use point;
    use polygon;
    fn sf_as_features_2d_list;
    fn sf_as_features_2d_string;
    fn sf_as_features_3d_list;
    fn sf_as_features_3d_string;
}
