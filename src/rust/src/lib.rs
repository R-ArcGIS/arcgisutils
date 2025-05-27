use extendr_api::prelude::*;
// mod sf_compat;
mod sfc;
mod sfg;
mod to;
use crate::sfg::{Dim, SfgDim};
use serde_esri::{
    geometry::{EsriMultiPoint, EsriPolygon, EsriPolyline},
    spatial_reference::SpatialReference,
};
use sfg::{
    SfgLineString, SfgMultiLineString, SfgMultiPoint, SfgMultiPolygon, SfgPoint, SfgPolygon,
};
use to::AsEsriGeometry;

pub fn deserialize_sr(sr: &Robj) -> Option<SpatialReference> {
    extendr_api::deserializer::from_robj::<SpatialReference>(sr).ok()
}

#[extendr]
fn sfg_point_as_point(x: Doubles, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let sfg = SfgPoint(x);
    serde_json::to_string(&sfg.as_point(sr).unwrap()).unwrap()
}

#[extendr]
fn sfg_multipoint_as_multipoint(x: RMatrix<f64>, sr: Robj) -> String {
    let sfg = SfgMultiPoint(x);
    let dim = sfg.sfg_dim().unwrap();
    let sr = deserialize_sr(&sr);

    match dim {
        SfgDim::XY => {
            let mpnt: EsriMultiPoint<2> = sfg.as_multipoint(sr.clone()).unwrap();
            serde_json::to_string(&mpnt).unwrap()
        }
        SfgDim::XYZ => {
            let mpnt: EsriMultiPoint<3> = sfg.as_multipoint(sr.clone()).unwrap();
            serde_json::to_string(&mpnt).unwrap()
        }
        SfgDim::XYM => {
            let mpnt: EsriMultiPoint<3> = sfg.as_multipoint(sr.clone()).unwrap();
            serde_json::to_string(&mpnt).unwrap()
        }
        SfgDim::XYZM => {
            let mpnt: EsriMultiPoint<4> = sfg.as_multipoint(sr.clone()).unwrap();
            serde_json::to_string(&mpnt).unwrap()
        }
    }
}

#[extendr]
fn sfg_linestring_as_polyline(x: RMatrix<f64>, sr: Robj) -> String {
    let sfg = SfgLineString(x);
    let dim = sfg.sfg_dim().unwrap();
    let sr = deserialize_sr(&sr);

    match dim {
        SfgDim::XY => {
            let pline: Option<EsriPolyline<2>> = sfg.as_polyline(sr.clone());
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYZ => {
            let pline: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYM => {
            let pline: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYZM => {
            let pline: EsriPolyline<4> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
    }
}

#[extendr]
fn sfg_multilinestring_as_polyline(x: List, sr: Robj) -> String {
    let sfg = SfgMultiLineString(x);
    let dim = sfg.sfg_dim().unwrap();
    let sr = deserialize_sr(&sr);

    match dim {
        SfgDim::XY => {
            let pline: Option<EsriPolyline<2>> = sfg.as_polyline(sr.clone());
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYZ => {
            let pline: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYM => {
            let pline: EsriPolyline<3> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
        SfgDim::XYZM => {
            let pline: EsriPolyline<4> = sfg.as_polyline(sr.clone()).unwrap();
            serde_json::to_string(&pline).unwrap()
        }
    }
}

#[extendr]
fn sfg_polygon_as_polygon(x: List, sr: Robj) -> String {
    let sfg = SfgPolygon(x);
    let dim = sfg.sfg_dim().unwrap();
    let sr = deserialize_sr(&sr);

    match dim {
        SfgDim::XY => {
            let poly: Option<EsriPolygon<2>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYZ => {
            let poly: Option<EsriPolygon<3>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYM => {
            let poly: Option<EsriPolygon<3>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYZM => {
            let poly: Option<EsriPolygon<4>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
    }
}

#[extendr]
fn sfg_multipolygon_as_polygon(x: List, sr: Robj) -> String {
    let sfg = SfgMultiPolygon(x);
    let dim = sfg.sfg_dim().unwrap();
    let sr = deserialize_sr(&sr);

    match dim {
        SfgDim::XY => {
            let poly: Option<EsriPolygon<2>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYZ => {
            let poly: Option<EsriPolygon<3>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYM => {
            let poly: Option<EsriPolygon<3>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
        SfgDim::XYZM => {
            let poly: Option<EsriPolygon<4>> = sfg.as_polygon(sr.clone());
            serde_json::to_string(&poly).unwrap()
        }
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod arcgisutils;
    use to;
    fn sfg_point_as_point;
    fn sfg_multipoint_as_multipoint;
    fn sfg_linestring_as_polyline;
    fn sfg_multilinestring_as_polyline;
    fn sfg_polygon_as_polygon;
    fn sfg_multipolygon_as_polygon;
    // fn parse_esri_json_str;
    // fn parse_esri_json_str_simd;
    // fn parse_esri_json_raw_simd;
    // fn parse_esri_json_raw;
    // fn parse_esri_json_raw_geoarrow;
}
