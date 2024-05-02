use crate::{deserialize_sr, sfc::*, sfg::SfgMultiLineString, to::AsEsriGeometry};
use extendr_api::prelude::*;
use serde_esri::{
    features::Feature, features::FeatureSet, geometry::EsriGeometry, geometry::EsriPolyline,
    spatial_reference::SpatialReference,
};
use serde_json::Map;

impl SfcMultiLineString {
    pub fn as_features_2d(self, sr: Option<SpatialReference>) -> Result<Vec<Feature<2>>> {
        let lstrs = self
            .0
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr);
                let lstr_list = match lstr_list {
                    Ok(lstr_list) => {
                        let sfg = SfgMultiLineString(lstr_list);
                        let lstr: Option<EsriPolyline<2>> = sfg.as_polyline(sr.clone());
                        lstr.unwrap()
                    }
                    Err(_) => EsriPolyline {
                        hasZ: Some(false),
                        hasM: Some(false),
                        paths: vec![],
                        spatialReference: None,
                    },
                };

                Feature::<2> {
                    geometry: Some(EsriGeometry::Polyline(lstr_list)),
                    attributes: Some(Map::default()),
                }
            })
            .collect::<Vec<_>>();

        Ok(lstrs)
    }

    pub fn as_features_3d(self, sr: Option<SpatialReference>) -> Result<Vec<Feature<3>>> {
        let lstrs = self
            .0
            .into_iter()
            .map(|(_, lstr)| {
                let lstr_list = List::try_from(lstr);
                let lstr_list = match lstr_list {
                    Ok(lstr_list) => {
                        let sfg = SfgMultiLineString(lstr_list);
                        let lstr: Option<EsriPolyline<3>> = sfg.as_polyline(sr.clone());
                        lstr.unwrap()
                    }
                    Err(_) => EsriPolyline {
                        hasZ: Some(false),
                        hasM: Some(false),
                        paths: vec![],
                        spatialReference: sr.clone(),
                    },
                };

                Feature::<3> {
                    geometry: Some(EsriGeometry::Polyline(lstr_list)),
                    attributes: Some(Map::default()),
                }
            })
            .collect::<Vec<_>>();

        Ok(lstrs)
    }

    pub fn as_featureset_2d(self, sr: Option<SpatialReference>) -> FeatureSet<2> {
        let feats = self.as_features_2d(None).expect("Features to be created");
        FeatureSet {
            objectIdFieldName: None,
            globalIdFieldName: None,
            displayFieldName: None,
            spatialReference: sr,
            geometryType: Some("esriGeometryPolyline".into()),
            features: feats,
            fields: None,
            hasM: None,
            hasZ: None,
        }
    }

    pub fn as_featureset_3d(self, sr: Option<SpatialReference>) -> FeatureSet<3> {
        let feats = self.as_features_3d(None).expect("Features to be created");
        FeatureSet {
            objectIdFieldName: None,
            globalIdFieldName: None,
            displayFieldName: None,
            spatialReference: sr,
            geometryType: Some("esriGeometryPolyline".into()),
            features: feats,
            fields: None,
            // TODO parameterize this??
            // how can we propagate the hasZ and M forward/
            hasM: None,
            hasZ: Some(true),
        }
    }
}

#[extendr]
fn sfc_multilinestring_features_2d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiLineString(x).as_features_2d(sr).unwrap();
    serde_json::to_string(&res).unwrap()
}
#[extendr]
fn sfc_multilinestring_features_2d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiLineString(x).as_features_2d(sr).unwrap();
    extendr_api::serializer::to_robj(&res).unwrap()
}

#[extendr]
fn sfc_multilinestring_features_3d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiLineString(x).as_features_3d(sr).unwrap();
    serde_json::to_string(&res).unwrap()
}

#[extendr]
fn sfc_multilinestring_features_3d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiLineString(x).as_features_3d(sr).unwrap();
    extendr_api::serializer::to_robj(&res).unwrap()
}

#[extendr]
fn sfc_multilinestring_featureset_2d_string(x: List, sr: Robj) -> String {
    let sfc = SfcMultiLineString(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_2d(crs);
    serde_json::to_string(&featureset).unwrap()
}
#[extendr]
fn sfc_multilinestring_featureset_2d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcMultiLineString(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_2d(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

#[extendr]
fn sfc_multilinestring_featureset_3d_string(x: List, sr: Robj) -> String {
    let sfc = SfcMultiLineString(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_3d(crs);
    serde_json::to_string(&featureset).unwrap()
}
#[extendr]
fn sfc_multilinestring_featureset_3d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcMultiLineString(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_3d(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

extendr_module! {
    mod multilinestring;
    fn sfc_multilinestring_features_2d_string;
    fn sfc_multilinestring_features_2d_list;
    fn sfc_multilinestring_features_3d_string;
    fn sfc_multilinestring_features_3d_list;
    fn sfc_multilinestring_featureset_2d_string;
    fn sfc_multilinestring_featureset_2d_list;
    fn sfc_multilinestring_featureset_3d_string;
    fn sfc_multilinestring_featureset_3d_list;
}
