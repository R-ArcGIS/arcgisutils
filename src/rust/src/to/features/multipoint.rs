use crate::{deserialize_sr, sfc::*, sfg::SfgMultiPoint, to::AsEsriGeometry};
use extendr_api::prelude::*;
use serde_esri::{
    features::{Feature, FeatureSet},
    geometry::{EsriGeometry, EsriMultiPoint},
    spatial_reference::SpatialReference,
};

use serde_json::Map;

impl SfcMultiPoint {
    pub fn as_features_2d(self, sr: Option<SpatialReference>) -> Result<Vec<Feature<2>>> {
        let mpnts = self
            .0
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt);
                let pnt_mat = match pnt_mat {
                    Ok(pnt_mat) => {
                        let sfg = SfgMultiPoint(pnt_mat);
                        let mpnt: Option<EsriMultiPoint<2>> = sfg.as_multipoint(sr.clone());
                        mpnt.unwrap()
                    }
                    Err(_) => EsriMultiPoint {
                        hasZ: Some(false),
                        hasM: Some(false),
                        points: vec![],
                        spatialReference: None,
                    },
                };

                Feature::<2> {
                    geometry: Some(EsriGeometry::MultiPoint(pnt_mat)),
                    attributes: Some(Map::default()),
                }
            })
            .collect::<Vec<_>>();

        Ok(mpnts)
    }

    pub fn as_features_3d(self, sr: Option<SpatialReference>) -> Result<Vec<Feature<3>>> {
        let mpnts = self
            .0
            .into_iter()
            .map(|(_, pnt)| {
                let pnt_mat = RMatrix::try_from(pnt);
                let pnt_mat = match pnt_mat {
                    Ok(pnt_mat) => {
                        let sfg = SfgMultiPoint(pnt_mat);
                        let mpnt: Option<EsriMultiPoint<3>> = sfg.as_multipoint(sr.clone());
                        mpnt.unwrap()
                    }
                    Err(_) => EsriMultiPoint {
                        hasZ: Some(false),
                        hasM: Some(false),
                        points: vec![],
                        spatialReference: None,
                    },
                };

                Feature::<3> {
                    geometry: Some(EsriGeometry::MultiPoint(pnt_mat)),
                    attributes: Some(Map::default()),
                }
            })
            .collect::<Vec<_>>();

        Ok(mpnts)
    }

    // TODO: Implement as_features_4d but not supported in sf
    pub fn as_featureset_2d(self, sr: Option<SpatialReference>) -> FeatureSet<2> {
        let feats = self.as_features_2d(None).expect("Features to be created");
        FeatureSet {
            objectIdFieldName: None,
            globalIdFieldName: None,
            displayFieldName: None,
            spatialReference: sr,
            geometryType: Some("esriGeometryMultiPoint".into()),
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
            geometryType: Some("esriGeometryMultiPoint".into()),
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
fn sfc_multipoint_features_2d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiPoint(x).as_features_2d(sr).unwrap();
    serde_json::to_string(&res).unwrap()
}

#[extendr]
fn sfc_multipoint_features_3d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiPoint(x).as_features_3d(sr).unwrap();
    serde_json::to_string(&res).unwrap()
}

#[extendr]
fn sfc_multipoint_featureset_2d_string(x: List, sr: Robj) -> String {
    let sfc = SfcMultiPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_2d(crs);
    serde_json::to_string(&featureset).unwrap()
}

#[extendr]
fn sfc_multipoint_featureset_3d_string(x: List, sr: Robj) -> String {
    let sfc = SfcMultiPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_3d(crs);
    serde_json::to_string(&featureset).unwrap()
}

#[extendr]
fn sfc_multipoint_features_2d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiPoint(x).as_features_2d(sr).unwrap();
    extendr_api::serializer::to_robj(&res).unwrap()
}

#[extendr]
fn sfc_multipoint_features_3d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let res = SfcMultiPoint(x).as_features_3d(sr).unwrap();
    extendr_api::serializer::to_robj(&res).unwrap()
}

#[extendr]
fn sfc_multipoint_featureset_2d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcMultiPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_2d(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

#[extendr]
fn sfc_multipoint_featureset_3d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcMultiPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset_3d(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

extendr_module! {
    mod multipoint;
    fn sfc_multipoint_features_2d_string;
    fn sfc_multipoint_features_2d_list;
    fn sfc_multipoint_features_3d_string;
    fn sfc_multipoint_features_3d_list;
    fn sfc_multipoint_featureset_2d_string;
    fn sfc_multipoint_featureset_2d_list;
    fn sfc_multipoint_featureset_3d_string;
    fn sfc_multipoint_featureset_3d_list;
}
