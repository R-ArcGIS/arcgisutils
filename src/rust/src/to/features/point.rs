use crate::deserialize_sr;
use crate::{sfc::*, sfg::SfgPoint};
use extendr_api::prelude::*;
use serde_esri::{
    features::{Feature, FeatureSet},
    geometry::EsriGeometry,
    spatial_reference::SpatialReference,
};
use serde_json::Map;

impl SfcPoint {
    /// Consume an SfcPoint to return a vector of Fetaures
    pub fn as_features<const N: usize>(
        self,
        sr: Option<SpatialReference>,
    ) -> Result<Vec<Feature<N>>> {
        let feats = self
            .0
            .into_iter()
            .map(|(_, feat)| {
                let inner = Doubles::try_from(feat).expect("doubles vector");
                let geom = SfgPoint(inner)
                    .as_point(sr.clone())
                    .expect("correct length of doubles vector");

                Feature {
                    geometry: Some(EsriGeometry::Point(geom)),
                    attributes: Some(Map::default()),
                }
            })
            .collect::<Vec<_>>();
        Ok(feats)
    }
    /// Consume an SfcPoint to create a FeatureSet<N>
    /// A spatial reference should be created and passed in from the `crs` attribute
    /// of the sfc object. For points, it is safe to ignore the `<N>` constant, I believe.
    pub fn as_featureset<const N: usize>(self, sr: Option<SpatialReference>) -> FeatureSet<N> {
        let feats = self.as_features(None).expect("Features to be created");
        let z = Some(N >= 3);
        let m = Some(N == 4);
        FeatureSet {
            objectIdFieldName: None,
            globalIdFieldName: None,
            displayFieldName: None,
            spatialReference: sr,
            geometryType: Some("esriGeometryPoint".into()),
            features: feats,
            fields: None,
            // TODO parameterize this??
            // Because
            hasM: m,
            hasZ: z,
        }
    }
}

#[extendr]
fn sfc_point_features_2d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let sfc = SfcPoint(x);
    let features = sfc.as_features::<2>(sr).unwrap();
    serde_json::to_string(&features).unwrap()
}

#[extendr]
fn sfc_point_features_3d_string(x: List, sr: Robj) -> String {
    let sr = deserialize_sr(&sr);
    let sfc = SfcPoint(x);
    let features = sfc.as_features::<3>(sr).unwrap();
    serde_json::to_string(&features).unwrap()
}

#[extendr]
fn sfc_point_featureset_2d_string(x: List, sr: Robj) -> String {
    let sfc = SfcPoint(x);
    // This should be part of the R library
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset::<2>(crs);
    serde_json::to_string(&featureset).unwrap()
}

#[extendr]
fn sfc_point_featureset_3d_string(x: List, sr: Robj) -> String {
    let sfc = SfcPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset::<3>(crs);
    serde_json::to_string(&featureset).unwrap()
}

#[extendr]
fn sfc_point_features_2d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let sfc = SfcPoint(x);
    let features = sfc.as_features::<2>(sr).unwrap();
    extendr_api::serializer::to_robj(&features).unwrap()
}

#[extendr]
fn sfc_point_features_3d_list(x: List, sr: Robj) -> Robj {
    let sr = deserialize_sr(&sr);
    let sfc = SfcPoint(x);
    let features = sfc.as_features::<3>(sr).unwrap();
    extendr_api::serializer::to_robj(&features).unwrap()
}

#[extendr]
fn sfc_point_featureset_2d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcPoint(x);
    // This should be part of the R library
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset::<2>(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

#[extendr]
fn sfc_point_featureset_3d_list(x: List, sr: Robj) -> Robj {
    let sfc = SfcPoint(x);
    let crs = deserialize_sr(&sr);
    let featureset = sfc.as_featureset::<3>(crs);
    extendr_api::serializer::to_robj(&featureset).unwrap()
}

extendr_module! {
    mod point;
    fn sfc_point_features_2d_string;
    fn sfc_point_features_3d_string;
    fn sfc_point_featureset_2d_string;
    fn sfc_point_featureset_3d_string;
    fn sfc_point_features_2d_list;
    fn sfc_point_features_3d_list;
    fn sfc_point_featureset_2d_list;
    fn sfc_point_featureset_3d_list;
}
