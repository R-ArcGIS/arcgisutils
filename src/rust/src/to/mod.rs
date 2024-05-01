use extendr_api::extendr_module;
use serde_esri::{
    geometry::{EsriMultiPoint, EsriPolygon, EsriPolyline},
    spatial_reference::SpatialReference,
};

mod attributes;
mod features;
mod featureset;
mod linestring;
mod multilinestring;
mod multipoint;
mod multipolygon;
mod point;
mod polygon;
// this is used for geometries that have a const usize parameter
// they will always need type annotions
// point is implemented independently since it does not have a
// const generic. It is implemented on the struct itself
#[allow(unused_variables
)]
pub trait AsEsriGeometry<const N: usize> {
    fn as_multipoint(&self, sr: Option<SpatialReference>) -> Option<EsriMultiPoint<N>> {
        None
    }
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<N>> {
        None
    }
    fn as_polygon(&self, sr: Option<SpatialReference>) -> Option<EsriPolygon<N>> {
        None
    }
}

extendr_module! {
    mod to;
    use attributes;
    use features;
    use featureset;
}
