use crate::sfg::{Dim, SfgDim, SfgPoint};
use extendr_api::prelude::*;
use serde_esri::{geometry::*, spatial_reference::SpatialReference};

impl SfgPoint {
    pub fn as_point(&self, sr: Option<SpatialReference>) -> Option<EsriPoint> {
        let dim = self.sfg_dim()?;

        // if it is an empty geometry
        if self.0.len() == 0 {
            return None;
        }

        match dim {
            SfgDim::XY => Some(EsriPoint {
                x: self.0.elt(0).inner(),
                y: self.0.elt(1).inner(),
                z: None,
                m: None,
                spatialReference: sr,
            }),
            SfgDim::XYZ => Some(EsriPoint {
                x: self.0.elt(0).inner(),
                y: self.0.elt(1).inner(),
                z: Some(self.0.elt(2).inner()),
                m: None,
                spatialReference: sr,
            }),
            SfgDim::XYM => Some(EsriPoint {
                x: self.0.elt(0).inner(),
                y: self.0.elt(1).inner(),
                z: None,
                m: Some(self.0.elt(2).inner()),
                spatialReference: sr,
            }),
            SfgDim::XYZM => Some(EsriPoint {
                x: self.0.elt(0).inner(),
                y: self.0.elt(1).inner(),
                z: Some(self.0.elt(2).inner()),
                m: Some(self.0.elt(3).inner()),
                spatialReference: sr,
            }),
        }
    }
}
