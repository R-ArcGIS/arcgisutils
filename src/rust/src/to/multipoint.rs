use super::AsEsriGeometry;
use crate::sfg::{Dim, SfgDim, SfgMultiPoint};
use serde_esri::{geometry::*, spatial_reference::SpatialReference};

impl AsEsriGeometry<2> for SfgMultiPoint {
    fn as_multipoint(&self, sr: Option<SpatialReference>) -> Option<EsriMultiPoint<2>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();

        if nrow == 0 {
            return None;
        }

        let slice = self.0.as_real_slice().unwrap();

        match dim {
            SfgDim::XY => {
                let mut points = Vec::with_capacity(nrow);
                for i in 0..nrow {
                    let x = slice[i];
                    let y = slice[i + nrow];
                    let crd = EsriCoord([x, y]);
                    points.push(crd);
                }

                let res = EsriMultiPoint {
                    hasZ: Some(false),
                    hasM: Some(false),
                    points,
                    spatialReference: sr,
                };

                Some(res)
            }
            SfgDim::XYZ => None,
            SfgDim::XYM => None,
            SfgDim::XYZM => None,
        }
    }
}

impl AsEsriGeometry<3> for SfgMultiPoint {
    fn as_multipoint(&self, sr: Option<SpatialReference>) -> Option<EsriMultiPoint<3>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();

        let slice = self.0.as_real_slice().unwrap();

        match dim {
            SfgDim::XY => None,
            SfgDim::XYZ => {
                let mut points = Vec::with_capacity(nrow);
                for i in 0..nrow {
                    let x = slice[i];
                    let y = slice[i + nrow];
                    let z = slice[i + 2 * nrow];
                    let crd = EsriCoord([x, y, z]);
                    points.push(crd);
                }

                let res = EsriMultiPoint {
                    hasZ: Some(true),
                    hasM: Some(false),
                    points,
                    spatialReference: sr,
                };

                Some(res)
            }
            SfgDim::XYM => {
                let mut points = Vec::with_capacity(nrow);
                for i in 0..nrow {
                    let x = slice[i];
                    let y = slice[i + nrow];
                    let m = slice[i + 2 * nrow];
                    let crd = EsriCoord([x, y, m]);
                    points.push(crd);
                }

                let res = EsriMultiPoint {
                    hasZ: Some(false),
                    hasM: Some(true),
                    points,
                    spatialReference: sr,
                };

                Some(res)
            }
            SfgDim::XYZM => None,
        }
    }
}

impl AsEsriGeometry<4> for SfgMultiPoint {
    fn as_multipoint(&self, sr: Option<SpatialReference>) -> Option<EsriMultiPoint<4>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();

        let slice = self.0.as_real_slice().unwrap();

        match dim {
            SfgDim::XY => None,
            SfgDim::XYZ => None,
            SfgDim::XYM => None,
            SfgDim::XYZM => {
                let mut points = Vec::with_capacity(nrow);
                for i in 0..nrow {
                    let x = slice[i];
                    let y = slice[i + nrow];
                    let z = slice[i + 2 * nrow];
                    let m = slice[i + 3 * nrow];
                    let crd = EsriCoord([x, y, z, m]);
                    points.push(crd);
                }

                let res = EsriMultiPoint {
                    hasZ: Some(true),
                    hasM: Some(true),
                    points,
                    spatialReference: sr,
                };

                Some(res)
            }
        }
    }
}
