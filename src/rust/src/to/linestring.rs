use super::AsEsriGeometry;
use crate::sfg::{Dim, SfgDim, SfgLineString};
use serde_esri::{geometry::*, spatial_reference::SpatialReference};

impl AsEsriGeometry<2> for SfgLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<2>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();

        if nrow == 0 {
            let poly: EsriPolyline<2> = EsriPolyline::default();
            return Some(poly);
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

                let linestring = EsriLineString(points);
                let res = EsriPolyline {
                    hasZ: Some(false),
                    hasM: Some(false),
                    paths: vec![linestring],
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<3> for SfgLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<3>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();
        let slice = self.0.as_real_slice().unwrap();

        match dim {
            SfgDim::XYZ => {
                let mut points = Vec::with_capacity(nrow);
                for i in 0..nrow {
                    let x = slice[i];
                    let y = slice[i + nrow];
                    let z = slice[i + 2 * nrow];
                    let crd = EsriCoord([x, y, z]);
                    points.push(crd);
                }

                let linestring = EsriLineString(points);
                let res = EsriPolyline {
                    hasZ: Some(true),
                    hasM: Some(false),
                    paths: vec![linestring],
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

                let linestring = EsriLineString(points);
                let res = EsriPolyline {
                    hasZ: Some(false),
                    hasM: Some(true),
                    paths: vec![linestring],
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<4> for SfgLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<4>> {
        let dim = self.sfg_dim()?;

        let nrow = self.0.nrows();

        if nrow == 0 {
            return None;
        }

        let slice = self.0.as_real_slice().unwrap();

        match dim {
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

                let linestring = EsriLineString(points);
                let res = EsriPolyline {
                    hasZ: Some(true),
                    hasM: Some(true),
                    paths: vec![linestring],
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}
