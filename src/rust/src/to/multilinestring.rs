use super::AsEsriGeometry;
use crate::sfg::{Dim, SfgDim, SfgMultiLineString};
use extendr_api::prelude::*;
use serde_esri::{geometry::*, spatial_reference::SpatialReference};

impl AsEsriGeometry<2> for SfgMultiLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<2>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolyline<2> = EsriPolyline::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XY => {
                let linestrings = self
                    .0
                    .iter()
                    .map(|(_, line)| {
                        let mat = RMatrix::<f64>::try_from(line);

                        let mat = match mat {
                            Ok(mat) => mat,
                            Err(_) => return EsriLineString::default(),
                        };

                        let slice = mat.as_real_slice().unwrap();

                        let mut points = Vec::with_capacity(mat.nrows());
                        let nrow = mat.nrows();

                        for i in 0..nrow {
                            let x = slice[i];
                            let y = slice[i + nrow];
                            let crd = EsriCoord([x, y]);
                            points.push(crd);
                        }

                        EsriLineString(points)
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolyline {
                    hasZ: Some(false),
                    hasM: Some(false),
                    paths: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<3> for SfgMultiLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<3>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolyline<3> = EsriPolyline::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XYZ => {
                let linestrings = self
                    .0
                    .iter()
                    .map(|(_, line)| {
                        let mat = RMatrix::<f64>::try_from(line);

                        let mat = match mat {
                            Ok(mat) => mat,
                            Err(_) => return EsriLineString::default(),
                        };

                        let slice = mat.as_real_slice().unwrap();

                        let mut points = Vec::with_capacity(mat.nrows());
                        let nrow = mat.nrows();

                        for i in 0..nrow {
                            let x = slice[i];
                            let y = slice[i + nrow];
                            let z = slice[i + 2 * nrow];
                            let crd = EsriCoord([x, y, z]);
                            points.push(crd);
                        }

                        EsriLineString(points)
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolyline {
                    hasZ: Some(true),
                    hasM: Some(false),
                    paths: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            SfgDim::XYM => {
                let linestrings = self
                    .0
                    .iter()
                    .map(|(_, line)| {
                        let mat = RMatrix::<f64>::try_from(line);

                        let mat = match mat {
                            Ok(mat) => mat,
                            Err(_) => return EsriLineString::default(),
                        };

                        let slice = mat.as_real_slice().unwrap();

                        let mut points = Vec::with_capacity(mat.nrows());
                        let nrow = mat.nrows();

                        for i in 0..nrow {
                            let x = slice[i];
                            let y = slice[i + nrow];
                            let m = slice[i + 2 * nrow];
                            let crd = EsriCoord([x, y, m]);
                            points.push(crd);
                        }

                        EsriLineString(points)
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolyline {
                    hasZ: Some(false),
                    hasM: Some(true),
                    paths: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<4> for SfgMultiLineString {
    fn as_polyline(&self, sr: Option<SpatialReference>) -> Option<EsriPolyline<4>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolyline<4> = EsriPolyline::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XYZM => {
                let linestrings = self
                    .0
                    .iter()
                    .map(|(_, line)| {
                        let mat = RMatrix::<f64>::try_from(line);

                        let mat = match mat {
                            Ok(mat) => mat,
                            Err(_) => return EsriLineString::default(),
                        };

                        let slice = mat.as_real_slice().unwrap();

                        let mut points = Vec::with_capacity(mat.nrows());
                        let nrow = mat.nrows();

                        for i in 0..nrow {
                            let x = slice[i];
                            let y = slice[i + nrow];
                            let z = slice[i + 2 * nrow];
                            let m = slice[i + 3 * nrow];
                            let crd = EsriCoord([x, y, z, m]);
                            points.push(crd);
                        }

                        EsriLineString(points)
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolyline {
                    hasZ: Some(true),
                    hasM: Some(true),
                    paths: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}
