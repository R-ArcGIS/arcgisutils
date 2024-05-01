use super::AsEsriGeometry;
use crate::sfg::{Dim, SfgDim, SfgMultiPolygon};
use extendr_api::prelude::*;
use serde_esri::{geometry::*, spatial_reference::SpatialReference};

impl AsEsriGeometry<2> for SfgMultiPolygon {
    fn as_polygon(&self, sr: Option<SpatialReference>) -> Option<EsriPolygon<2>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolygon<2> = EsriPolygon::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XY => {
                let linestrings = self
                    .0
                    .iter()
                    .flat_map(|(_, list)| {
                        List::try_from(list).unwrap().into_iter().map(|(_, line)| {
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
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolygon {
                    hasZ: Some(false),
                    hasM: Some(false),
                    rings: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<3> for SfgMultiPolygon {
    fn as_polygon(&self, sr: Option<SpatialReference>) -> Option<EsriPolygon<3>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolygon<3> = EsriPolygon::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XYZ => {
                let linestrings = self
                    .0
                    .iter()
                    .flat_map(|(_, list)| {
                        List::try_from(list).unwrap().into_iter().map(|(_, line)| {
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
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolygon {
                    hasZ: Some(true),
                    hasM: Some(false),
                    rings: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            SfgDim::XYM => {
                let linestrings = self
                    .0
                    .iter()
                    .flat_map(|(_, list)| {
                        List::try_from(list).unwrap().into_iter().map(|(_, line)| {
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
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolygon {
                    hasZ: Some(false),
                    hasM: Some(true),
                    rings: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}

impl AsEsriGeometry<4> for SfgMultiPolygon {
    fn as_polygon(&self, sr: Option<SpatialReference>) -> Option<EsriPolygon<4>> {
        let dim = self.sfg_dim()?;

        let n_elements = self.0.len();

        if n_elements == 0 {
            let poly: EsriPolygon<4> = EsriPolygon::default();
            return Some(poly);
        }

        match dim {
            SfgDim::XYZM => {
                let linestrings = self
                    .0
                    .iter()
                    .flat_map(|(_, list)| {
                        List::try_from(list).unwrap().into_iter().map(|(_, line)| {
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
                    })
                    .collect::<Vec<_>>();

                let res = EsriPolygon {
                    hasZ: Some(true),
                    hasM: Some(true),
                    rings: linestrings,
                    spatialReference: sr,
                };

                Some(res)
            }
            _ => None,
        }
    }
}
