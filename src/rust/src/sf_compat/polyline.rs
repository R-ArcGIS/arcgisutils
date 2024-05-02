use extendr_api::prelude::*;
use crate::sf_compat::AsSfg;

use serde_esri::geometry::{EsriLineString, EsriPolyline};


// Impls for EsriLineString only return a matrix 
impl AsSfg for EsriLineString<2> {
    fn as_sfg(&self) -> Robj {
        let inner_crds = &self.0;

        let m = RArray::new_matrix(
            inner_crds.len(),
            2, 
            |r, c| inner_crds[r].0[c]
        );

        m.into()
    }
}

impl AsSfg for EsriLineString<3> {
    fn as_sfg(&self) -> Robj {
        let inner_crds = &self.0;
        
        let m = RArray::new_matrix(
            inner_crds.len(),
            2, 
            |r, c| inner_crds[r].0[c]
        );

        m.into()
    }
}

impl AsSfg for EsriLineString<4> {
    fn as_sfg(&self) -> Robj {
        let inner_crds = &self.0;
        
        let m = RArray::new_matrix(
            inner_crds.len(),
            2, 
            |r, c| inner_crds[r].0[c]
        );

        m.into()
    }
}


impl AsSfg for EsriPolyline<2> {
    fn as_sfg(&self) -> Robj {
        let inner_lines = &self.paths;

        let m = List::from_values(
            inner_lines.iter().map(|line| line.as_sfg())
        );

        m.into_robj()
            .set_class(&["XY", "MULTILINESTRING", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriPolyline<3> {
    fn as_sfg(&self) -> Robj {
        let inner_lines = &self.paths;

        let m = List::from_values(
            inner_lines.iter().map(|line| line.as_sfg())
        );

        m.into_robj()
            .set_class(&["XYZ", "MULTILINESTRING", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriPolyline<4> {
    fn as_sfg(&self) -> Robj {
        let inner_lines = &self.paths;

        let m = List::from_values(
            inner_lines.iter().map(|line| line.as_sfg())
        );

        m.into_robj()
            .set_class(&["XYZM", "MULTILINESTRING", "sfg"])
            .unwrap()
    }
}

extendr_module! {
    mod polyline;
}