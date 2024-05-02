use extendr_api::prelude::*;
use crate::sf_compat::AsSfg;
use serde_esri::geometry::EsriMultiPoint;

impl AsSfg for EsriMultiPoint<2> {
    fn as_sfg(&self) -> Robj {

        let m = RArray::new_matrix(
            self.points.len(),
            2, 
            |r, c| self.points[r].0[c]
        );

        m.into_robj()
            .set_class(&["XY", "MULTIPOINT", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriMultiPoint<3> {
    fn as_sfg(&self) -> Robj {

        let m = RArray::new_matrix(
            self.points.len(),
            2, 
            |r, c| self.points[r].0[c]
        );

        m.into_robj()
            .set_class(&["XYZ", "MULTIPOINT", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriMultiPoint<4> {
    fn as_sfg(&self) -> Robj {

        let m = RArray::new_matrix(
            self.points.len(),
            2, 
            |r, c| self.points[r].0[c]
        );

        m.into_robj()
            .set_class(&["XYZ", "MULTIPOINT", "sfg"])
            .unwrap()
    }
}


extendr_module! {
    mod multipoint;
}