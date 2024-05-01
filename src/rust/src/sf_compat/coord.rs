use extendr_api::extendr_module;
use extendr_api::prelude::*;
use crate::sf_compat::AsSfg;
use serde_esri::geometry::EsriCoord;

impl AsSfg for EsriCoord<2> {
    fn as_sfg(&self) -> Robj {
        let crds = Doubles::from_values(self.0);
        crds
            .into_robj()
            .set_class(&["XY", "POINT", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriCoord<3> {
    fn as_sfg(&self) -> Robj {
        let crds = Doubles::from_values(self.0);
        crds
            .into_robj()
            .set_class(&["XYZ", "POINT", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriCoord<4> {
    fn as_sfg(&self) -> Robj {
        let crds = Doubles::from_values(self.0);
        crds
            .into_robj()
            .set_class(&["XYZM", "POINT", "sfg"])
            .unwrap()
    }
}


extendr_module! {
    mod coord;
}