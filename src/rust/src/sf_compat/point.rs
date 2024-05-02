use extendr_api::prelude::*;

use crate::sf_compat::AsSfg;

use serde_esri::geometry::EsriPoint;


impl AsSfg for EsriPoint {
    fn as_sfg(&self) -> Robj {
        let has_z = self.z.is_some();
        let has_m = self.m.is_some();

        if has_z && has_m {
            let crds = Doubles::from_values([self.x, self.y, self.z.unwrap(), self.m.unwrap()]);
            crds.
                into_robj()
                .set_class(&["XYZM", "POINT", "sfg"])
                .unwrap()
        } else if has_z {
            let crds = Doubles::from_values([self.x, self.y, self.z.unwrap()]);
            crds
                .into_robj()
                .set_class(&["XYZ", "POINT", "sfg"])
                .unwrap()
        } else if has_m {
            let crds = Doubles::from_values([self.x, self.y, self.m.unwrap()]);
            crds
                .into_robj()
                .set_class(&["XYM", "POINT", "sfg"])
                .unwrap()
        } else {
            let crds = Doubles::from_values([self.x, self.y]);
            crds
                .into_robj()
                .set_class(&["XY", "POINT", "sfg"])
                .unwrap()
        }
    }
}

extendr_module! {
    mod point;
}