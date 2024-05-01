use extendr_api::prelude::*;
use crate::sf_compat::AsSfg;
use serde_esri::geometry::EsriPolygon;

impl AsSfg for EsriPolygon<2> {
    fn as_sfg(&self) -> Robj {
        let inner = self.rings
            .iter()
            .map(|ring| ring.as_sfg())
            .collect::<List>();

        inner
            .into_robj()
            .set_class(&["XY", "POLYGON", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriPolygon<3> {
    fn as_sfg(&self) -> Robj {
        let inner = self.rings
            .iter()
            .map(|ring| ring.as_sfg())
            .collect::<List>();

        inner
            .into_robj()
            .set_class(&["XYZ", "POLYGON", "sfg"])
            .unwrap()
    }
}

impl AsSfg for EsriPolygon<4> {
    fn as_sfg(&self) -> Robj {
        let inner = self.rings
            .iter()
            .map(|ring| ring.as_sfg())
            .collect::<List>();

        inner
            .into_robj()
            .set_class(&["XYZ", "POLYGON", "sfg"])
            .unwrap()
    }
}


extendr_module! {
    mod polygon;
}