use crate::sf_compat::AsSfg;
use extendr_api::prelude::*;
use serde_esri::geometry::EsriGeometry;

impl AsSfg for EsriGeometry<2> {
    fn as_sfg(&self) -> Robj {
        match self {
            EsriGeometry::Point(p) => p.as_sfg(),
            EsriGeometry::MultiPoint(mp) => mp.as_sfg(),
            EsriGeometry::Polyline(pl) => pl.as_sfg(),
            EsriGeometry::Polygon(pg) => pg.as_sfg(),
            _ => todo!(),
        }
    }
}

impl AsSfg for EsriGeometry<3> {
    fn as_sfg(&self) -> Robj {
        match self {
            EsriGeometry::Point(p) => p.as_sfg(),
            EsriGeometry::MultiPoint(mp) => mp.as_sfg(),
            EsriGeometry::Polyline(pl) => pl.as_sfg(),
            EsriGeometry::Polygon(pg) => pg.as_sfg(),
            _ => todo!(),
        }
    }
}

impl AsSfg for EsriGeometry<4> {
    fn as_sfg(&self) -> Robj {
        match self {
            EsriGeometry::Point(p) => p.as_sfg(),
            EsriGeometry::MultiPoint(mp) => mp.as_sfg(),
            EsriGeometry::Polyline(pl) => pl.as_sfg(),
            EsriGeometry::Polygon(pg) => pg.as_sfg(),
            _ => todo!(),
        }
    }
}

extendr_module! {
    mod geometry;
}
