use extendr_api::prelude::*;
mod coord;
mod geometry;
mod multipoint;
mod point;
mod polygon;
mod polyline;
mod sfc;
// pub use sfc::handle_features;

pub trait AsSfg {
    fn as_sfg(&self) -> Robj;
}

extendr_module! {
    mod sf_compat;
    use coord;
    use point;
    use polygon;
    use multipoint;
    use polyline;
    use sfc;
}
