use extendr_api::prelude::*;

// represent sfg objects that are compatible with EsriGeometry
pub struct SfgPoint(pub Doubles);
pub struct SfgMultiPoint(pub RMatrix<f64>);
pub struct SfgLineString(pub RMatrix<f64>);
pub struct SfgMultiLineString(pub List);
pub struct SfgPolygon(pub List);
pub struct SfgMultiPolygon(pub List);
// pub struct SfgBbox(pub Doubles);

/// Enum that tracks possible dimension pairs
pub enum SfgDim {
    XY,
    XYZ,
    XYM,
    XYZM,
}

/// Trait that returns None if the Robj
/// isn't actually an sfg Some with the dimension
/// for an actual sfg
pub trait Dim {
    fn sfg_dim(&self) -> Option<SfgDim>;
}

// Checks the classes of an Robj
// Handy for below macro
fn check_dim(x: &Robj) -> Option<SfgDim> {
    if x.inherits("XY") {
        Some(SfgDim::XY)
    } else if x.inherits("XYZ") {
        Some(SfgDim::XYZ)
    } else if x.inherits("XYM") {
        Some(SfgDim::XYM)
    } else if x.inherits("XYZM") {
        Some(SfgDim::XYZM)
    } else {
        None
    }
}

macro_rules! impl_sfg_dim {
    ($sfg:ident) => {
        impl Dim for $sfg {
            fn sfg_dim(&self) -> Option<SfgDim> {
                // cloning is fine because it only increases the reference count
                check_dim(&self.0.clone().into_robj())
            }
        }
    };
}

// Implement for the sfg classes
impl_sfg_dim!(SfgPoint);
impl_sfg_dim!(SfgMultiPoint);
impl_sfg_dim!(SfgLineString);
impl_sfg_dim!(SfgMultiLineString);
impl_sfg_dim!(SfgPolygon);
impl_sfg_dim!(SfgMultiPolygon);
