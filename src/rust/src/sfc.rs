use extendr_api::prelude::*;

/// A List of Doubles
pub struct SfcPoint(pub List);
/// A List of RMatrix
pub struct SfcMultiPoint(pub List);
/// A List of RMatrix
pub struct SfcLineString(pub List);
/// A List of List of RMatrix
pub struct SfcMultiLineString(pub List);
/// A List of List of RMatrix ([`SfgPolygon`])
pub struct SfcPolygon(pub List);
/// A List of List of List of RMatrix ([`SfgMultiPolygon`])
pub struct SfcMultiPolygon(pub List);
