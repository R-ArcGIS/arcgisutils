use std::{collections::HashMap};
use crate::sf_compat::AsSfg;
use extendr_api::prelude::*;
use extendr_api::AsTypedSlice;
use serde_esri::{features::{FeatureSet, Field}, field_type::FieldType};


pub fn schema(x: &FeatureSet<2>) -> HashMap<String, (FieldType, Robj)>  {
    let mut schema: HashMap<String, (FieldType, Robj)> = HashMap::new();
    let n = x.features.len();

    // early return if the featureset is empty
    if x.fields.is_none() {
        return schema;
    }

    x.fields.as_ref()
        .unwrap()
        .into_iter()
        .for_each(| Field { name, field_type, .. } | {
            let res = match &field_type {
                FieldType::EsriFieldTypeSmallInteger => Integers::new(n).into_robj(),
                FieldType::EsriFieldTypeInteger => Integers::new(n).into_robj(),
                FieldType::EsriFieldTypeSingle => Doubles::new(n).into_robj(),
                FieldType::EsriFieldTypeDouble => Doubles::new(n).into_robj(),
                FieldType::EsriFieldTypeString => Strings::new(n).into_robj(),
                FieldType::EsriFieldTypeDate => Doubles::new(n).into_robj(),
                FieldType::EsriFieldTypeOid => Doubles::new(n).into_robj(),
                FieldType::EsriFieldTypeBlob => List::new(n).into_robj(),
                FieldType::EsriFieldTypeGuid => Strings::new(n).into_robj(),
                FieldType::EsriFieldTypeGlobalId => Strings::new(n).into_robj(),
                FieldType::EsriFieldTypeXml => Strings::new(n).into_robj(),
                FieldType::EsriFieldTypeGeometry => unimplemented!(),
                FieldType::EsriFieldTypeRaster => unimplemented!(),
            };
            // TODO can we avoid cloning here?
            schema.insert(name.clone(), (field_type.clone(), res));
        });

    schema 
}


pub fn handle_features(x: FeatureSet<2>) -> Robj {
    let n = x.features.len();
    let mut hm = schema(&x);
    // let mut geoms: Vec<Option<EsriGeometry<2>>> = Vec::with_capacity(n);
    let mut res_geoms = List::new(n);

    // process all features
    let _ = x.features
        .into_iter()
        .enumerate()
        // for each feature
        .for_each(|(i, f)| {
            // push the geometry into a new vec
            // geoms.push(f.geometry);
            if f.geometry.is_some() {
                res_geoms.set_elt(i, f.geometry.unwrap().as_sfg()).unwrap();
            };  
            // for each attribute
            f.attributes
                .into_iter()
                .for_each(|m| {
                   m.into_iter()
                    .for_each(|(k, v)| {
                        let (ft, col) = hm.get_mut(&k).unwrap();
                        match ft {
                            FieldType::EsriFieldTypeSmallInteger => {
                                let val = v.as_i64();
                                let rint = match val {
                                    Some(i) => Rint::from(i as i32),
                                    None => Rint::na(),
                                };
                                let col: &mut [Rint] = col.as_typed_slice_mut().unwrap();
                                col[i] = rint;
                            },
                            FieldType::EsriFieldTypeInteger => {
                                let val = v.as_i64();
                                let rint = match val {
                                    Some(i) => Rint::from(i as i32),
                                    None => Rint::na(),
                                };
                                let col: &mut [Rint] = col.as_typed_slice_mut().unwrap();
                                col[i] = rint;
                            },
                            FieldType::EsriFieldTypeSingle => {
                                let val = v.as_f64();
                                let rdbl = match val {
                                    Some(i) => Rfloat::from(i as f64),
                                    None => Rfloat::na(),
                                };
                                let col: &mut [Rfloat] = col.as_typed_slice_mut().unwrap();
                                col[i] = rdbl;
                            },
                            FieldType::EsriFieldTypeDouble =>  {
                                let val = v.as_f64();
                                let rdbl = match val {
                                    Some(i) => Rfloat::from(i as f64),
                                    None => Rfloat::na(),
                                };
                                let col: &mut [Rfloat] = col.as_typed_slice_mut().unwrap();
                                col[i] = rdbl;
                            },
                            FieldType::EsriFieldTypeString => {
                                let val = v.as_str();
                                let rstr = match val {
                                    Some(i) => Rstr::from(i),
                                    None => Rstr::na(),
                                };
                                let mut col = Strings::from_robj(&col).unwrap();
                                col.set_elt(i, rstr);
                            },
                            // TODO handle dates
                            FieldType::EsriFieldTypeDate => {
                                let val = v.as_f64();
                                let rdbl = match val {
                                    Some(i) => Rfloat::from(i as f64),
                                    None => Rfloat::na(),
                                };
                                let col: &mut [Rfloat] = col.as_typed_slice_mut().unwrap();
                                col[i] = rdbl;
                            },
                            FieldType::EsriFieldTypeOid => {
                                let val = v.as_f64();
                                let rdbl = match val {
                                    Some(i) => Rfloat::from(i as f64),
                                    None => Rfloat::na(),
                                };
                                let col: &mut [Rfloat] = col.as_typed_slice_mut().unwrap();
                                col[i] = rdbl;
                            },
                            FieldType::EsriFieldTypeGuid => {
                                let val = v.as_str();
                                let rstr = match val {
                                    Some(i) => Rstr::from(i),
                                    None => Rstr::na(),
                                };
                                let mut col = Strings::from_robj(&col).unwrap();
                                col.set_elt(i, rstr);
                            },
                            FieldType::EsriFieldTypeGlobalId => {
                                let val = v.as_str();
                                let rstr = match val {
                                    Some(i) => Rstr::from(i),
                                    None => Rstr::na(),
                                };
                                let mut col = Strings::from_robj(&col).unwrap();
                                col.set_elt(i, rstr);
                            },
                            FieldType::EsriFieldTypeXml => {
                                let val = v.as_str();
                                let rstr = match val {
                                    Some(i) => Rstr::from(i),
                                    None => Rstr::na(),
                                };
                                let mut col = Strings::from_robj(&col).unwrap();
                                col.set_elt(i, rstr);
                            },
                            FieldType::EsriFieldTypeBlob => unimplemented!("Blob not supported. Email me."),
                            FieldType::EsriFieldTypeRaster => todo!(),
                            FieldType::EsriFieldTypeGeometry => todo!(),
                        }
                    });
                });
        });

    let (keys, vals): (Vec<String>, Vec<Robj>) = hm.into_iter()
        .map(|(k, (_, col))| {
            (k, col)
        }).unzip();


    let res = List::from_names_and_values(&keys, &vals).unwrap();
    
    if x.geometryType.is_some() {
        return  list!(res, geometry = res_geoms).into();
    }
    
    res.into()
}


// TODO: handle missing / empty geometries
// fn as_sfc(x: Vec<Option<EsriGeometry<2>>>, geom_type: &str) -> Robj {
//     match geom_type {
//         "esriGeometryPoint" => {
//             x.into_iter()
//                 .map(|g| g.unwrap().as_sfg())
//                 .collect::<List>()
//                 .into_robj()
//                 .set_class(&["sfc_POINT", "sfc"])
//                 .unwrap()
//         },
//         "esriGeometryMultipoint" => {
//             x.into_iter()
//                 .map(|g| g.unwrap().as_sfg())
//                 .collect::<List>()
//                 .into_robj()
//                 .set_class(&["sfc_MULTIPOINT", "sfc"])
//                 .unwrap()
//         },
//         "esriGeometryPolyline" => {
//             x.into_iter()
//                 .map(|g| g.unwrap().as_sfg())
//                 .collect::<List>()
//                 .into_robj()
//                 .set_class(&["sfc_MULTILINESTRING", "sfc"])
//                 .unwrap()
//         },
//         "esriGeometryPolygon" => {
//             x.into_iter()
//                 .map(|g| g.unwrap().as_sfg())
//                 .collect::<List>()
//                 .into_robj()
//                 .set_class(&["sfc_POLYGON", "sfc"])
//                 .unwrap()      
//         },
//         _ => unimplemented!()
//     }
// }

extendr_module! {
    mod sfc;
}