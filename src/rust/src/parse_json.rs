// use arrow_extendr::to::IntoArrowRobj;
// use serde_esri::{arrow_compat::featureset_to_arrow, features::FeatureSet};

// #[extendr]
// /// @export
// fn parse_esri_json_str_simd(str: String, n_dim: i32) -> Robj {
//     let n_dim = n_dim as usize;
//     let mut str = str;
//     let str = str.as_mut_str();

//     match n_dim {
//         // 0 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<0>>(str).unwrap()) },
//         2 => unsafe {
//             crate::sf_compat::handle_features(
//                 simd_json::serde::from_str::<FeatureSet<2>>(str).unwrap(),
//             )
//         },
//         // 3 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<3>>(str).unwrap()) },
//         // 4 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<4>>(str).unwrap()) },
//         _ => unimplemented!(),
//     }
// }

// #[extendr]
// / @export
// fn parse_esri_json_raw_simd(raw: Raw, n_dim: i32) -> Robj {
//     let n_dim = n_dim as usize;
//     let mut raw = raw;
//     let bytes = unsafe { raw.as_typed_slice_raw_mut() };
//     match n_dim {
//         // 0 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<0>>(str).unwrap()) },
//         2 => crate::sf_compat::handle_features(
//             simd_json::serde::from_slice::<FeatureSet<2>>(bytes).unwrap(),
//         ),
//         // 3 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<3>>(str).unwrap()) },
//         // 4 => unsafe { fset_to_robj(simd_json::serde::from_str::<FeatureSet<4>>(str).unwrap()) },
//         _ => unimplemented!(),
//     }
// }

// #[extendr]
// /// @export
// fn parse_esri_json_str(str: String, n_dim: i32) -> Robj {
//     let n_dim = n_dim as usize;
//     let str = str.as_str();

//     let res = match n_dim {
//         2 => serde_json::from_str::<FeatureSet<2>>(str).unwrap(),
//         _ => unimplemented!(),
//     };

//     let robj = crate::sf_compat::handle_features(res);

//     robj
// }

// #[extendr]
// /// @export
// fn parse_esri_json_raw(raw: Raw, n_dim: i32) -> Robj {
//     let n_dim = n_dim as usize;
//     let bytes = raw.as_slice();

//     match n_dim {
//         // 0 => (serde_json::from_slice::<FeatureSet<0>>(bytes).unwrap()),
//         2 => crate::sf_compat::handle_features(
//             serde_json::from_slice::<FeatureSet<2>>(bytes).unwrap(),
//         ),
//         // 3 => fset_to_robj(serde_json::from_slice::<FeatureSet<3>>(bytes).unwrap()),
//         // 4 => fset_to_robj(serde_json::from_slice::<FeatureSet<4>>(bytes).unwrap()),
//         _ => unimplemented!(),
//     }
// }

// #[extendr]
// /// @export
// fn parse_esri_json_raw_geoarrow(raw: Raw, n_dim: i32) -> Robj {
//     let n_dim = n_dim as usize;
//     let bytes = raw.as_slice();

//     match n_dim {
//         // 0 => (serde_json::from_slice::<FeatureSet<0>>(bytes).unwrap()),
//         2 => featureset_to_arrow(serde_json::from_slice::<FeatureSet<2>>(bytes).unwrap())
//             .unwrap()
//             .into_arrow_robj()
//             .unwrap(),
//         // 3 => fset_to_robj(serde_json::from_slice::<FeatureSet<3>>(bytes).unwrap()),
//         // 4 => fset_to_robj(serde_json::from_slice::<FeatureSet<4>>(bytes).unwrap()),
//         _ => unimplemented!(),
//     }
// }
