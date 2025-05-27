use extendr_api::prelude::*;
use serde_json::{Map, Number, Value};

pub fn df_to_attributes(x: List, n: usize) -> Vec<Map<String, Value>> {
    let ncol = x.len();

    // extract columns from the data.frame
    let col_names = x
        .names()
        .unwrap()
        .map(|si| si.to_string())
        .collect::<Vec<_>>();

    let mut feats = Vec::with_capacity(n);

    for i in 0..(n) {
        let mut map = Map::with_capacity(ncol);
        for j in 0..ncol {
            let name = col_names[j].clone();
            let col = &x[j];
            // println!("column type: {:?}", col.rtype());
            match col.rtype() {
                Rtype::Doubles => {
                    let col_typed = Doubles::try_from(col).unwrap();
                    let v = &col_typed[i];

                    if let false = v.is_na() {
                        let num = Number::from_f64(v.inner())
                            .expect("double can't be converted to serde_json::Number");
                        map.insert(name, Value::Number(num));
                    }
                }
                Rtype::Integers => {
                    let col_typed = Integers::try_from(col).unwrap();
                    let v = &col_typed[i];

                    if let false = v.is_na() {
                        let num = Number::from(v.inner());
                        map.insert(name, Value::Number(num));
                    }
                }
                Rtype::Strings => {
                    let col_typed = Strings::try_from(col).unwrap();
                    let rstr = &col_typed[i];
                    if let false = rstr.is_na() {
                        map.insert(name, Value::String(rstr.to_string()));
                    }
                }
                Rtype::Logicals => {
                    let col_typed = Logicals::try_from(col).unwrap();
                    let v = &col_typed[i];

                    if let false = v.is_na() {
                        map.insert(name, Value::Bool(v.to_bool()));
                    }
                }
                _ => unimplemented!(),
            }
        }

        feats.push(map);
    }

    feats
}

extendr_module! {
    mod attributes;
}
