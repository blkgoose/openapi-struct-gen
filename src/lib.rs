#[cfg(feature = "build")]
pub mod error;
#[cfg(feature = "build")]
mod generate;
#[cfg(feature = "build")]
mod parse;

#[cfg(feature = "build")]
use crate::error::GenError;
#[cfg(feature = "build")]
use openapiv3::OpenAPI;

#[cfg(feature = "build")]
pub fn generate<P1: AsRef<std::path::Path>, P2: AsRef<std::path::Path>>(
    schema_filename: P1,
    output_filename: P2,
    derivatives: Option<&[&str]>,
    imports: Option<&[(&str, &str)]>,
    annotations: Option<&[&str]>,
) -> Result<(), GenError> {
    use std::collections::HashSet;

    let schema_filename = schema_filename.as_ref();
    let data = std::fs::read_to_string(schema_filename)?;
    let oapi: OpenAPI = match schema_filename.extension().map(|s| s.to_str().unwrap()) {
        Some("json") => serde_json::from_str(&data)?,
        Some("yaml") | Some("yml") => serde_yaml::from_str(&data)?,
        o => return Err(GenError::WrongFileExtension(o.map(|s| s.to_owned()))),
    };

    let derives = match derivatives {
        Some(d) => HashSet::from_iter(d.to_vec().into_iter().map(|v| v.to_owned())),
        None => HashSet::new(),
    };

    let annotations = match annotations {
        Some(a) => HashSet::from_iter(a.to_vec().into_iter().map(|v| v.to_owned())),
        None => HashSet::new(),
    };

    let schemas_map = parse::parse_schema(oapi);
    let resp = generate::generate(schemas_map, derives, imports, annotations);
    std::fs::write(output_filename, resp)?;
    Ok(())
}

#[macro_export]
macro_rules! include {
    ($package: tt) => {
        include!(concat!(env!("OUT_DIR"), concat!("/", $package, ".rs")));
    };
}
