use std::collections::{BTreeMap, HashMap, HashSet};

use codegen::Scope;
use heck::ToSnekCase;
use openapiv3::{
    ArrayType, IntegerFormat, IntegerType, NumberFormat, NumberType, ObjectType, ReferenceOr,
    Schema, SchemaKind, Type, VariantOrUnknownOrEmpty,
};

#[derive(Debug)]
enum Element {
    Struct {
        name: String,
        fields: HashMap<String, String>,
        derives: HashSet<String>,
        annotations: HashSet<String>,
    },
    Enum {
        name: String,
        variants: HashSet<String>,
        derives: HashSet<String>,
        annotations: HashSet<String>,
    },
    Alias(String, String),
}

pub fn generate(
    schemas: BTreeMap<String, Schema>,
    derives: HashSet<String>,
    imports: Option<&[(&str, &str)]>,
    annotations: HashSet<String>,
) -> String {
    let mut scope = Scope::new();
    if let Some(imports) = imports {
        for (path, name) in imports {
            scope.import(path, name);
        }
    }

    let schemas_c = schemas.clone();

    for (name, schema) in schemas_c.into_iter() {
        match generate_for_schema(
            name,
            schema,
            schemas.clone(),
            derives.clone(),
            annotations.clone(),
        ) {
            Element::Struct {
                name,
                fields,
                derives,
                annotations,
            } => {
                let derives = derives.into_iter().collect::<Vec<String>>();
                scope.raw(&format!("#[derive({})]", derives.join(", ")));

                for annotation in annotations {
                    scope.raw(&annotation);
                }

                let s = scope.new_struct(&name).vis("pub");
                for (fname, ftype) in fields {
                    s.field(&format!("pub {}", &fname), &ftype);
                }
            }
            Element::Enum {
                name,
                variants,
                derives,
                annotations,
            } => {
                let derives = derives.into_iter().collect::<Vec<String>>();
                scope.raw(&format!("#[derive({})]", derives.join(", ")));

                for annotation in annotations {
                    scope.raw(&annotation);
                }

                let e = scope.new_enum(&name).vis("pub");
                for variant in variants {
                    e.new_variant(&variant).tuple(&variant);
                }
            }
            Element::Alias(t, a) => {
                scope.raw(&format!("pub type {} = {};", t, a));
            }
        }
    }
    scope.to_string()
}

fn generate_for_schema(
    name: String,
    schema: Schema,
    schemas: BTreeMap<String, Schema>,
    derives: HashSet<String>,
    annotations: HashSet<String>,
) -> Element {
    match schema.schema_kind {
        SchemaKind::Type(r#type) => generate_struct(name, r#type, derives, annotations),
        SchemaKind::OneOf { one_of } => generate_enum(name, one_of, derives, annotations),
        SchemaKind::AnyOf { any_of } => generate_enum(name, any_of, derives, annotations),
        SchemaKind::AllOf { all_of } => all_of
            .into_iter()
            .map(|r| get_schema(r, &schemas).unwrap())
            .map(|s| {
                generate_for_schema(
                    name.clone(),
                    s,
                    schemas.clone(),
                    derives.clone(),
                    annotations.clone(),
                )
            })
            .reduce(|a, b| match (a, b) {
                (
                    Element::Struct {
                        mut fields,
                        derives,
                        annotations,
                        ..
                    },
                    Element::Struct {
                        name,
                        fields: rfields,
                        ..
                    },
                ) => {
                    fields.extend(rfields);

                    Element::Struct {
                        name,
                        fields,
                        derives,
                        annotations,
                    }
                }
                s => panic!("schema kind cannot be resolved: {:?}", s),
            })
            .unwrap(),
        _ => panic!("Does not support 'not' and 'any'"),
    }
}

fn get_schema(r: ReferenceOr<Schema>, schemas: &BTreeMap<String, Schema>) -> Option<Schema> {
    match r {
        ReferenceOr::Item(i) => Some(i),
        ReferenceOr::Reference { reference } => {
            let reference_type_name = get_item_from_ref(reference);

            schemas.get(&reference_type_name).cloned()
        }
    }
}

fn get_item_from_ref(reference: String) -> String {
    let mut split = reference.split("/").into_iter().collect::<Vec<_>>();
    if split[0] != "#" {
        unreachable!();
    }
    if split[1] != "components" {
        panic!("Trying to load from something other than components");
    }
    if split[2] != "schemas" {
        panic!("Only references to schemas are supported");
    }

    split.pop().unwrap().to_owned()
}

fn get_number_type(t: NumberType) -> String {
    if let VariantOrUnknownOrEmpty::Item(f) = t.format {
        if f == NumberFormat::Double {
            "f64".into()
        } else {
            "f32".into()
        }
    } else {
        "f32".into()
    }
}

fn get_integer_type(t: IntegerType) -> String {
    if let VariantOrUnknownOrEmpty::Item(f) = t.format {
        if f == IntegerFormat::Int64 {
            "i64".into()
        } else {
            "i32".into()
        }
    } else {
        "i32".into()
    }
}

fn gen_property_type_for_schema_kind(sk: SchemaKind) -> String {
    let t = match sk {
        SchemaKind::Type(r#type) => r#type,
        _ => panic!("Does not support 'oneOf', 'anyOf' 'allOf', 'not' and 'any'"),
    };
    match t {
        Type::String(_) => "String".into(),
        Type::Number(f) => get_number_type(f),
        Type::Integer(f) => get_integer_type(f),
        Type::Object(o) => gen_object_type(o),
        Type::Array(a) => gen_array_type(a),
        Type::Boolean {} => "bool".into(),
    }
}

fn get_property_type_from_schema_refor(refor: ReferenceOr<Schema>, is_required: bool) -> String {
    let t = match refor {
        ReferenceOr::Item(i) => gen_property_type_for_schema_kind(i.schema_kind),
        ReferenceOr::Reference { reference } => handle_reference(reference),
    };
    if is_required {
        t
    } else {
        format!("Option<{}>", t)
    }
}

fn gen_object_type(_: ObjectType) -> String {
    todo!("missing object definition")
}

fn gen_array_type(a: ArrayType) -> String {
    let inner_type = if let Some(items) = a.items {
        get_property_type_from_schema_refor(items.unbox(), true)
    } else {
        todo!();
    };
    format!("Vec<{}>", inner_type)
}

fn handle_reference(reference: String) -> String {
    let mut split = reference.split("/").into_iter().collect::<Vec<_>>();
    if split[0] != "#" {
        unreachable!();
    }
    if split[1] != "components" {
        panic!("Trying to load from something other than components");
    }
    if split[2] != "schemas" {
        panic!("Only references to schemas are supported");
    }
    split.pop().unwrap().to_owned()
}

fn generate_struct(
    name: String,
    r#type: Type,
    mut derives: HashSet<String>,
    annotations: HashSet<String>,
) -> Element {
    match r#type {
        Type::Object(obj) => {
            derives.insert("Debug".to_owned());

            let mut fields = HashMap::new();

            let required = obj.required.into_iter().collect::<HashSet<String>>();
            for (name, refor) in obj.properties {
                let is_required = required.contains(&name);

                let t = get_property_type_from_schema_refor(refor.unbox(), is_required);
                fields.insert(name.to_snek_case(), t);
            }

            Element::Struct {
                name,
                fields,
                derives,
                annotations,
            }
        }

        Type::Array(a) => Element::Alias(name, gen_array_type(a)),

        t => {
            println!("#{:#?}", t);
            unreachable!();
        }
    }
}

fn generate_enum(
    name: String,
    types: Vec<ReferenceOr<Schema>>,
    mut derives: HashSet<String>,
    annotations: HashSet<String>,
) -> Element {
    derives.insert("Debug".to_owned());

    let mut variants = HashSet::new();

    for t in types.into_iter() {
        let t = get_property_type_from_schema_refor(t, true);
        variants.insert(t);
    }

    Element::Enum {
        name,
        variants,
        derives,
        annotations,
    }
}
