use std::collections::{BTreeMap, HashSet};

use codegen::Scope;
use heck::ToSnekCase;
use openapiv3::{
    ArrayType, IntegerFormat, IntegerType, NumberFormat, NumberType, ObjectType, ReferenceOr,
    Schema, SchemaKind, Type, VariantOrUnknownOrEmpty,
};

pub fn generate(
    schemas: BTreeMap<String, Schema>,
    derivatives: Option<&[&str]>,
    imports: Option<&[(&str, &str)]>,
    annotations: Option<&[&str]>,
) -> String {
    let mut scope = Scope::new();
    if let Some(imports) = imports {
        for (path, name) in imports {
            scope.import(path, name);
        }
    }

    let schemas_c = schemas.clone();

    for (name, schema) in schemas_c.into_iter() {
        generate_for_schema(
            &mut scope,
            name,
            schema,
            schemas.clone(),
            derivatives,
            annotations,
        );
    }
    scope.to_string()
}

fn generate_for_schema(
    scope: &mut Scope,
    name: String,
    schema: Schema,
    schemas: BTreeMap<String, Schema>,
    derivatives: Option<&[&str]>,
    annotations: Option<&[&str]>,
) {
    match schema.schema_kind {
        SchemaKind::Type(r#type) => generate_struct(scope, name, r#type, derivatives, annotations),
        SchemaKind::OneOf { one_of } => {
            generate_enum(scope, name, one_of, derivatives, annotations)
        }
        SchemaKind::AnyOf { any_of } => {
            generate_enum(scope, name, any_of, derivatives, annotations)
        }
        SchemaKind::AllOf { all_of } => {
            let final_schema = all_of
                .into_iter()
                .map(|r| get_item(r, &schemas).unwrap())
                .reduce(|mut a, b| {
                    a.schema_kind = match (a.schema_kind.clone(), b.schema_kind) {
                        (SchemaKind::Type(t1), SchemaKind::Type(t2)) => {
                            let ft = match (t1, t2) {
                                (Type::Object(mut o1), Type::Object(o2)) => {
                                    o1.properties.extend(o2.properties.into_iter());
                                    Type::Object(o1)
                                }
                                _ => panic!("type not supported for AllOf"),
                            };

                            SchemaKind::Type(ft)
                        }
                        s => panic!("schema kind cannot be resolved: {:?}", s),
                    };

                    a
                })
                .unwrap();

            generate_for_schema(scope, name, final_schema, schemas, derivatives, annotations)
        }
        _ => panic!("Does not support 'not' and 'any'"),
    }
}

fn get_item(r: ReferenceOr<Schema>, schemas: &BTreeMap<String, Schema>) -> Option<Schema> {
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

fn gen_object_type(o: ObjectType) -> String {
    String::new()
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
    scope: &mut Scope,
    name: String,
    r#type: Type,

    derivatives: Option<&[&str]>,
    annotations: Option<&[&str]>,
) {
    match r#type {
        Type::Object(obj) => {
            let mut derivs = vec!["Debug"];
            if let Some(derivatives) = derivatives {
                derivs.extend(derivatives);
            }
            scope.raw(&format!("#[derive({})]", derivs.join(", ")));
            if let Some(annotations) = annotations {
                for annotation in annotations {
                    scope.raw(annotation);
                }
            }
            let r#struct = scope.new_struct(&name).vis("pub");
            let required = obj.required.into_iter().collect::<HashSet<String>>();
            for (name, refor) in obj.properties {
                let is_required = required.contains(&name);
                let t = get_property_type_from_schema_refor(refor.unbox(), is_required);
                r#struct.field(&format!("pub {}", &name.to_snek_case()), &t);
            }
        }
        Type::Array(a) => {
            scope.raw(&format!("pub type {} = {};", name, gen_array_type(a)));
        }
        t => {
            println!("#{:#?}", t);
            unreachable!();
        }
    }
}

fn generate_enum(
    scope: &mut Scope,
    name: String,
    types: Vec<ReferenceOr<Schema>>,
    derivatives: Option<&[&str]>,
    annotations: Option<&[&str]>,
) {
    let mut derivs = vec!["Debug"];
    if let Some(derivatives) = derivatives {
        derivs.extend(derivatives);
    }
    scope.raw(&format!("#[derive({})]", derivs.join(", ")));

    if let Some(annotations) = annotations {
        for annotation in annotations {
            scope.raw(annotation);
        }
    }
    let r#enum = scope.new_enum(&name).vis("pub");

    for t in types.into_iter() {
        let t = get_property_type_from_schema_refor(t, true);
        r#enum.new_variant(&t).tuple(&t);
    }
}
