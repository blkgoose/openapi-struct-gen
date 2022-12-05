use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
};

use codegen::Scope;
use heck::{ToSnakeCase, ToSnekCase, ToUpperCamelCase};
use openapiv3::{
    ArrayType, IntegerFormat, IntegerType, NumberFormat, NumberType, ObjectType, ReferenceOr,
    Schema, SchemaKind, Type, VariantOrUnknownOrEmpty,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Element {
    Struct {
        name: String,
        extends: Option<String>,
        fields: HashMap<String, FieldType>,
        derives: HashSet<String>,
        annotations: HashSet<String>,
    },
    Enum {
        name: String,
        variants: Vec<FieldType>,
        derives: HashSet<String>,
        annotations: HashSet<String>,
    },
    Alias(String, FieldType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FieldType {
    String,
    Number(String),
    Integer(String),
    Object {
        name: String,
        fields: HashMap<String, FieldType>,
    },
    Array(Box<FieldType>),
    Boolean,
    Optional(Box<FieldType>),
}

impl Display for FieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldType::String => write!(f, "String"),
            FieldType::Number(t) => write!(f, "{}", t),
            FieldType::Integer(t) => write!(f, "{}", t),
            FieldType::Object { name, .. } => write!(f, "{}", name.to_upper_camel_case()),
            FieldType::Boolean => write!(f, "bool"),
            FieldType::Optional(i) => write!(f, "Option<{}>", *i),
            FieldType::Array(i) => write!(f, "Vec<{}>", *i),
        }
    }
}

impl FieldType {
    fn to_scope(&self, scope: &mut Scope, derives: HashSet<String>, annotations: HashSet<String>) {
        match self {
            FieldType::Object { name, fields } => Element::Struct {
                name: name.clone().to_upper_camel_case(),
                extends: None,
                fields: fields.clone(),
                derives: derives.clone(),
                annotations: annotations.clone(),
            }
            .to_scope(scope),
            FieldType::Optional(o) => (*o).to_scope(scope, derives, annotations),
            _ => (),
        }
    }
}

impl Element {
    pub fn to_scope(&self, scope: &mut Scope) {
        match self {
            Element::Struct {
                name,
                extends: _,
                fields,
                derives,
                annotations,
            } => {
                let mut fields: Vec<(String, FieldType)> = fields
                    .into_iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                fields.sort_by_key(|f| f.0.clone());

                for (_, ftype) in fields.clone() {
                    ftype.to_scope(scope, derives.clone(), annotations.clone());
                }

                let derive_list = derives.into_iter().cloned().collect::<Vec<String>>();
                scope.raw(&format!("#[derive({})]", derive_list.join(", ")));

                for annotation in annotations {
                    scope.raw(&annotation);
                }

                let s = scope.new_struct(&name).vis("pub");
                for (fname, ftype) in fields {
                    s.field(&format!("pub {}", &fname), &ftype.clone().to_string());
                }
            }
            Element::Enum {
                name,
                variants,
                derives,
                annotations,
            } => {
                let derives = derives.into_iter().cloned().collect::<Vec<String>>();
                scope.raw(&format!("#[derive({})]", derives.join(", ")));

                for annotation in annotations {
                    scope.raw(&annotation);
                }

                let e = scope.new_enum(&name).vis("pub");
                for variant in variants {
                    e.new_variant(&variant.clone().to_string())
                        .tuple(&variant.clone().to_string());
                }
            }
            Element::Alias(t, a) => {
                scope.raw(&format!("pub type {} = {};", t, a));
            }
        }
    }
}

pub type Schemas = BTreeMap<String, Schema>;

pub fn generate(
    schemas: Schemas,
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

    let elements: Vec<Element> = schemas_c
        .into_iter()
        .map(|(name, schema)| {
            generate_for_schema(
                name,
                schema,
                schemas.clone(),
                derives.clone(),
                annotations.clone(),
            )
        })
        .collect();

    for el in elements {
        el.to_scope(&mut scope);
    }

    scope.to_string()
}

fn generate_for_schema(
    name: String,
    schema: Schema,
    schemas: Schemas,
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
            .map(|(rname, s)| {
                generate_for_schema(
                    rname.unwrap_or(name.clone()),
                    s.clone(),
                    schemas.clone(),
                    derives.clone(),
                    annotations.clone(),
                )
            })
            .reduce(|a, b| match (a, b) {
                (
                    Element::Struct {
                        name: oname,
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
                        extends: Some(oname),
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

fn get_schema(r: ReferenceOr<Schema>, schemas: &Schemas) -> Option<(Option<String>, Schema)> {
    match r {
        ReferenceOr::Item(i) => Some((None, i)),
        ReferenceOr::Reference { reference } => {
            let reference_type_name = get_item_from_ref(reference);

            schemas
                .get(&reference_type_name)
                .cloned()
                .map(|s| (Some(reference_type_name), s))
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

fn gen_property_type_for_schema_kind(field_name: String, sk: SchemaKind) -> FieldType {
    let t = match sk {
        SchemaKind::Type(r#type) => r#type,
        _ => panic!("Does not support 'oneOf', 'anyOf' 'allOf', 'not' and 'any'"),
    };
    match t {
        Type::String(_) => FieldType::String,
        Type::Number(f) => FieldType::Number(get_number_type(f)),
        Type::Integer(f) => FieldType::Integer(get_integer_type(f)),
        Type::Object(o) => gen_object_type(field_name, o),
        Type::Array(a) => gen_array_type(field_name, a),
        Type::Boolean {} => FieldType::Boolean,
    }
}

fn get_property_type_from_schema_refor(
    field_name: String,
    refor: ReferenceOr<Schema>,
    is_required: bool,
) -> FieldType {
    let t = match refor {
        ReferenceOr::Item(i) => gen_property_type_for_schema_kind(field_name, i.schema_kind),
        ReferenceOr::Reference { reference } => handle_reference(reference),
    };
    if is_required {
        t
    } else {
        FieldType::Optional(Box::new(t))
    }
}

fn gen_object_type(name: String, o: ObjectType) -> FieldType {
    if o.properties.is_empty() {
        todo!("empty object_type")
    } else {
        let mut fields = HashMap::new();

        let required = o.required.into_iter().collect::<HashSet<String>>();
        for (name, refor) in o.properties {
            let is_required = required.contains(&name);

            let t = get_property_type_from_schema_refor(
                name.to_snake_case(),
                refor.unbox(),
                is_required,
            );

            fields.insert(name.to_snek_case(), t);
        }

        FieldType::Object { name, fields }
    }
}

fn gen_array_type(field_name: String, a: ArrayType) -> FieldType {
    let inner_type = if let Some(items) = a.items {
        get_property_type_from_schema_refor(field_name, items.unbox(), true)
    } else {
        todo!();
    };

    FieldType::Array(Box::new(inner_type))
}

fn handle_reference(reference: String) -> FieldType {
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

    let name = split.pop().unwrap().to_owned();

    FieldType::Object {
        name,
        fields: HashMap::new(),
    }
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

                let t = get_property_type_from_schema_refor(
                    name.to_snake_case(),
                    refor.unbox(),
                    is_required,
                );
                fields.insert(name.to_snek_case(), t);
            }

            Element::Struct {
                name,
                extends: None,
                fields,
                derives,
                annotations,
            }
        }

        Type::Array(a) => Element::Alias(name.clone(), gen_array_type(name.clone(), a)),

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

    let mut variants = vec![];

    for t in types.into_iter() {
        let t = get_property_type_from_schema_refor(name.clone(), t, true);
        variants.push(t);
    }

    Element::Enum {
        name,
        variants,
        derives,
        annotations,
    }
}
