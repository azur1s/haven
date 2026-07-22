use std::collections::HashMap;
use haven_common::ast::*;
use super::context::{Context, EnumDef};

/// Whether every aggregate (struct, or data-enum) that `ename`'s payload fields
/// reference is already registered in `structs` - i.e. whether it's safe to call
/// `layout::size_of` on `ename`'s payload structs yet. Used to sequence data-enum
/// "pass 2" (aggregate registration) into a fixpoint: a data enum's payload can
/// itself be another (unregistered) data enum (`Option<Option<i32>>`).
pub(crate) fn enum_agg_deps_ready<'a>(
    ename: &str,
    enums: &HashMap<&'a str, EnumDef<'a>>,
    structs: &HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
) -> bool {
    fn ty_ready<'a>(ty: &Type<'a>, structs: &HashMap<&'a str, Vec<(&'a str, Type<'a>)>>) -> bool {
        match ty {
            Type::Struct { name, .. } => structs.contains_key(name),
            Type::Enum { name, has_payload: true, .. } => structs.contains_key(name),
            // a pointer is a fixed-size opaque handle: doesn't need its pointee's
            // own layout, so it never blocks readiness.
            Type::Pointer(_) => true,
            Type::Array(inner, _) | Type::Slice(inner) | Type::Simd(inner, _) => ty_ready(inner, structs),
            _ => true,
        }
    }
    enums[ename].payloads.values().all(|fields| fields.iter().all(|(_, ty)| ty_ready(ty, structs)))
}

/// The discriminant repr type for an enum, from its `@repr(<int>)` attribute.
/// Defaults to `i32` (C `int`); `@repr(C)` is an explicit spelling of that.
/// haven has no 16-bit integer, so `u16`/`i16` are not accepted yet.
pub(crate) fn enum_repr<'a>(attributes: &[Attribute<'a>]) -> Result<Type<'a>, String> {
    for a in attributes {
        if a.value.name == "repr" {
            return match a.value.value.as_deref() {
                None | Some("C") | Some("i32") => Ok(Type::Int32),
                Some("i8")  => Ok(Type::Int8),
                Some("i64") => Ok(Type::Int64),
                Some("u8")  => Ok(Type::Uint8),
                Some("u32") => Ok(Type::Uint32),
                Some("u64") => Ok(Type::Uint64),
                Some(other) => Err(format!(
                    "unknown @repr('{}') on enum; expected an integer type \
                     (i8/i32/i64/u8/u32/u64) or C", other)),
            };
        }
    }
    Ok(Type::Int32)
}

/// If `name` is an `Enum::Variant` reference to a declared enum, return the
/// enum's name, the variant's discriminant value, and the discriminant repr.
/// The enum name comes from the table key so it carries the `'a` lifetime.
pub(crate) fn enum_variant<'a>(cx: &Context<'a>, name: &str) -> Option<(&'a str, i64, Type<'a>)> {
    let (ename, variant) = name.split_once("::")?;
    let (&ekey, def) = cx.enums.get_key_value(ename)?;
    let val = *def.variants.get(variant)?;
    Some((ekey, val, def.repr.clone()))
}

/// Validate that `path` (a joined `E::V`) names a variant of enum `en`; returns
/// the variant name. Shared by the field-less `Path` and the destructuring
/// `Variant` match-arm patterns.
pub(crate) fn check_variant_pattern<'a>(cx: &Context<'a>, en: &'a str, path: &'a str, span: &Span)
-> Result<&'a str, Error> {
    let (pat_enum, variant) = path.split_once("::")
        .ok_or_else(|| Error { msg: format!("invalid enum pattern `{}`", path), span: span.clone() })?;
    // `en` may be a monomorphized instance name (`Option$i32`): the surface
    // pattern is written against the ORIGINAL generic enum name (`Option::Some`),
    // never the mangled one - mono rewrites construction call/struct-literal sites
    // to the mangled name, but never touches match-pattern text (it has no type
    // info to know which instantiation a bare pattern refers to; see mono.rs).
    // `$` can't appear in a source identifier (mangling's own injectivity
    // invariant), so stripping at the first `$` recovers the declared base name.
    let en_base = en.split('$').next().unwrap_or(en);
    if pat_enum != en_base {
        return Err(Error { msg: format!("pattern `{}` is not a variant of enum '{}'", path, en), span: span.clone() });
    }
    if !cx.enums[en].variants.contains_key(variant) {
        return Err(Error { msg: format!("enum '{}' has no variant '{}'", en, variant), span: span.clone() });
    }
    Ok(variant)
}

/// If `name` is `E::V` naming a variant of a declared enum, returns the enum's
/// name and the variant's payload field types (empty for a unit variant). Used
/// to recognize a constructor call `E::V(...)` in `infer`/`lower_expr`.
pub(crate) fn enum_variant_ctor<'a>(cx: &Context<'a>, name: &str) -> Option<(&'a str, Vec<Type<'a>>)> {
    let (ename, variant) = name.split_once("::")?;
    let (&ekey, def) = cx.enums.get_key_value(ename)?;
    if !def.variants.contains_key(variant) { return None; }
    let tys = def.payloads.get(variant)
        .map(|fs| fs.iter().map(|(_, t)| t.clone()).collect())
        .unwrap_or_default();
    Some((ekey, tys))
}

/// If `name` is `E::V` naming a variant of a declared enum, returns the (enum,
/// variant) names carrying the `'a` lifetime from the table keys. Used to route
/// a struct-literal `E::V { ... }` and a `StructVariant` pattern to the variant.
pub(crate) fn split_enum_variant<'a>(cx: &Context<'a>, name: &str) -> Option<(&'a str, &'a str)> {
    let (ename, variant) = name.split_once("::")?;
    let (&ekey, def) = cx.enums.get_key_value(ename)?;
    let (&vkey, _) = def.variants.get_key_value(variant)?;
    Some((ekey, vkey))
}
