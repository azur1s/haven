/// std/option: an optional pointer type.
///
/// `Option<*T>` holds either a valid `*T` (`some`) or nothing (`none`).
/// Emptiness is represented by a null pointer, so this is the standard way for a
/// fallible operation (an allocation, a `vec_pop`, a lookup, etc.), to signal "no
/// value" without a sentinel of its own. Test with `is_some`/`is_none`, then
/// retrieve with `unwrap` or `unwrap_or`.

pub struct Option<T> { value: T }

/// An `Option` holding the pointer `value`.
pub proc some<T>(value: *T) Option<*T> {
    return Option::<*T> { value: value };
}
/// The empty `Option`.
pub proc none<T>() Option<*T> {
    return Option::<*T> { value: null::<*T>() };
}

/// Whether the `Option` holds a value.
pub proc is_some<T>(o: Option<*T>) bool {
    return o.value != null::<*T>();
}
/// Whether the `Option` is empty.
pub proc is_none<T>(o: Option<*T>) bool {
    return o.value == null::<*T>();
}

/// The contained pointer. On `none` this prints an error and returns the null
/// pointer rather than aborting, so guard with `is_some` when `none` is possible.
pub proc unwrap<T>(o: Option<*T>) *T {
    if (is_none::<T>(o)) {
        print("Error: Attempted to unwrap a None value\n");
    }
    return o.value;
}

/// The contained pointer, or `default` when the `Option` is empty.
pub proc unwrap_or<T>(o: Option<*T>, default: *T) *T {
    if (is_some::<T>(o)) {
        return o.value;
    } else {
        return default;
    }
}
