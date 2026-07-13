struct Option<T> { value: T }

proc some<T>(value: *T) Option<*T> {
    return Option::<*T> { value: value };
}
proc none<T>() Option<*T> {
    return Option::<*T> { value: null::<*T>() };
}

proc is_some<T>(o: Option<*T>) bool {
    return o.value != null::<*T>();
}
proc is_none<T>(o: Option<*T>) bool {
    return o.value == null::<*T>();
}

proc unwrap<T>(o: Option<*T>) *T {
    if (is_none::<T>(o)) {
        print("Error: Attempted to unwrap a None value\n");
    }
    return o.value;
}

proc unwrap_or<T>(o: Option<*T>, default: *T) *T {
    if (is_some::<T>(o)) {
        return o.value;
    } else {
        return default;
    }
}
