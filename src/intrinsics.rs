#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    /// `len(slice) -> usize`
    Len,
    /// Numerical type cast
    /// `numerical_cast(0, iN, uN, fN) -> iN/uN/fN`
    NumericalCast,

    /// `simd_splat(T, N, value) -> T where T = simd[T, N]`
    /// e.g. `value = simd_splat(f32, 4, 1.0) -> simd[f32, 4] (1.0, 1.0, 1.0, 1.0)`
    SimdSplat,
    /// `simd_load(T, N, slice, offset: iN/uN) -> T where T = simd[T, N]`
    /// e.g. `value = simd_load(f32, 4, buf, i) -> simd[f32, 4]`
    SimdLoad,
    /// `simd_store(T, N, slice, offset: iN/uN, value: T) -> () where T = simd[T, N]`
    /// e.g. `simd_store(f32, 4, buf, i, value * 0.5) -> ()`
    SimdStore,
    /// `simd_concat(T, N, value1, value2) -> T where T = simd[T, 2N]`
    SimdConcat,
    /// `simd_low(T, N, value: simd[T, M]) -> simd[T, N] where N < M`
    /// e.g. `value = simd_low(f32, 2, simd[f32, 4]) -> simd[f32, 2] (value[0], value[1])`
    SimdLow,
    /// `simd_high(T, N, value: simd[T, M]) -> simd[T, N] where N < M`
    SimdHigh,
}

impl Intrinsic {
    pub fn lookup(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            "numerical_cast" => Some(Self::NumericalCast),
            "simd_splat" => Some(Self::SimdSplat),
            "simd_load" => Some(Self::SimdLoad),
            "simd_store" => Some(Self::SimdStore),
            "simd_concat" => Some(Self::SimdConcat),
            "simd_low" => Some(Self::SimdLow),
            "simd_high" => Some(Self::SimdHigh),
            _ => None,
        }
    }
}

impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Len => "len",
            Self::NumericalCast => "numerical_cast",
            Self::SimdSplat => "simd_splat",
            Self::SimdLoad => "simd_load",
            Self::SimdStore => "simd_store",
            Self::SimdConcat => "simd_concat",
            Self::SimdLow => "simd_low",
            Self::SimdHigh => "simd_high",
        };
        write!(f, "{}", name)
    }
}