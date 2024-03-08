export Scalar, WGPUScalarType

const WGPUScalarType = Union{Float32, UInt32, Int32, Bool}

struct Scalar
	element::WGPUScalarType
end

Base.convert(::Type{WGPUScalarType}, a::Int64) = WGPUScalarType(Int32(a))
Base.convert(::Type{WGPUScalarType}, a::UInt64) = WGPUScalarType(UInt32(a))
Base.convert(::Type{WGPUScalarType}, a::Float64) = WGPUScalarType(Float32(a))

Base.eltype(s::Scalar) = typeof(s.element)
