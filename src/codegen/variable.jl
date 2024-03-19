using CEnum

export WGPUVariableAttribute

abstract type AbstractWGPUVariable end

@cenum WGPUVariableType begin
	Dims
	Global
	Local
	Constant
	Intrinsic
	Generic
	Private
	Uniform
	WorkGroup
	StorageRead
	StorageReadWrite
end

struct WGPUVariableAttribute
	group::Int
	binding::Int
end

mutable struct WGPUVariable <: AbstractWGPUVariable
	sym::Symbol
	dataType::Union{DataType, Type}
	varType::WGPUVariableType
	varAttr::Union{Nothing, WGPUVariableAttribute}
	mutable::Bool
	isnew::Bool
end

symbol(var::WGPUVariable) = var.sym
symbol(var::Ref{WGPUVariable}) = var[].sym

isMutable(var::WGPUVariable) = var.mutable
isMutable(var::Ref{WGPUVariable}) = var[].mutable

isNew(var::WGPUVariable) = var.isnew
isNew(var::Ref{WGPUVariable}) = var[].isnew

setMutable!(var::WGPUVariable, b::Bool) = (var.mutable = b)
setMutable!(varRef::Ref{WGPUVariable}, b::Bool) = (varRef[].mutable = b)

setNew!(var::WGPUVariable, b::Bool) = (var.isnew = b)
setNew!(var::Ref{WGPUVariable}, b::Bool) = (var[].isnew = b)
