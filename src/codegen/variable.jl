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
isMutable(var::WGPUVariable) = var.mutable

isNew(var::WGPUVariable) = var.isnew

setMutable!(var::WGPUVariable, b::Bool) = (var.mutable = b) 
setNew!(var::WGPUVariable, b::Bool) = (var.isnew = b)
