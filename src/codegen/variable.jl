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
end

symbol(var::WGPUVariable) = var.sym

