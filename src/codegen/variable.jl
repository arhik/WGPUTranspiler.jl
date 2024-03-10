using CEnum

abstract type AbstractWGPUVariable end

@cenum WGPUVariableType begin
	Global
	Local
	Constant
	Generic
	Private
	Uniform
	WorkGroup
	StorageRead
	StorageReadWrite
end

struct WGPUVariable <: AbstractWGPUVariable
	sym::Symbol
	dataType::DataType
end

symbol(var::WGPUVariable) = var.sym

struct WGPUVariableAttribute
	group::Int
	binding::Int
end

