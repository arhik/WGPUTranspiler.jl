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

function transpile(var::WGPUVariable)
	if var.dataType == Any
		return :($(var.sym))
	else
		return :($(var.sym)::$(var.dataType))
	end
end

struct WGPUVariableAttribute
	group::Int
	binding::Int
end

