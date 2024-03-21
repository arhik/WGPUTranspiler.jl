using CEnum

export WGPUVariableAttribute

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

mutable struct WGPUVariable <: JLVariable
	sym::Symbol
	dataType::Union{DataType, Type}
	varType::WGPUVariableType
	varAttr::Union{Nothing, WGPUVariableAttribute}
	mutable::Bool
	undefined::Bool
end

symbol(var::WGPUVariable) = var.sym
symbol(var::Ref{WGPUVariable}) = var[].sym

isMutable(var::WGPUVariable) = var.mutable
isMutable(var::Ref{WGPUVariable}) = var[].mutable

setMutable!(var::WGPUVariable, b::Bool) = (var.mutable = b)
setMutable!(varRef::Ref{WGPUVariable}, b::Bool) = (varRef[].mutable = b)

Base.isequal(var1::WGPUVariable, var2::WGPUVariable) = begin
	r = true
	for field in fieldnames(WGPUVariable)
		r |= getproperty(var1, field) == getproperty(var2, field)
	end
	return r
end
