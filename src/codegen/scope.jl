
export Scope, getDataTypeFrom, getDataType, getGlobalScope, findVar

struct Scope
	localVars::Dict{Symbol, Ref{WGPUVariable}}
	newVars::Dict{Symbol, Ref{WGPUVariable}}
	moduleVars::Ref{Dict{Symbol, Ref{WGPUVariable}}}
	typeVars::Dict{Symbol, Ref{WGPUVariable}}
	depth::Int
	parent::Union{Nothing, Scope}
	code::Expr
end

# TODO scope should include wgslFunctions and other builtins by default

Scope() = Scope(Dict(), Dict(), Dict(), Dict(), 0, nothing, quote end)

Scope(localVars::Dict, moduleVars::Dict, typeVars::Dict, depth::Int, parent::Union{Nothing, Scope}, code::Expr) = Scope(
    localVars,
    Dict(),
    moduleVars,
    typeVars,
    depth,
    parent,
    code
)

function findVar(scope::Union{Nothing, Scope}, sym::Symbol)
	if scope == nothing
		return (false, nothing, scope)
	end
	localsyms  = keys(scope.localVars)
	newsyms  = keys(scope.newVars)
	modulesyms = keys(scope.moduleVars[])
	typesyms   = keys(scope.typeVars)
	if (sym in localsyms)
		return (true, :localsym, scope)
	elseif (sym in modulesyms)
		return (true, :modulesym, scope)
    elseif (sym in typesyms)
		return (true, :typesym, scope)
    elseif (sym in newsyms)
		return (true, :newsym, scope)
	end
	return findVar(scope.parent, sym)
end


function getGlobalScope(scope::Union{Nothing, Scope})
	if scope == nothing
		@error "No global state can be retrieved from `nothing` scope"
	elseif scope.depth == 1
		return scope
	else
		getGlobalScope(scope.parent)
	end
end

function getDataTypeFrom(scope::Union{Nothing, Scope}, location, var::Symbol)
	if scope == nothing
		@error "Nothing scope cannot be searched for $var symbol"
	elseif location == :newsym
	   return getindex(scope.newVars[var]).dataType
	elseif location == :localsym
		return getindex(scope.localVars[var]).dataType
	elseif location == :modulesym
		return getindex(scope.moduleVars[][var]).dataType
	elseif location == :typesym
		return getindex(scope.typeVars[var]).dataType
	end
end

function getDataType(scope::Union{Nothing, Scope}, var::Symbol)
	(found, location, rootScope) = findVar(scope, var)
	if found == true
		getDataTypeFrom(rootScope, location, var)
	else
		return Any
	end
end


function Base.isequal(scope::Scope, other::Scope)
	length(scope.localVars) == length(other.localVars) &&
	keys(scope.localVars) == keys(other.localVars) &&
	length(scope.moduleVars[]) == length(other.moduleVars[]) &&
	keys(scope.moduleVars[]) == keys(other.moduleVars[]) &&
	for (key, value) in scope.localVars
		if !Base.isequal(other.localVars[key][], value[])
			return false
		end
	end
	for (key, value) in scope.moduleVars[]
		if Base.isequal(other.moduleVars[][key][], value[])
			return false
		end
	end
	for (key, value) in scope.typeVars
		if Base.isequal(other.typeVars[key][], value[])
			return false
		end
	end
	return true
end
