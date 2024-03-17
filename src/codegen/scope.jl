
export Scope, getDataTypeFrom, getDataType, getGlobalScope

struct Scope
	locals::Dict{Symbol, WGPUVariable}
	globals::Dict{Symbol, Any}
	typeVars::Dict{Symbol, Any}
	depth::Int
	parent::Union{Nothing, Scope}
	code::Expr
end

# TODO scope should include wgslFunctions and other builtins by default

Scope() = Scope(Dict(), Dict(), Dict(), 0, nothing, quote end)

function findVar(scope::Union{Nothing, Scope}, sym::Symbol)
	if scope == nothing
		return (false, nothing, scope)
	end
	localsyms=keys(scope.locals)
	globalsyms=keys(scope.globals)
	typesyms = keys(scope.typeVars)
	if (sym in localsyms)
		return (true, :localScope, scope)
	elseif (sym in globalsyms)
		return (true, :globalScope, scope)
	elseif (sym in typesyms)
		return (true, :typeScope, scope)
	end
	return findVar(scope.parent, sym)
end

function inferScope!(scope, scalar::Scalar)
	# Do nothing
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
	elseif location == :localScope
		return scope.locals[var].dataType
	elseif location == :globalScope
		return scope.globals[var].dataType
	elseif location == :typeScope
		return scope.typeVars[var].dataType
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
