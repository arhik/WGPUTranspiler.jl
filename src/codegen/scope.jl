
export Scope

struct Scope
	locals::Vector{Symbol}
	globals::Vector{Symbol}
	depth::Int
	parent::Union{Nothing, Scope}
	code::Expr
end

Scope() = Scope([], [], 0, nothing, quote end)

# TODO hardcoded for now

function findVar(scope::Union{Nothing, Scope}, sym::Symbol)
	if scope == nothing
		return false
	end
	if (sym in scope.locals) || (sym in scope.globals)
		return true
	elseif findVar(scope.parent, sym)
		return true
	else
		return false
	end
end

