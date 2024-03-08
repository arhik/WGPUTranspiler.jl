
export Scope

struct Scope
	locals::Vector{Symbol}
	globals::Vector{Symbol}
	depth::Int
	parent::Union{Nothing, Scope}
	code::Expr
end

Scope() = Scope([], [], 0, nothing, quote end)
