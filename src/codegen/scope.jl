using MacroTools

struct Scope
	locals::Vector{Symbol}
	globals::Vector{Symbol}
	depth::Int
	parent::Scope
end

