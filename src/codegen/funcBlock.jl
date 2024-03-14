struct FuncBlock <: JLBlock
	fname::WGPUVariable
	fargs::Vector{WGPUVariable}
	Targs::Vector{WGPUVariable}
	fbody::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function funcBlock(scope::Scope, fname::Symbol, fargs::Vector{Any}, fbody::Vector{Any})
	childScope = Scope(Dict(), Dict(), Dict(), scope.depth+1, scope, quote end)
	fn = inferExpr(scope, fname)
	fa = map(x -> inferVariable(scope, x), fargs)
	fb = map(x -> inferExpr(scope, x), fbody)
	return FuncBlock(fn, fa, WGPUVariable[], fb, childScope)
end
