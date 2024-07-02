struct FuncBlock <: JLBlock
	fname::Ref{WGPUVariable}
	fargs::Vector{DeclExpr}
	Targs::Vector{Ref{WGPUVariable}}
	fbody::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function funcBlock(scope::Scope, fname::Symbol, fargs::Vector{Any}, fbody::Vector{Any})
	childScope = Scope(Dict(), scope.moduleVars[], Dict(), scope.depth+1, scope, quote end)
	fn = inferExpr(scope, fname)
	fa = map(x -> inferExpr(scope, x), fargs)
	fb = map(x -> inferExpr(scope, x), fbody)
	return FuncBlock(fn, fa, WGPUVariable[], fb, childScope)
end
