struct FuncBlock <: JLBlock
	fname::WGPUVariable
	fargs::Vector{WGPUVariable}
	Targs::Vector{WGPUVariable}
	fbody::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function funcBlock(scope, fname, fargs, fbody)
	childScope = Scope([], [], 0, scope, quote end)
	fn = inferExpr(scope, fname)
	fa = map(x -> inferVariable(scope, x), fargs)
	fb = map(x -> inferExpr(scope, x), fbody)
	return FuncBlock(fn, fa, WGPUVariable[], fb, childScope)
end
