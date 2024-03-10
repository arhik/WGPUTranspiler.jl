using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:i, :a, :d, :b, :c], [:g, :println, :+], 0, nothing, quote end)

inferredExpr = inferExpr(
	scope, 
	:(c = d)
)

