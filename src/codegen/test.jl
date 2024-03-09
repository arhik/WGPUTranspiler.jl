using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:a, :d, :b, :c], [:g, :println, :+], 0, nothing, quote end)

inferredExpr = inferExpr(
	scope, 
	:(
		for i in 0:1:10
			println(i) 
			a[i] = b[i]
			c[i] = d[i] + c[i] + 1.0
		end
	)
)

