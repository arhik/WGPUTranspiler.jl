using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:a, :d, :b, :c], [:g, :+], 0, nothing, quote end)

inferredExpr = inferExpr(scope, :(a[c] = g.b[1]))

