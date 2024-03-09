using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:d, :b, :c], [:g, :+], 0, nothing, quote end)

inferredExpr = inferExpr(scope, :(a[c] = b[1]))

