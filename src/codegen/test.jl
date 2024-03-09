using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:a, :b, :c], [:g, :+], 0, nothing, quote end)

inferredExpr = inferExpr(scope, :(a::Int32 = (a + b + g(b + c))))

