using Revise
using WGPUCompiler
using AbstractTrees
using WGSLTypes

scope = Scope([:a, :b, :c], [:+,], 0, nothing, quote end)

inferredExpr = inferExpr(scope, :(a::Int32 = (a + b + b + c)))

