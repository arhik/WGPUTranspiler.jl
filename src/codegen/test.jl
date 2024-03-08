using Revise
using WGPUCompiler

scope = Scope([:a, :b, :c], [], 0, nothing, quote end)

inferExpr(scope, :(a::Int32 = b + c))
