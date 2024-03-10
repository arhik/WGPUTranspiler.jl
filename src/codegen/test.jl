using Revise
using WGPUCompiler

scope = Scope([:a, :b, :c], [], 0, nothing, quote end)
aExpr = inferExpr(scope, :(a::Int32 = b + c))
transpile(scope, aExpr)

scope = Scope([:a, :b, :c], [:+, :g], 0, nothing, quote end)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(a + b + b + c) + g(2, 3, c)))
transpile(scope, cExpr)
