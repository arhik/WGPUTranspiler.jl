using Revise
using WGPUCompiler

scope = Scope()

inferExpr(scope, :(a::Int32 = 0))
