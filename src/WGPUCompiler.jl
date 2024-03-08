module WGPUCompiler

using Infiltrator 

include("codegen/scope.jl")
include("codegen/scalar.jl")
include("codegen/abstracts.jl")
include("codegen/variable.jl")
include("codegen/expr.jl")
include("codegen/assignment.jl")
include("codegen/infer.jl")
include("codegen/resolve.jl")

end # module WGPUCompiler
