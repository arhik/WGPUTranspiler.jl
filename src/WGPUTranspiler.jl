module WGPUTranspiler

using CodeTracking
using Infiltrator 

include("codegen/scalar.jl")
include("codegen/abstracts.jl")
include("codegen/macros.jl")
include("codegen/variable.jl")
include("codegen/scope.jl")
include("codegen/assignment.jl")
include("codegen/atomics.jl")
include("codegen/rangeBlock.jl")
include("codegen/conditionBlock.jl")
include("codegen/builtin.jl")
include("codegen/expr.jl")
include("codegen/funcBlock.jl")
include("codegen/computeBlock.jl")
include("codegen/infer.jl")
include("codegen/resolve.jl")
include("codegen/transpile.jl")
include("codegen/tree.jl")
include("codegen/compile.jl")

export @wgpuatomic

end # module WGPUTranspiler
