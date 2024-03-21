using WGPUTranspiler
using WGPUTranspiler: WorkGroupDims, Generic, WGPUVariable

using Test

makeVarPair(p::Pair{Symbol, DataType}) = p.first => WGPUVariable(p.first, p.second, Generic, nothing, false, false)

@testset "WGPUTranspiler.jl" begin
	include("variableTests.jl")
	# include("declTests.jl")
	# include("assignmentTests.jl")
end
