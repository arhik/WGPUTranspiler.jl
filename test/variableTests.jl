using Revise
using WGPUTranspiler
using WGPUTranspiler: WGPUVariable, WorkGroupDims, Generic
using Test

makeVarPair(p::Pair{Symbol, DataType}; islocal=false) = begin
	if !islocal
		return (p.first => WGPUVariable(
			p.first, # sym
			p.second, # dataType
			Generic,  # varType
			nothing,  # varAttr
			false, 	  # mutable
			false,    # new
			true	  # undefined
		))
	else
		return (p.first => Ref{WGPUVariable}(WGPUVariable(
		p.first, # sym
		p.second, # dataType
		Generic,  # varType
		nothing,  # varAttr
		false, 	  # mutable
		false,    # new
		true	  # undefined
	)))
	end
end

@testset "WGPUVariable Test" begin
	scope = Scope(
		Dict{Symbol, Ref{WGPUVariable}}(
			makeVarPair(:b => Int32),
			makeVarPair(:c => Int32)
		),
		Dict(),
		Dict(),
		0,
		nothing,
		quote end
	)
	scopeCopy = deepcopy(scope)
	aExpr = inferExpr(scope, :(b))
	@test scope.moduleVars[] == Dict()
	@test Base.isequal(scope, scopeCopy)
	#@test (aExpr[]).dataType == Int32
	@test isequal(aExpr, scope.localVars[:b])
end

@testset "WGPUVariable Test" begin
	scope = Scope(
		Dict{Symbol, Ref{WGPUVariable}}(
			makeVarPair(:b => Int32),
			makeVarPair(:c => Int32)
		),
		Dict(),
		Dict(),
		0,
		nothing,
		quote end
	)
	scopeCopy = deepcopy(scope)
	aExpr = inferExpr(scope, :(a))
	@test aExpr[].undefined == true
	@test !(scope.moduleVars[] == Dict())
	@test !Base.isequal(scope, scopeCopy)
	@test (aExpr[]).dataType == Any
	@test isequal(aExpr, scope.localVars[:a])
end
