struct RangeBlock <: JLBlock
	start::Union{WGPUVariable, Scalar}
	step::Union{WGPUVariable, Scalar}
	stop::Union{WGPUVariable, Scalar}
	idx::Union{WGPUVariable}
	block::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

struct RangeExpr <: JLExpr
	start::Union{WGPUVariable, Scalar}
	step::Union{WGPUVariable, Scalar}
	stop::Union{WGPUVariable, Scalar}
end

function inferExpr(scope::Scope, range::StepRangeLen)
	@error "Not implemented yet"
end

function rangeBlock(scope::Scope, idx::Symbol, range::Expr, block::Vector{Any})
	# TODO deal with StepRangeLen also may be ? I don't see its use though.
	childScope = Scope(Dict(), Dict(), Dict(), scope.depth + 1, scope, :())
	rangeExpr = inferRange(childScope, range)
	startExpr = rangeExpr.start
	stopExpr =  rangeExpr.stop
	stepExpr = rangeExpr.step
	idxExpr = inferVariable(childScope, :($idx::UInt32))
	inferScope!(childScope, idxExpr)
	exprArray = JLExpr[]
	for stmnt in block
		push!(exprArray, inferExpr(childScope, stmnt))
	end
	rangeBlockExpr = RangeBlock(startExpr, stepExpr, stopExpr, idxExpr, exprArray, childScope)
end
