struct RangeBlock <: JLBlock
	start::Union{WGPUVariable, Scalar}
	step::Union{WGPUVariable, Scalar}
	stop::Union{WGPUVariable, Scalar}
	idx::Union{JLExpr}
	block::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

struct RangeExpr <: JLExpr
	start::Union{Ref{WGPUVariable}, Scalar}
	step::Union{Ref{WGPUVariable}, Scalar}
	stop::Union{Ref{WGPUVariable}, Scalar}
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
	idxExpr = inferExpr(childScope, :($idx::UInt32))
	setMutable!(idxExpr, true)
	scope.globals[idx] = idxExpr.sym[]
	#scope.locals[idx] = idxExpr.sym
	#inferScope!(childScope, idxExpr)
	exprArray = JLExpr[]
	for stmnt in block
		push!(exprArray, inferExpr(childScope, stmnt))
	end
	rangeBlockExpr = RangeBlock(startExpr, stepExpr, stopExpr, idxExpr, exprArray, childScope)
end
