struct RangeBlock <: JLExpr
	start::Scalar
	stop::Scalar
	step::Scalar
	idx::Union{Symbol, WGPUVariable}
	block::Vector{JLExpr}
end

function rangeBlock(scope::Scope, idx::Symbol, range::Expr, block::Vector{Any})
	childScope = Scope([idx], [], 1, scope, :())
	idxExpr = inferExpr(scope, idx)
	inferScope!(childScope, idxExpr)
	# TODO deal with StepRangeLen also may be ? I don't see its use though.
	rangeExpr = Base.eval(range)
	startExpr = inferExpr(childScope, rangeExpr.start)
	inferScope!(childScope, startExpr)
	stopExpr = inferExpr(childScope, rangeExpr.stop)
	inferScope!(childScope, stopExpr)
	stepExpr = inferExpr(childScope, rangeExpr.step)
	inferScope!(childScope, stepExpr)
	exprArray = JLExpr[]
	for stmnt in block
		push!(exprArray, inferExpr(childScope, stmnt))
	end
	rangeBlockExpr = RangeBlock(startExpr, stopExpr, stepExpr, idxExpr, exprArray)
end
