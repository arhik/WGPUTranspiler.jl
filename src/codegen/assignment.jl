export Scalar, assignExpr

mutable struct LHS
	variable::WGPUVariable
	mutable::Bool
end

mutable struct RHS
	rhs::Union{Nothing, Symbol, Expr, Scalar}
end

struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::Union{Nothing, RHS}
	scope::Union{Nothing, Scope} # TODO Is Scope mandatory ?
end

# TODO hardcoded for now
function inferScope!(scope, var::LHS)
	var.mutable = false
end

function assignExpr(scope, lhs, rhs)
	lhsVar = LHS(inferVariable(lhs), false)
	inferScope!(scope, lhsVar)
	rhsExpr = RHS(inferExpr(scope, rhs))
	return AssignmentExpr(lhsVar, rhsExpr, scope)
end

