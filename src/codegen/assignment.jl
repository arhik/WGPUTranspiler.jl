export Scalar, assignExpr

mutable struct LHS
	variable::WGPUVariable
	mutable::Bool
end

mutable struct RHS
	rhs::Union{Nothing, Symbol, Expr, Scalar, JLExpr}
end

function inferScope!(scope, lhs::LHS)
	if findVar(scope, lhs.variable.sym)
		lhs.mutable = true
	else
		push!(scope.locals, lhs.variable.sym)
	end
end

function inferScope!(scope, var::WGPUVariable)
	@assert findVar(scope, var.sym) "Variable $(var.sym) is not in local and global scope"
end


struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::Union{Nothing, RHS}
	scope::Union{Nothing, Scope} # TODO Is Scope mandatory ?
end

function assignExpr(scope, lhs, rhs)
	lhsVar = LHS(inferVariable(lhs), false)
	inferScope!(scope, lhsVar)
	rhsExpr = RHS(inferExpr(scope, rhs))
	statement = AssignmentExpr(lhsVar, rhsExpr, scope)
	#push!(scope.code.args, statement)
	return statement
end

function toWGSLCode(jlexpr::AssignExpr)
	return quote
		#if $(jlexpr.lhs.variable.mutable)
		#	@var $(jlexpr.lhs.variable.sym)
	end
end


