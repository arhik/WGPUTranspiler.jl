export Scalar, assignExpr

mutable struct LHS
	variable::Union{WGPUVariable, JLExpr}
	mutable::Bool
	# isnew::Bool
end

symbol(lhs::LHS) = symbol(lhs.variable)

mutable struct RHS
	rhsExpr::Union{Nothing, WGPUVariable, Scalar, JLExpr}
end

symbol(rhs::RHS) = symbol(rhs.variable)
symbol(::Nothing) = nothing
symbol(sym::Symbol) = sym
symbol(::Scalar) = nothing

function inferScope!(scope, lhs::LHS)
	sym = symbol(lhs)
	@assert findVar(scope, sym) "Variable $sym is not found in local and global scope"
end

function inferScope!(scope, var::WGPUVariable)
	@assert findVar(scope, var.sym) "Variable $(var.sym) is not in local and global scope"
end


struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::Union{Nothing, RHS}
	scope::Union{Nothing, Scope} # TODO Is Scope mandatory ?
end

symbol(assign::AssignmentExpr) = symbol(assign.lhs)

function assignExpr(scope, lhs, rhs)
	lhsVar = LHS(inferExpr(scope, lhs), false)
	inferScope!(scope, lhsVar)
	rhsExpr = RHS(inferExpr(scope, rhs))
	statement = AssignmentExpr(lhsVar, rhsExpr, scope)
	#push!(scope.code.args, statement)
	return statement
end


