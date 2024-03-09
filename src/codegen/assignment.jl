export Scalar, assignExpr

mutable struct LHS
	variable::Union{WGPUVariable, JLExpr}
	mutable::Bool
	# isnew::Bool
end

symbol(lhs::LHS) = symbol(lhs.variable)

mutable struct RHS
	rhs::Union{Nothing, Symbol, Scalar, JLExpr}
end

symbol(rhs::RHS) = symbol(rhs.variable)
symbol(::Nothing) = nothing
symbol(sym::Symbol) = sym
symbol(::Scalar) = nothing

function inferScope!(scope, lhs::LHS)
	sym = symbol(lhs)
	if findVar(scope, sym)
		lhs.mutable = true
	else
		push!(scope.locals, sym)
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

symbol(assign::AssignmentExpr) = symbol(assign.lhs)

function assignExpr(scope, lhs, rhs)
	lhsVar = LHS(inferVariable(scope, lhs), false)
	inferScope!(scope, lhsVar)
	rhsExpr = RHS(inferExpr(scope, rhs))
	statement = AssignmentExpr(lhsVar, rhsExpr, scope)
	#push!(scope.code.args, statement)
	return statement
end

function toWGSLCode(jlexpr::AssignmentExpr)
	return quote
		#if $(jlexpr.lhs.variable.mutable)
		#	@var $(jlexpr.lhs.variable.sym)
	end
end


