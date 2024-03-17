export Scalar, assignExpr

typeInfer(scope::Scope, var::WGPUVariable) = begin
	sym = symbol(var)
	inferScope!(scope, var)
	(found, location, rootScope) = findVar(scope, sym)
	issamescope = rootScope.depth == scope.depth
	# @assert issamescope "Not on same scope! What to do ?"
	var.dataType = getDataTypeFrom(rootScope, location, sym)
	return var.dataType
end

struct LHS
	variable::Union{WGPUVariable, JLExpr}
end

isMutable(lhs::LHS) = isMutable(lhs.variable)
setMutable!(lhs, b::Bool) = setMutable!(lhs.variable, b)

isNew(lhs::LHS) = isNew(lhs.variable)
setNew!(lhs::LHS, b::Bool) = setNew!(lhs.variable, b)

symbol(lhs::LHS) = symbol(lhs.variable)

typeInfer(scope::Scope, lhs::LHS) = begin
	typeInfer(scope, lhs.variable)
end

mutable struct RHS
	rhsExpr::Union{Nothing, WGPUVariable, Scalar, JLExpr}
end

symbol(rhs::RHS) = symbol(rhs.variable)
symbol(::Nothing) = nothing
symbol(sym::Symbol) = sym
symbol(::Scalar) = nothing

function inferScope!(scope, lhs::LHS)
	sym = symbol(lhs)
	(found, location, rootScope) = findVar(scope, sym)
	if found == true && location != :typeScope
		setMutable!(lhs.variable, true)
	end
end

function inferScope!(scope, var::WGPUVariable)
	(found, _) = findVar(scope, var.sym)
	@assert found == true "Variable $(var.sym) is not in local, global and type scope"
end

typeInfer(scope::Scope, rhs::RHS) = typeInfer(scope, rhs.rhsExpr)
typeInfer(scope::Scope, s::Scalar) = eltype(s)

struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::RHS
	scope::Scope
end

symbol(assign::AssignmentExpr) = symbol(assign.lhs)

function assignExpr(scope, lhs, rhs)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	lExpr = inferExpr(scope, lhs)
	lhsExpr = Ref{LHS}()
	if typeof(lExpr) == DeclExpr
		lhsType = typeInfer(scope, lExpr)
		@assert rhsType == lhsType "Datatypes of assignment operands are not compatible $lhsType != $rhsType"
		lhsExpr[] = LHS(lExpr)
		setNew!(lhsExpr[], true)
	elseif typeof(lExpr) == IndexExpr
		lhsType = typeInfer(scope, lExpr)
		@assert rhsType == lhsType "Datatypes of assignment operands are not compatible $lhsType != $rhsType"
		lhsExpr[] = LHS(lExpr)
		setMutable!(lhsExpr[], true)
	elseif typeof(lExpr) == WGPUVariable
		# lhsType = typeInfer(scope, lExpr)
		(lhsFound, location, rootScope) = findVar(scope, symbol(lExpr))
		@assert location != :typeScope "variable is found in typeScope and cannot be used as local variable"
		if (lhsFound == false)
			lhsExpr[] = LHS(lExpr)
			lExpr.dataType = rhsType
			scope.locals[symbol(lExpr)] = lhsExpr[].variable
			setNew!(lhsExpr[], true)
			setMutable!(lhsExpr[], false)
		else (lhsFound == true)
			@infiltrate
			lhsExpr[] = LHS(lExpr)
			setNew!(lhsExpr[], false)
			setMutable!(lhsExpr[], true)
		end
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


