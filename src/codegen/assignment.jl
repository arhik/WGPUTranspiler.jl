export Scalar, assignExpr

typeInfer(scope::Scope, var::WGPUVariable) = begin
	if symbol(var) == :WgpuArray
		return WgpuArray
	end
	sym = symbol(var)
	inferScope!(scope, var)
	(found, location, rootScope) = findVar(scope, sym)
	issamescope = rootScope.depth == scope.depth
	# @assert issamescope "Not on same scope! What to do ?"
	var.dataType = getDataTypeFrom(rootScope, location, sym)
	return var.dataType
end

typeInfer(scope::Scope, varRef::Ref{WGPUVariable}) = typeInfer(scope, varRef[])

struct LHS
	expr::Union{Ref{WGPUVariable}, JLExpr}
end

isMutable(lhs::LHS) = isMutable(lhs.expr)
setMutable!(lhs::LHS, b::Bool) = setMutable!(lhs.expr, b)


isNew(lhs::LHS) = isNew(lhs.expr)
setNew!(lhs::LHS, b::Bool) = setNew!(lhs.expr, b)

symbol(lhs::LHS) = symbol(lhs.expr)

typeInfer(scope::Scope, lhs::LHS) = begin
	typeInfer(scope, lhs.expr)
end

mutable struct RHS
	expr::Union{Nothing, Ref{WGPUVariable}, Scalar, JLExpr}
end

symbol(rhs::RHS) = symbol(rhs.expr)
symbol(::Nothing) = nothing
symbol(sym::Symbol) = sym
symbol(::Scalar) = nothing

function inferScope!(scope, lhs::LHS)
	sym = symbol(lhs)
	(found, location, rootScope) = findVar(scope, sym)
	if found == true && location != :typeScope
		setMutable!(lhs.expr, true)
	end
end

function inferScope!(scope, var::WGPUVariable)
	(found, _) = findVar(scope, var.sym)
	@assert found == true "Variable $(var.sym) is not in local, global and type scope"
end

function inferScope!(scope, var::Ref{WGPUVariable})
	(found, _) = findVar(scope, (var[]).sym)
	@assert found == true "Variable $(var.sym) is not in local, global and type scope"
end

typeInfer(scope::Scope, rhs::RHS) = typeInfer(scope, rhs.expr)
typeInfer(scope::Scope, s::Scalar) = eltype(s)

struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::RHS
	scope::Scope
end

symbol(assign::AssignmentExpr) = symbol(assign.lhs)

function assignExpr(scope, lhs::Symbol, rhs::Symbol)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(lhsfound, lhslocation, lhsScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if lhsfound
		lExpr = lhsScope.locals[lhs]
		lhsExpr[] = LHS(lExpr)
		lExpr[].dataType = rhsType
		setNew!(lhsExpr[], false)
		setMutable!(lhsExpr[], true)
	else
		lVar = inferExpr(scope, lhs)
		scope.locals[lhs] = lVar
		lVar[].dataType = rhsType
		lhsExpr[] = LHS(lVar)
		setNew!(lhsExpr[], true)
		setMutable!(lhsExpr[], false)
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end

function assignExpr(scope, lhs::Symbol, rhs::Expr)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(found, location, rootScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if found && location != :typeScope
		lExpr = rootScope.locals[lhs]
		lhsExpr[] = LHS(lExpr)
		@assert lExpr[].dataType == rhsType
		setNew!(lhsExpr[], false)
		setMutable!(lhsExpr[], true)
	elseif found == false && location != :typeScope
		# new var
		lvar = inferExpr(scope, lhs)
		scope.locals[lhs] = lvar
		lvar[].dataType = rhsType
		lhsExpr[] = LHS(lvar)
		setNew!(lhsExpr[], true)
		setMutable!(lhsExpr[], false)
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end

function assignExpr(scope, lhs::Expr, rhs::Expr)
	lExpr = inferExpr(scope, lhs)
	rhsExpr = RHS(inferExpr(scope, rhs))
	inferScope!(scope, rhsExpr.expr)
	rhsType = typeInfer(scope, rhsExpr)
	lhsExpr = Ref{LHS}()
	if typeof(lExpr) == IndexExpr
		(found, location, rootScope) = findVar(scope, symbol(lExpr))
		if found && location != :typeScope
			lvar = location == :localScope ? rootScope.locals[symbol(lExpr)] : rootScope.globals[symbol(lExpr)]
			#lvar = rootScope.locals[symbol(lExpr)]
			lhsExpr[]  = LHS(lExpr)
			lhsType = typeInfer(scope, lhsExpr[])
			@assert lhsType == rhsType "$lhsType != $rhsType"
			#setMutable!(lhsExpr[], true)
			#setNew!(lhsExpr[], false)
		else found == false
			error("LHS var $(symbol(lhs)) had to be mutable for indexing")
		end
	elseif typeof(lExpr) == AccessExpr
		(found, location, rootScope) = findVar(scope, symbol(lExpr))
		if found && location !=:typeScope
			lExpr = rootScope.locals[symbol(lExpr)]
			lhsExpr[] = LHS(lExpr[])
			setMutable!(lhsExpr[], true)
			setNew!(lhsExpr[], false)
		else found == false
			error("LHS var $(symbol(lhs)) has to be mutable for `getproperty`")
		end
	elseif typeof(lExpr) == DeclExpr
		(found, location, rootScope) = findVar(scope, symbol(lExpr))
		if found && location !=:typeScope
			lExpr = rootScope.locals[symbol(lExpr)]
			lhsExpr[] = LHS(lExpr)
			setMutable!(lhsExpr[], true)
			setNew!(lhsExpr[], false)
		else found ==  false
			lvar = scope.globals[symbol(lExpr)]
			lvarRef = Ref{WGPUVariable}(lvar)
			scope.locals[symbol(lExpr)] = lvarRef
			lhsExpr[] = LHS(lExpr)
			setMutable!(lhsExpr[], false)
			setNew!(lhsExpr[], true)
		end
	else
		error("This $lhs type Expr is not captured yet")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


