export Scalar, assignExpr

typeInfer(scope::Scope, var::WGPUVariable) = begin
	if symbol(var) == :WgpuArray
		return eval(symbol(var))
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
	newVar::Bool
end

isMutable(lhs::LHS) = isMutable(lhs.expr)
setMutable!(lhs::LHS, b::Bool) = setMutable!(lhs.expr, b)

isNew(lhs::LHS) = lhs.newVar

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
	if var[].dataType != Function
		(found, _) = findVar(scope, var[].sym)
		@assert found == true "Variable $(var.sym) is not in local, global and type scope"
	end
end

typeInfer(scope::Scope, rhs::RHS) = typeInfer(scope, rhs.expr)
typeInfer(scope::Scope, s::Scalar) = eltype(s)

struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::RHS
	scope::Scope
end

symbol(assign::AssignmentExpr) = symbol(assign.lhs)

function assignExpr(scope, lhs::Symbol, rhs::Number)
	rhsExpr = RHS(Scalar(rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(lhsfound, lhslocation, lhsScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if lhsfound && lhslocation == :localScope
		lExpr = lhsScope.locals[lhs]
		lhsExpr[] = LHS(lExpr, false)
		rhsExpr = RHS(Scalar(rhs |> lExpr[].dataType))
		setMutable!(lhsExpr[], true)
	elseif lhsfound && lhslocation == :globalScope
		lVar = lhsScope.globals[lhs]
		lVarRef = Ref{WGPUVariable}(lVar)
		lVarRef[].dataType = rhsType
		lhsExprp[] = LHS(lVarRef, false)
		setMutable!(lhsExpr[], true)
	elseif lhsfound == false
		lvar = inferExpr(scope, lhs)
		scope.globals[lhs] = lvar[]
		lvar[].dataType = rhsType
		scope.locals[lhs] = lvar
		#lvar[].undefined = true
		lhsExpr[] = LHS(lvar, true)
		setMutable!(lhsExpr[], false)
	else
		error("This should not have reached")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end

function assignExpr(scope, lhs::Expr, rhs::Number)
	lExpr = inferExpr(scope, lhs)
	rhsExpr = RHS(Scalar(rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(lhsfound, lhslocation, lhsScope) = findVar(scope, symbol(lExpr))
	lhsExpr = Ref{LHS}()
	if typeof(lExpr) == IndexExpr
		if lhsfound && lhslocation == :localScope
			lExpr = lhsScope.locals[lhs]
			lhsExpr[] = LHS(lExpr, false)
			rhsExpr = RHS(Scalar(rhs |> lExpr[].dataType))
			setMutable!(lhsExpr[], true)
		elseif lhsfound && lhslocation == :globalScope
			lVar = lhsScope.globals[symbol(lExpr)]
			lVarRef = Ref{WGPUVariable}(lVar)
			rhsExpr = RHS(Scalar(rhs |> eltype(lVar.dataType)))
			lhsExpr[] = LHS(lExpr, false)
			setMutable!(lhsExpr[], true)
		elseif lhsfound == false
			lvar = inferExpr(scope, lhs)
			scope.globals[lhs] = lvar[]
			lvar[].dataType = rhsType
			lhsExpr[] = LHS(lvar, true)
			setMutable!(lhsExpr[], false)
		else
			error("This should not have reached")
		end
	elseif typeof(lExpr) == AccessExpr
	else
		error(" This expr type is not covered : $lExpr")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


function assignExpr(scope, lhs::Symbol, rhs::Symbol)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(lhsfound, lhslocation, lhsScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if lhsfound && lhslocation == :localScope
		lExpr = lhsScope.locals[lhs]
		lhsExpr[] = LHS(lExpr, false)
		lExpr[].dataType = rhsType
		setMutable!(lhsExpr[], true)
	elseif lhsfound && lhslocation == :globalScope
		lVar = lhsScope.globals[lhs]
		lVarRef = Ref{WGPUVariable}(lVar)
		#scope.locals[lhs] = lVarRef
		lVarRef[].dataType = rhsType
		lhsExpr[] = LHS(lVarRef, false)
		setMutable!(lhsExpr[], true)
	elseif found == false
		# setMutable!(lhsExpr[], false)
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


function assignExpr(scope, lhs::Symbol, rhs::Expr)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(found, location, rootScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if found && location == :localScope
		lExpr = rootScope.locals[lhs]
		lhsExpr[] = LHS(lExpr, false)
		@assert lExpr[].dataType == rhsType "$(lExpr[].dataType) != $rhsType"
		lExpr[].undefined = false
		setMutable!(lhsExpr[], true)
	elseif found && location == :globalScope
		lExpr = rootScope.globals[lhs]
		lExprRef = Ref{WGPUVariable}(lExpr)
		rootScope.locals[lhs] = lExprRef
		lhsExpr[] = LHS(lExprRef, false)
		@assert lExprRef[].dataType == rhsType "$(lExprRef[].dataType) != $rhsType"
		lExprRef[].undefined = false
		setMutable!(lhsExpr[], true)
	elseif found == false && location == nothing
		# new var
		lvar = inferExpr(scope, lhs)
		scope.globals[lhs] = lvar[]
		lvar[].dataType = rhsType
		lvar[].undefined = true
		lhsExpr[] = LHS(lvar, true)
		setMutable!(lhsExpr[], false)
	else
		error("Not captured this case yet!!!!")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end

function assignExpr(scope, lhs::Expr, rhs::Union{Expr, Symbol})
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
			lhsExpr[]  = LHS(lExpr, false)
			lhsType = typeInfer(scope, lhsExpr[])
			@assert lhsType == rhsType "$lhsType != $rhsType"
			#setMutable!(lhsExpr[], true)
		else found == false
			error("LHS var $(symbol(lhs)) had to be mutable for indexing")
		end
	elseif typeof(lExpr) == AccessExpr
		(found, location, rootScope) = findVar(scope, symbol(lExpr))
		if found && location !=:typeScope
			lExpr = rootScope.locals[symbol(lExpr)]
			lhsExpr[] = LHS(lExpr[], false)
			setMutable!(lhsExpr[], true)
		else found == false
			error("LHS var $(symbol(lhs)) has to be mutable for `getproperty`")
		end
	elseif typeof(lExpr) == DeclExpr
		(found, location, rootScope) = findVar(scope, symbol(lExpr))
		if found && location ==:globalScope && location != :localScope
			lhsExpr[] = LHS(lExpr, true)
			scope.locals[symbol(lExpr)] = scope.globals[symbol(lExpr)]
		elseif found && location ==:localScope
			if rootScope.depth == scope.depth
				error("Duplication definition is not allowed")
			else
				# set new var node
			end
		else found ==  false
			error("This state shouldn't have been reached in Decl")
		end
	else
		error("This $lhs type Expr is not captured yet")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


