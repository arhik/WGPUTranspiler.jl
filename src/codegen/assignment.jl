export Scalar, assignExpr

function typeInfer(scope::Scope, var::WGPUVariable)
    sym = symbols(var)
    @assert length(sym) == 1 "WGPUVariable should be a holder of single variable"
    varsym = pop!(sym)
	(found, location, rootScope) = findVar(scope, varsym)
	issamescope = rootScope.depth == scope.depth
	# @assert issamescope "Not on same scope! What to do ?"
	if varsym == :WgpuArray
		return eval(varsym)
	end
	if 1 == 1
	end
	var.dataType = getDataTypeFrom(rootScope, location, varsym)
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

symbols(lhs::LHS) = symbols(lhs.expr)
symbols(s::Set, lhs::LHS) = symbols(s, lhs.expr)

typeInfer(scope::Scope, lhs::LHS) = typeInfer(scope, lhs.expr)

mutable struct RHS
	expr::Union{Nothing, Ref{WGPUVariable}, Scalar, JLExpr}
end

symbols(rhs::RHS) = symbols(rhs.expr)
symbols(::Nothing) = Set()
symbols(sym::Symbol) = Set((sym,))
symbols(::Scalar) = Set()

symbols(s::Set, rhs::RHS) = union(s, symbols(s, rhs.expr))
symbols(s::Set, ::Nothing) = s
symbols(s::Set, sym::Symbol) = union(s, symbols(sym))
symbols(s::Set, ::Scalar) = s

typeInfer(scope::Scope, rhs::RHS) = typeInfer(scope, rhs.expr)
typeInfer(scope::Scope, s::Scalar) = eltype(s)

struct AssignmentExpr <: JLExpr
	lhs::LHS
	rhs::RHS
	scope::Scope
end

symbols(assign::AssignmentExpr) = symbols(assign.lhs)
symbols(s::Set, assign::AssignmentExpr) = union(s, symbols(s, assign.lhs))

function assignExpr(scope, lhs::Symbol, rhs::Number)
	rhsExpr = RHS(Scalar(rhs))
	rhsType = typeInfer(scope, rhsExpr)
	(lhsfound, lhslocation, lhsScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if lhsfound && lhslocation == :localsym
		lExpr = lhsScope.localVars[lhs]
		lhsExpr[] = LHS(lExpr, false)
		rhsExpr = RHS(Scalar(rhs |> lExpr[].dataType))
		setMutable!(lhsExpr[], true)
	elseif lhsfound && lhslocation == :moduleVars
		lVar = lhsScope.moduleVars[lhs]
		lVarRef = Ref{WGPUVariable}(lVar)
		lVarRef[].dataType = rhsType
		lhsExprp[] = LHS(lVarRef, false)
		setMutable!(lhsExpr[], true)
	elseif lhsfound == false
		lvar = inferExpr(scope, lhs)
		scope.moduleVars[lhs] = lvar[]
		lvar[].dataType = rhsType
		scope.localVars[lhs] = lvar
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
	(lhsfound, lhslocation, lhsScope) = findVar(scope, symbols(lExpr))
	lhsExpr = Ref{LHS}()
	if typeof(lExpr) == IndexExpr
		if lhsfound && lhslocation == :localsym
			lExpr = lhsScope.localVars[lhs]
			lhsExpr[] = LHS(lExpr, false)
			rhsExpr = RHS(Scalar(rhs |> lExpr[].dataType))
			setMutable!(lhsExpr[], true)
		elseif lhsfound && lhslocation == :moduleVars
			lVar = lhsScope.moduleVars[symbols(lExpr)]
			lVarRef = Ref{WGPUVariable}(lVar)
			rhsExpr = RHS(Scalar(rhs |> eltype(lVar.dataType)))
			lhsExpr[] = LHS(lExpr, false)
			setMutable!(lhsExpr[], true)
		elseif lhsfound == false
			lvar = inferExpr(scope, lhs)
			scope.moduleVars[lhs] = lvar[]
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
	if lhsfound && lhslocation == :localsym
        lExpr = lhsScope.localVars[lhs]
        lhsExpr[] = LHS(lExpr, false)
        lExpr[].dataType = rhsType
        setMutable!(lhsExpr[], true)
	elseif lhsfound && lhslocation == :modulesym
        lVar = lhsScope.moduleVars[lhs]
        lVarRef = Ref{WGPUVariable}(lVar)
        #scope.localVars[lhs] = lVarRef
        lVarRef[].dataType = rhsType
        lhsExpr[] = LHS(lVarRef, false)
        setMutable!(lhsExpr[], true)
    elseif lhsfound && lhslocation == :newsym
        # new var
		lvar = scope.newVars[lhs]
		lvar[].dataType = rhsType
		lvar[].undefined = false
		lhsExpr[] = LHS(lvar, false)
		setMutable!(lhsExpr[], true)
		delete!(scope.newVars, lhs)
	elseif lhsfound == false
        lvar = inferExpr(scope, lhs)
        lhsExpr[] = LHS(lvar, true)
        scope.newVars[lhs] = lvar
        lvar[].dataType = rhsType
        lvar[].undefined = false
	else
	    error("This is not covered")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end


function assignExpr(scope, lhs::Symbol, rhs::Expr)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rhsType = typeInfer(scope, rhsExpr)
	rsyms = symbols(Set(), rhsExpr)
	for rsym in rsyms
	   (found, location, rootScope) = findVar(scope, rsym)
		# TODO this should be ScopeError
	    @assert location != :newsym "RHS variable $rsym should be in scope."
	end
	@infiltrate
	(found, location, rootScope) = findVar(scope, lhs)
	lhsExpr = Ref{LHS}()
	if found && location == :localsym
		lExpr = rootScope.localVars[lhs]
		lhsExpr[] = LHS(lExpr, false)
		@assert lExpr[].dataType == rhsType "$(lExpr[].dataType) != $rhsType"
		lExpr[].undefined = false
		setMutable!(lhsExpr[], true)
	elseif found && location == :modulesym
		lExpr = rootScope.moduleVars[lhs]
		lExprRef = Ref{WGPUVariable}(lExpr)
		rootScope.localVars[lhs] = lExprRef
		lhsExpr[] = LHS(lExprRef, false)
		@assert lExprRef[].dataType == rhsType "$(lExprRef[].dataType) != $rhsType"
		lExprRef[].undefined = false
		setMutable!(lhsExpr[], true)
	elseif found == true && location == :newsym
		# new var
		@infiltrate
		lvar = scope.newVars[lhs]
		lvar[].dataType = rhsType
		lvar[].undefined = false
		lhsExpr[] = LHS(lvar, false)
		setMutable!(lhsExpr[], true)
		delete!(scope.newVars, lhs)
    elseif found == false
        @infiltrate
        lvar = inferExpr(scope, lhs)
        lhsExpr[] = LHS(lvar, true)
        scope.newVars[lhs] = lvar
        lvar[].dataType = rhsType
        lvar[].undefined = false
    else
		error("Not captured this case yet!!!!")
	end
	statement = AssignmentExpr(lhsExpr[], rhsExpr, scope)
	return statement
end

function assignExpr(scope, lhs::Expr, rhs::Union{Expr, Symbol})
	lExpr = inferExpr(scope, lhs)
	rhsExpr = RHS(inferExpr(scope, rhs))
	rsyms = symbols(Set(), rhsExpr)
   	for rsym in rsyms
	   (found, location, rootScope) = findVar(scope, rsym)
  		# TODO this should be ScopeError
   	    @assert (location in [nothing, :newsym]) == false "RHS variable $rsym should be in scope."
   	end
	rhsType = typeInfer(scope, rhsExpr)
	lhsExpr = Ref{LHS}()
	if typeof(lExpr) == IndexExpr
		(found, location, rootScope) = findVar(scope, symbols(lExpr) |> pop!)
		if found && location != :typesym
			lvar = location == :localsym ? rootScope.localVars[symbols(lExpr) |> pop!] : rootScope.moduleVars[symbols(lExpr) |> pop!]
			#lvar = rootScope.localVars[symbols(lExpr)]
			lhsExpr[]  = LHS(lExpr, false)
			lhsType = typeInfer(scope, lhsExpr[])
			@assert lhsType == rhsType "$lhsType != $rhsType"
			#setMutable!(lhsExpr[], true)
		else found == false
			error("LHS var $(symbols(lhs) |> pop!) had to be mutable for indexing")
		end
	elseif typeof(lExpr) == AccessExpr
		(found, location, rootScope) = findVar(scope, symbols(lExpr) |> pop!)
		if found && location !=:typesym
			lExpr = rootScope.localVars[symbols(lExpr) |> pop!]
			lhsExpr[] = LHS(lExpr[], false)
			setMutable!(lhsExpr[], true)
		else found == false
			error("LHS var $(symbols(lhs) |> pop!) has to be mutable for `getproperty`")
		end
	elseif typeof(lExpr) == DeclExpr
        lsym = symbols(lExpr) |> pop!
		(found, location, rootScope) = findVar(scope, lsym)
		if found && location ==:newsym
			lhsExpr[] = LHS(lExpr, true)
			lhsType = typeInfer(scope, lhsExpr[])
			@assert lhsType == rhsType "$lhsType != $rhsType"
			scope.localVars[lsym] = scope.newVars[lsym]
			delete!(scope.newVars, lsym)
		elseif found && location ==:localsym
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
