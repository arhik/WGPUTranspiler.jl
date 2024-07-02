using MacroTools

export inferExpr, @infer

function inferExpr(scope::Scope, expr::Expr)
	if @capture(expr, lhs_ = rhs_)
		return assignExpr(scope, lhs, rhs)
	elseif @capture(expr, a_ + b_)
		return binaryOp(scope, :+, a, b)
	elseif @capture(expr, a_ - b_)
		return binaryOp(scope, :-, a, b)
	elseif @capture(expr, a_ * b_)
		return binaryOp(scope, :*, a, b)
	elseif @capture(expr, a_ / b_)
		return binaryOp(scope, :/, a, b)
	elseif @capture(expr, a_ < b_)
		return binaryOp(scope, :<, a, b)
	elseif @capture(expr, a_ > b_)
		return binaryOp(scope, :>, a, b)
	elseif @capture(expr, a_ <= b_)
		return binaryOp(scope, :<=, a, b)
	elseif @capture(expr, a_ >= b_)
		return binaryOp(scope, :>=, a, b)
	elseif @capture(expr, a_ == b_)
		return binaryOp(scope, :(==), a, b)
	elseif @capture(expr, a_ += b_)
		return binaryOp(scope, :+=, a, b) # TODO this should be assignment expr
	elseif @capture(expr, a_ -= b_)
		return binaryOp(scope, :-=, a, b) # TODO this should be assignment expr
	elseif @capture(expr, f_(args__))
		return callExpr(scope, f, args)
	elseif @capture(expr, a_::b_)
		return declExpr(scope, a, b)
	elseif @capture(expr, a_[b_])
		return indexExpr(scope, a, b)
	elseif @capture(expr, a_.b_)
		return accessExpr(scope, a, b)
	elseif @capture(expr, a_{b__})
		return typeExpr(scope, a, b)
	elseif @capture(expr, for idx_ in range_ block__ end)
		return rangeBlock(scope, idx, range, block)
	elseif @capture(expr, if cond_ block__ end)
		return ifBlock(scope, cond, block)
	elseif @capture(expr, if cond_ ifblock__ else elseblock__ end)
		return ifelseBlock(scope, cond, ifblock, elseblock)
	elseif @capture(expr, function fname_(fargs__) fbody__ end)
		return funcBlock(scope, fname, fargs, fbody)
	elseif @capture(expr, function fname_(fargs__) where Targs__ fbody__ end)
		return funcBlock(scope, fname, fargs, Targs, fbody)
	elseif @capture(expr, @wgpuatomic aExpr_)
		return atomicExpr(scope, aExpr)
	elseif 	@capture(expr, @wgpukernel islaunch_ workgroupSize_ workgroupCount_ function fname_(fargs__) where Targs__ fbody__ end)
		return computeBlock(scope, islaunch, workgroupSize, workgroupCount, fname, fargs, Targs, fbody)
	elseif 	@capture(expr, @wgpukernel islaunch_ workgroupSize_ workgroupCount_ shmem_ (fname_)(fargs__))
		fexpr = @code_string(fname(fargs...)) |> Meta.parse |> MacroTools.striplines
		#scope = Scope(Dict(), Dict(), Dict(), 0, nothing, quote end)
		return computeBlock(scope, islaunch, workgroupSize, workgroupCount, shmem, fname, fargs, fexpr)
	else
		error("Couldn't capture $expr")
	end
end


function inferExpr(scope::Scope, a::Symbol)
	(found, location, rootScope) = findVar(scope, a)
	var = Ref{WGPUVariable}()
	if found == false
		var[] = WGPUVariable(a, Any, Generic, nothing, false, true)
		scope.newVars[a] = var
	elseif found == true && location == :modulesym
		var = rootScope.moduleVars[][a]
		var[].undefined = false
	elseif found == true && location == :typesym
		var = rootScope.typeVars[a]
		var[].undefined = false
	elseif found == true && location == :localsym
		var = rootScope.localVars[a]
		var[].undefined = false
	elseif found == true && location == :newsym
	    var = rootScope.newVars[a]
		scope.localVars[a] = var
		delete!(scope.newVars, a)
		var[].undefined = false
	end
	return var
end

function inferRange(scope, expr::Expr)
	if @capture(expr, a_:b_)
		start = inferExpr(scope, a)
		stop = inferExpr(scope, b)
		step = inferExpr(scope, 1)
		return RangeExpr(start, step, stop)
	elseif @capture(expr, a_:b_:c_)
		start = inferExpr(scope, a)
		step = inferExpr(scope, b)
		stop = inferExpr(scope, c)
		return RangeExpr(start, step, stop)
	end
end

function inferExpr(scope::Scope, a::Number)
	return Scalar(a)
end

function inferExpr(scope::Scope, a::WGPUScalarType)
	return Scalar(a)
end

inferExpr(scope::Scope, a::Scalar) = a

macro infer(cntxt, expr)
	inferExpr(cntxt, expr)
end
