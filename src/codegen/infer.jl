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
	elseif 	@capture(expr, function fname_(fargs__) fbody__ end)
		return funcBlock(scope, fname, fargs, fbody)
	elseif 	@capture(expr, function fname_(fargs__) where Targs__ fbody__ end)
		return funcBlock(scope, fname, fargs, Targs, fbody)
	elseif 	@capture(expr, @wgpukernel islaunch_ workgroupSize_ workgroupCount_ function fname_(fargs__) where Targs__ fbody__ end)
		return computeBlock(scope, islaunch, workgroupSize, workgroupCount, fname, fargs, Targs, fbody)
	elseif 	@capture(expr, @wgpukernel islaunch_ workgroupSize_ workgroupCount_ fname_(fargs__))
		return computeBlock(scope, islaunch, workgroupSize, workgroupCount, fname, fargs)
	else
		error("Couldn't capture $expr")
	end
end


function inferExpr(scope::Scope, a::Symbol)
	(found, location, rootScope) = findVar(scope, a)
	var = Ref{WGPUVariable}()
	if found == false
		var[] = WGPUVariable(a, Any, Generic, nothing, false, true)
		scope.globals[a] = var[]
	elseif found == true && location == :globalScope
		var[] = rootScope.globals[a]
		var[].undefined = true
	elseif found == true && location == :typeScope
		var[] = rootScope.typeVars[a]
		var[].undefined = false
	else found == true && location == :localScope
		var = rootScope.locals[a]
		var[].undefined = false
	end
	return var
end

function inferRange(scope, expr::Expr)
	if @capture(expr, a_:b_)
		start = inferExpr(scope, a)
		inferScope!(scope, start)
		stop = inferExpr(scope, b)
		inferScope!(scope, stop)
		step = inferExpr(scope, 1)
		inferScope!(scope, step)
		return RangeExpr(start, step, stop)
	elseif @capture(expr, a_:b_:c_)
		start = inferExpr(scope, a)
		inferScope!(scope, start)
		step = inferExpr(scope, b)
		inferScope!(scope, step)
		stop = inferExpr(scope, c)
		inferScope!(scope, stop)
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
