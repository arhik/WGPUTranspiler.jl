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
		return binaryOp(scope, :-, a, b)
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
		return binaryOp(scope, :>=, a, b)
	elseif @capture(expr, a_ += b_)
		return binaryOp(scope, :+=, a, b)
	elseif @capture(expr, a_ -= b_)
		return binaryOp(scope, :-=, a, b)
	elseif @capture(expr, f_(args__))
		return callExpr(scope, f, args)
	elseif @capture(expr, a_[b_])
		return indexExpr(scope, a, b)
	elseif @capture(expr, a_.b_)
		return accessExpr(scope, a, b)
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

function inferVariable(scope, expr::Expr)
	if @capture(expr, a_::b_{t__})
		push!(scope.locals, a)
		return WGPUVariable(a, eval(b), Generic, nothing, ) # TODO t is ignored
	elseif @capture(expr, a_::b_)
		push!(scope.locals, a)
		return WGPUVariable(a, eval(b), Generic, nothing, )
	elseif @capture(expr, a_[b_])
		return indexExpr(scope, a, b)
	else
		error("This expression $expr type is not captured yet")
	end
end

function inferVariable(scope, array::WgpuArray)
	# push!(scope.globals, )
	@error "This should not have happened"
end

function inferVariable(scope, sym::Symbol)
	#TODO DataType needs to inferred from scope
	return WGPUVariable(sym, Any, Generic, nothing, )
end

function inferRange(scope, expr::Expr)
	if @capture(expr, a_:b_)
		start = inferExpr(scope, a)
		inferScope!(scope, start)
		stop = inferExpr(scope, b)
		inferScope!(scope, stop)
		step = inferExpr(scope, 1)
		inferScope!(scope, step)
		return RangeExpr(start, stop, step)
	elseif @capture(expr, a_:b_:c_)
		start = inferExpr(scope, a)
		inferScope!(scope, start)
		stop = inferExpr(scope, b)
		inferScope!(scope, stop)
		step = inferExpr(scope, c)
		inferScope!(scope, step)
		return RangeExpr(start, stop, step)
	end
end

function inferExpr(scope::Scope, a::Symbol)
	if a == :workgroupDims
		return WGPUVariable(a, WorkGroupDims, Dims, nothing, )
	elseif a == :workgroupId
		return WGPUVariable(a, WorkGroupId, Intrinsic, nothing,)
	else
		return WGPUVariable(a, Any, Generic, nothing, )
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
