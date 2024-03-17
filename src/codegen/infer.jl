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
		return binaryOp(scope, :>=, a, b)
	elseif @capture(expr, a_ += b_)
		return binaryOp(scope, :+=, a, b)
	elseif @capture(expr, a_ -= b_)
		return binaryOp(scope, :-=, a, b)
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

function declExpr(scope, a::Val{:hello}) 
	@error "Not implemented yet"
end

function declExpr(scope, a::Symbol, b::Symbol)
	aExpr = inferExpr(scope, a)
	bExpr = Base.eval(b)
	return DeclExpr(aExpr, bExpr)
end

function typeExpr(scope, a::Symbol, b::Vector{Any})
	aExpr = inferExpr(scope, a)
	bExpr = map(x -> inferExpr(scope, x), b)
	return TypeExpr(aExpr, bExpr)
end

function declExpr(scope, a::Symbol, b::Expr)
	bExpr = inferExpr(scope, b)
	aExpr = inferExpr(scope, a)
	return DeclExpr(aExpr, bExpr)
end


function inferVariable(scope, expr::Expr)
	if @capture(expr, a_::b_{t__})
		var = WGPUVariable(a, eval(b{t...}), Generic, nothing, false, false)
		ref = Ref{WGPUVariable}(var)
		scope.globals[Symbol(:origin_, a)] = var
		for tvar in t
			var = WGPUVariable(tvar, Any, Generic, nothing, false, false)
			ref = Ref{WGPUVariable}(var)
			scope.typeVars[tvar] = var
		end
		# TODO ignored t types for now
		return ref[] # TODO t is ignored
	elseif @capture(expr, a_::b_)
		var = WGPUVariable(a, eval(b), Generic, nothing, false, false)
		ref = Ref{WGPUVariable}(var)
		scope.globals[Symbol(:origin_, a)] = var
		return ref[]
	else
		error("This expression $expr type is not captured yet")
	end
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

function inferExpr(scope::Scope, a::Symbol)
	var = WGPUVariable(a, Any, Generic, nothing, false, false)
	if a == :workgroupDims
		var.dataType = WorkGroupDims
		var.varType = Dims
	elseif a == :workgroupId
		var.dataType = WorkGroupId
		var.varType = Intrinsic
	end
	scope.globals[Symbol(:origin_, a)] = var
	ref = Ref{WGPUVariable}(var)
	return ref[]
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
