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
	elseif @capture(expr, f_(args__))
		return callExpr(scope, f, args)
	elseif @capture(expr, a_[b_])
		return indexExpr(scope, a, b)
	else
		error("Couldn't capture $expr")
	end
end

function inferVariable(scope, expr::Expr)
	if @capture(expr, a_::b_)
		return WGPUVariable(a, eval(b))
	elseif @capture(expr, a_[b_])
		return indexExpr(scope, a, b)
	else
		error("This expression $expr type is not captured yet")
	end
end

function inferVariable(scope, sym::Symbol)
	#TODO DataType needs to inferred from scope
	return WGPUVariable(sym, Any)
end

function inferExpr(scope::Scope, a::Symbol)
	return WGPUVariable(a, Any) #TODO should be infererred from scope
end

function inferExpr(scope::Scope, a::Number)
	return Scalar(a)
end

function inferExpr(scope::Scope, a::WGPUScalarType)
	return Scalar(a)
end

macro infer(cntxt, expr)
	inferExpr(cntxt, expr)
end
