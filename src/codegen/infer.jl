using MacroTools

export inferExpr, @infer

function inferExpr(scope::Scope, expr::Expr)
	if @capture(expr, lhs_ = rhs_)
		return assignExpr(scope, lhs, rhs)
	elseif @capture(expr, a_ + b_)
		return addExpr(scope, a, b)
	elseif @capture(expr, a_ - b_)
		subExpr(scopeCntxt, a, b)
	elseif @capture(expr, a_ * b_)
		mulExpr(scopeCntxt, a, b)
	elseif @capture(expr, a_ / b_)
		divExpr(scopeCntxt, a, b)
	elseif @capture(expr, f_(args__))
		callExpr(scopeCntxt, f, args)
	else
		error("Couldn't capture $expr")
	end
end

function inferVariable(expr::Expr)
	@assert @capture(expr, a_::b_) "This expr : $expr doesn't fit the format a_::b_"
	return WGPUVariable(a, eval(b))
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
