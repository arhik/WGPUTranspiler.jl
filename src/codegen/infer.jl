using MacroTools

export inferExpr, @infer

function inferExpr(scope::Scope, expr::Expr)
	scopeCntxt = scope
	code = scope.code
	if @capture(expr, lhs_ = rhs_)
		push!(code.args, assignExpr(scopeCntxt, lhs, rhs))
	elseif @capture(expr, a_ + b_)
		addExpr(scopeCntxt, a, b)
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
	if @capture(expr, a_::b_)
		return WGPUVariable(a, eval(b))
	elseif @capture(expr, a_)
		return WGPUVariable(a, Any)
	else
		error("This variable is currently not handled")
	end
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
