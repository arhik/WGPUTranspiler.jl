struct AddExpr <: JLExpr
	left::WGPUVariable
	right::WGPUVariable
end

function addExpr(scope::Scope, a::Union{Symbol, Expr}, b::Union{Symbol, Expr})
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return AddExpr(lOperand, rOperand)
end
