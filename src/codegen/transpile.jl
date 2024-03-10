export transpile 

transpile(scope, s::Scalar) = s.element
transpile(scope, var::WGPUVariable) = var.dataType == Any ? :($(var.sym)) : :($(var.sym)::$(var.dataType))
transpile(scope, lhs::LHS) = transpile(scope, lhs.variable, Val(lhs.mutable))
transpile(scope, var::WGPUVariable, ::Val{true}) = :(@var $(transpile(scope, var)))
transpile(scope, var::WGPUVariable, ::Val{false}) = :($(var.sym))
transpile(scope, rhs::RHS) = transpile(scope, rhs.rhsExpr)
transpile(scope, binOp::BinaryOp) = transpile(scope, binOp, Val(binOp.op))

# for each op in [:+, :-, :*, :\, :<, :>, :<=, :>=, :==, :+=, :-=]
transpile(scope, binOp::BinaryOp, op::Val{:+}) = :($(transpile(scope, binOp.left)) + $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:-}) = :($(transpile(scope, binOp.left)) - $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:*}) = :($(transpile(scope, binOp.left)) * $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:/}) = :($(transpile(scope, binOp.left)) / $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:<}) = :($(transpile(scope, binOp.left)) < $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:>}) = :($(transpile(scope, binOp.left)) > $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:(<=)}) = :($(transpile(scope, binOp.left)) <= $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:(>=)}) = :($(transpile(scope, binOp.left)) >= $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:(==)}) = :($(transpile(scope, binOp.left)) == $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:(+=)}) = :($(transpile(scope, binOp.left)) += $(transpile(scope, binOp.right)))
transpile(scope, binOp::BinaryOp, op::Val{:(-=)}) = :($(transpile(scope, binOp.left)) -= $(transpile(scope, binOp.right)))

function transpile(scope, a::AssignmentExpr)
	@infiltrate
	lExpr = transpile(scope, a.lhs)
	rExpr = transpile(scope, a.rhs)
	if @capture(lExpr, @var_(v_))
		return :(@var $v =  $rExpr)
	else
		return :($lExpr =  $rExpr)
	end
end

function transpile(scope, cExpr::CallExpr)
	return Expr(:call, transpile(scope, cExpr.func), map(x -> transpile(scope, x), cExpr.args)...)
end

