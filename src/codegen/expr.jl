# AddExpression
struct AddExpr <: JLExpr
	left::Union{WGPUVariable, JLExpr}
	right::Union{WGPUVariable, JLExpr}
end

function addExpr(scope::Scope, a::Union{Symbol, Expr}, b::Union{Symbol, Expr})
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return AddExpr(lOperand, rOperand)
end

function inferScope!(scope::Scope, jlexpr::AddExpr)
	@assert findVar(scope, jlexpr.left.sym) "$(jlexpr.left.sym) is not found in this scope"
	@assert findVar(scope, jlexpr.right.sym) "$(jlexpr.left.sym) is not found in this scope"
end

# CallExpression 
struct CallExpr <: JLExpr
	func::Union{WGPUVariable, JLExpr}
	args::Vector{Union{WGPUVariable, JLExpr}}
end

function callExpr(scope::Scope, f::Union{Symbol, Expr}, args::Vector{Any})
	func = inferExpr(scope, f)
	inferScope!(scope, func)
	arguments = []
	for arg in args
		argument = inferExpr(scope, arg)
		inferScope!(scope, argument)
		push!(arguments, argument)
	end
	return CallExpr(func, arguments)
end
