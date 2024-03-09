# BinaryExpressions
struct BinaryExpr <: BinaryOp
	op::Union{Symbol, Function}
	left::Union{WGPUVariable, JLExpr}
	right::Union{WGPUVariable, JLExpr}
end

function binaryOp(scope::Scope, op::Union{Symbol, Function}, a::Union{Symbol, Expr}, b::Union{Symbol, Expr})
	@assert op in [:+, :-, :/, :*] #TODO other list
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return BinaryExpr(op, lOperand, rOperand)
end


# Common inferScope! for all binary operations
function inferScope!(scope::Scope, jlexpr::BinaryOp)
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

function inferScope!(scope::Scope, jlexpr::CallExpr)
	# We don't have to anything for now
end
