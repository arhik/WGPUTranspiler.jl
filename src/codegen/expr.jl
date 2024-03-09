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
	args::Vector{Union{WGPUVariable, Scalar, JLExpr}}
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
	# We don't have to do anything for now
end

# IndexExpressions
struct IndexExpr <: JLExpr
	sym::Union{WGPUVariable, JLExpr}
	idx::Union{WGPUVariable, Scalar, JLExpr}
end

symbol(idxExpr::IndexExpr) = symbol(idxExpr.sym)

function indexExpr(scope::Scope, sym::Union{Symbol, WGPUVariable, Expr}, idx::Union{Symbol, Number, Expr})
	symExpr = inferExpr(scope, sym)
	inferScope!(scope, symExpr)
	idxExpr = inferExpr(scope, idx)
	inferScope!(scope, idxExpr)
	return IndexExpr(symExpr, idxExpr)
end


# AccessorExpression
struct AccessorExpr<: JLExpr
	sym::Union{Symbol, JLExpr}
	field::Union{Symbol, JLExpr}
end


