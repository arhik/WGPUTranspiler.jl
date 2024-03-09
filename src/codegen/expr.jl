# BinaryExpressions
struct BinaryExpr <: BinaryOp
	op::Union{Symbol, Function}
	left::Union{WGPUVariable, Scalar, JLExpr}
	right::Union{WGPUVariable, Scalar, JLExpr}
end

function binaryOp(scope::Scope, op::Union{Symbol, Function}, a::Union{Symbol, Number, Expr}, b::Union{Number, Symbol, Expr})
	#@assert op in [:+, :-, :/, :*] #TODO other list
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return BinaryExpr(op, lOperand, rOperand)
end

function symbol(binOp::BinaryExpr)
	syms = map(symbol, (binOp.left, binOp.right)) 
	return syms
end

# Common inferScope! for all binary operations
function inferScope!(scope::Scope, jlexpr::BinaryOp)
	syms = symbol(jlexpr)
	for sym in syms
		if sym != nothing
			@assert findVar(scope, sym) "$(jlexpr.left.sym) is not found in this scope"
		end
	end
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

function indexExpr(scope::Scope, sym::Union{Symbol, Expr}, idx::Union{Symbol, Number, Expr})
	symExpr = inferExpr(scope, sym)
	inferScope!(scope, symExpr)
	idxExpr = inferExpr(scope, idx)
	inferScope!(scope, idxExpr)
	return IndexExpr(symExpr, idxExpr)
end

function inferScope!(scope::Scope, jlexpr::IndexExpr)

end

# AccessorExpression
struct AccessExpr<: JLExpr
	sym::Union{WGPUVariable, JLExpr}
	field::Union{WGPUVariable, JLExpr}
end

function accessExpr(scope::Scope, sym::Union{Symbol, Expr}, field::Union{Symbol, Expr})
	symExpr = inferExpr(scope, sym)
	inferScope!(scope, symExpr)
	fieldExpr = inferExpr(scope, field)
	inferScope!(scope, fieldExpr)
	return AccessExpr(symExpr, fieldExpr)
end

symbol(access::AccessExpr) = symbol(access.sym)

function inferScope!(scope::Scope, jlexpr::AccessExpr)
	
end
