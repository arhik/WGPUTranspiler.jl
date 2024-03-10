struct Condition <: JLExpr
	cond::Union{WGPUVariable, Scalar, JLExpr}
end

struct IfBlock <: JLBlock
	cond::JLExpr
	block::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function ifBlock(scope::Scope, cond::Expr, block::Vector{Any})
	childScope = Scope([], [], scope.depth + 1, scope, :())
	condExpr = inferExpr(scope, cond)
	inferScope!(scope, condExpr)
	exprArray = JLExpr[]
	for jlexpr in block
		push!(exprArray, inferExpr(childScope, jlexpr))
	end
	return IfBlock(condExpr, exprArray, scope)
end

symbol(iff::IfBlock) = (symbol(iff.cond), map(symbol, iff.block)...)
