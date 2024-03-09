struct Condition <: JLExpr
	cond::Union{WGPUVariable, Scalar, JLExpr}
end

struct IfBlock <: JLExpr
	cond::JLExpr
	block::Vector{JLExpr}
end

function ifBlock(scope::Scope, cond::Expr, block::Vector{Any})
	childScope = Scope([], [], scope.depth + 1, scope, :())
	condExpr = inferExpr(scope, cond)
	inferScope!(scope, condExpr)
	exprArray = JLExpr[]
	for jlexpr in block
		push!(exprArray, inferExpr(childScope, jlexpr))
	end
	return IfBlock(condExpr, exprArray)
end
