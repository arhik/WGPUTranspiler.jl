struct Condition <: JLExpr
	cond::Union{WGPUVariable, Scalar, JLExpr}
end

struct IfBlock <: JLBlock
	cond::JLExpr
	block::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function ifBlock(scope::Scope, cond::Expr, block::Vector{Any})
	childScope = Scope(Dict(), scope.moduleVars[], Dict(), scope.depth + 1, scope, :())
	condExpr = inferExpr(scope, cond)
	exprArray = JLExpr[]
	for jlexpr in block
		push!(exprArray, inferExpr(childScope, jlexpr))
	end
	return IfBlock(condExpr, exprArray, childScope)
end

symbols(iff::IfBlock) = Set((symbols(iff.cond), map(symbols, iff.block)...))
symbols(s::Set, iff::IfBlock) = union(s, symbols(s, iff.cond), map(x->symbols(s, x), iff.block)...)
