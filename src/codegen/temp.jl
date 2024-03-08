struct IndexExpr <: JLExpr
	var::Expr
	idx::Expr
end

struct RangeBlock <: JLBlock
	expr::Expr
	range::Union{UnitRange, StepRange}
	scope::Scope
end

function capture(depth, expr)
	scope = Scope([], [], depth)
end

macro forloop(depth, idx, range, blocks)
	@capture(expr, for idx_ in range_ blocks__ end)
	range = Base.eval(range)
	RangeBlock(blocks, range, Scope([], [], depth))
end

