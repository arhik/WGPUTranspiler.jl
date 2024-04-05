
# fn atomicAnd(atomic_ptr: ptr<AS, atomic<T>, read_write>, v: T) -> T

struct WGPUAtomics <: JLExpr
	expr::BinaryOp
end

function atomicExpr(scope::Scope, expr::Expr)
	return WGPUAtomics(inferExpr(scope, expr))
end	
