
# fn atomicAnd(atomic_ptr: ptr<AS, atomic<T>, read_write>, v: T) -> T

struct AtomicExpr <: JLExpr
	expr::JLExpr
end

function atomicExpr(scope::Scope, expr::Expr)
    declexpr = inferExpr(scope, expr)
   	(var, dType) = (declexpr.sym, declexpr.dataType)
    @assert dType in [UInt32, Int32] "Only atomic datatypes UInt32 or Int32 are supported!!!"
    var[].varType = Atomic
    # TODO module or local. In other words workspace or storage space.
    scope.moduleVars[][var[].sym] = var
    delete!(scope.newVars, var[].sym)
	return AtomicExpr(declexpr)
end
