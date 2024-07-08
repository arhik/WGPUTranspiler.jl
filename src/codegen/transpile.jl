export transpile

transpile(scope::Scope, s::Scalar) = s.element
transpile(scope::Scope, var::WGPUVariable) = begin
	(found, location, rootScope) = findVar(scope, var.sym)
	if location == :typesym
		return :($(getDataTypeFrom(rootScope, location, var.sym)))
	else
		:($(var.sym))
	end
end
transpile(scope::Scope, var::Ref{WGPUVariable}) = transpile(scope, var[])

transpile(scope::Scope, lhs::LHS) = transpile(scope, lhs.expr)
transpile(scope::Scope, rhs::RHS) = transpile(scope, rhs.expr)
transpile(scope::Scope, binOp::BinaryOp) = transpile(scope, binOp, Val(binOp.op))

# for each op in [:+, :-, :*, :\, :<, :>, :<=, :>=, :==, :+=, :-=]
transpile(scope::Scope, binOp::BinaryOp, op::Val{:+}) = :($(transpile(scope, binOp.left)) + $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:-}) = :($(transpile(scope, binOp.left)) - $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:*}) = :($(transpile(scope, binOp.left)) * $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:/}) = :($(transpile(scope, binOp.left)) / $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:<}) = :($(transpile(scope, binOp.left)) < $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:>}) = :($(transpile(scope, binOp.left)) > $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:(<=)}) = :($(transpile(scope, binOp.left)) <= $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:(>=)}) = :($(transpile(scope, binOp.left)) >= $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:(==)}) = :($(transpile(scope, binOp.left)) == $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:(+=)}) = :($(transpile(scope, binOp.left)) += $(transpile(scope, binOp.right)))
transpile(scope::Scope, binOp::BinaryOp, op::Val{:(-=)}) = :($(transpile(scope, binOp.left)) -= $(transpile(scope, binOp.right)))

function transpile(scope::Scope, a::AssignmentExpr)
	lExpr = transpile(scope, a.lhs)
	rExpr = transpile(scope, a.rhs)
	if typeof(a.lhs.expr) == DeclExpr
	  	if isMutable(a.lhs)
			return	:(@var $lExpr = $rExpr)
		else
			return :(@let $lExpr = $rExpr)
		end
	elseif typeof(a.lhs.expr) == Base.RefValue{WGPUVariable}
		if isNew(a.lhs)
			return (isMutable(a.lhs) ? :(@var $lExpr = $rExpr) : :(@let $lExpr = $rExpr))
		else
			return :($lExpr = $rExpr)
		end
	elseif typeof(a.lhs.expr) == IndexExpr
		return :($lExpr = $rExpr)
	elseif typeof(a.lhs.expr) == AccessExpr
		return :($lExpr = $rExpr)
	elseif typeof(a.lhs.expr) <: JLExpr
		error(
			"This is not covered yet"
		)
	else
		error("This shouldn't have been reached. Assignment LHS is $(typeof(a.lhs)) and RHS is $(typeof(a.rhs))")
	end
end

function transpile(scope::Scope, a::AtomicAssignmentExpr)
    lExpr = transpile(scope, a.lhs)
    rExpr = transpile(scope, a.rhs)
    if a.op == :(=)
        return :(atomicStore(@ptr($lExpr), $rExpr))
    elseif a.op == :(+=)
        return :(atomicAdd(@ptr($lExpr), $rExpr))
    elseif a.op == :(-=)
        return :(atomicSub(@ptr($lExpr), $rExpr))
    elseif a.op == :(|=)
        return :(atomicOr(@ptr($lExpr), $rExpr))
    elseif a.op == :(&=)
        return :(atomicAnd(@ptr($lExpr), $rExpr))
    elseif a.op == :(⊻=)
        return :(atomicXor(@ptr($lExpr), $rExpr))
    else
        @error "Not Implemented: This compound assignment $(a.op) is yet to be covered"
    end
end

function transpile(scope::Scope, cExpr::CallExpr)
	return Expr(:call, transpile(scope, cExpr.func), map(x -> transpile(scope, x), cExpr.args)...)
end

function transpile(scope::Scope, idxExpr::IndexExpr)
	return Expr(:ref, transpile(scope, idxExpr.sym), transpile(scope, idxExpr.idx))
end

transpile(scope::Scope, idxExpr::IndexExpr, ::Val{true}) = transpile(scope, idxExpr::IndexExpr)
transpile(scope::Scope, idxExpr::IndexExpr, ::Val{false}) = error("This variable cannot be indexed")

function transpile(scope::Scope, acsExpr::AccessExpr)
	return Expr(:., transpile(scope, acsExpr.sym), QuoteNode(transpile(scope, acsExpr.field)))
end

transpile(scope::Scope, declExpr::DeclExpr) = Expr(:(::),
	map(x -> transpile(scope, x), (declExpr.sym, declExpr.dataType))...
)

transpile(scope::Scope, ::Type{T}) where T = :($T)

transpile(scope::Scope, typeExpr::TypeExpr) = Expr(
	:curly, transpile(scope, typeExpr.sym),
	map(x -> transpile(scope, x), typeExpr.types)...
)

function transpile(scope::Scope, rblock::RangeBlock)
	(start, step, stop) = map(x -> transpile(rblock.scope, x), (rblock.start, rblock.step, rblock.stop))
	range = :($start:$step:$stop)
	block = map(x -> transpile(rblock.scope, x), rblock.block)
	idx = transpile(rblock.scope, rblock.idx)
	return :(@forloop $(Expr(:for, Expr(:(=), idx, range), quote $(block...) end)))
end

function transpile(scope::Scope, ifblock::IfBlock)
	c = transpile(ifblock.scope, ifblock.cond)
	block = map(x -> transpile(scope, x), ifblock.block)
	return :(@escif $(Expr(:if, c, quote $(block...) end)))
end

function transpile(scope::Scope, whileblock::WhileBlock)
    c = transpile(whileblock.scope, whileblock.cond)
    block = map(x -> transpile(scope, x), whileblock.block)
    return :(@whileblock $(Expr(:while, c, quote $(block...) end)))
end

function transpile(scope::Scope, funcblk::FuncBlock)
	fn = transpile(funcblk.scope, funcblk.fname)
	fa = map(x -> transpile(scope, x), funcblk.fargs)
	fb = map(x -> transpile(scope, x), funcblk.fbody)
	return Expr(:function, Expr(:call, fn, fa...), quote $(fb...) end)
end

function transpile(scope::Scope, computeBlk::ComputeBlock)
	fn = transpile(scope::Scope, computeBlk.fname)
	fa = map(x -> transpile(scope, x), computeBlk.fargs)
	fb = map(x -> transpile(scope, x), computeBlk.fbody)
	ta = map(x -> transpile(scope, x), computeBlk.Targs)
	workgroupSize = (computeBlk.wgSize .|> Int)
	code = quote end
	push!(code.args, map(x -> unblock(x), scope.code.args)...)
	bargs = computeBlk.builtinArgs
	fexpr =	Expr(:function, Expr(:call, fn, bargs...), quote $(fb...) end)

	push!(
		code.args,
		:(@compute @workgroupSize($(workgroupSize...)) $(fexpr))
	)
	@info (code |> MacroTools.striplines)
	return code |> MacroTools.striplines
end


function transpile(scope::Scope, atomicExpr::AtomicExpr)
	# TODO cover this for other atomics & better interface
	if atomicExpr.expr isa BinaryExpr
		op = atomicExpr.expr.op
		lExpr = atomicExpr.expr.left
		rExpr = atomicExpr.expr.right
		if op == :+=
			transpiledlExpr = (transpile(scope, lExpr))
			transpiledrExpr = (transpile(scope, rExpr))
			return :(atomicAdd(@ptr($transpiledlExpr), @ptr($transpiledrExpr)))
		end
	elseif atomicExpr.expr isa DeclExpr
	   return :(@atomicdecl $(transpile(scope, atomicExpr.expr)))
	end
end

function transpile(scope::Scope, ca::CompoundAssignExpr)
    lExpr = transpile(scope, ca.lhs)
    rExpr = transpile(scope, ca.rhs)
    return Expr(ca.op, lExpr, rExpr)
end
