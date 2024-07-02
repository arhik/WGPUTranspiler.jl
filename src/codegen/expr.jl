# BinaryExpressions
struct BinaryExpr <: BinaryOp
	op::Union{Symbol, Function}
	left::Union{Ref{WGPUVariable}, Scalar, JLExpr}
	right::Union{Ref{WGPUVariable}, Scalar, JLExpr}
end

function binaryOp(
	scope::Scope,
	op::Union{Symbol, Function},
	loperand::Union{Symbol, Number, Expr},
	roperand::Union{Number, Symbol, Expr};
)
	lOperand = inferExpr(scope, loperand)
	rOperand = inferExpr(scope, roperand)
	return BinaryExpr(op, lOperand, rOperand)
end

typeInfer(scope::Scope, binOp::BinaryExpr) = typeintersect(typeInfer(scope, binOp.left), typeInfer(scope, binOp.right))

function symbols(binOp::BinaryExpr)
    syms = union(Set(), Set((binOp.op,)), map(symbols, (binOp.left, binOp.right))...)
	return syms
end

symbols(s::Set, binOp::BinaryExpr) = union(
        s,
        symbols(Set(), binOp.op),
        symbols(Set(), binOp.left),
        symbols(Set(), binOp.right)
)

# CallExpression
struct CallExpr <: JLExpr
	func::Union{Ref{WGPUVariable}, JLExpr}
	args::Vector{Union{Ref{WGPUVariable}, Scalar, JLExpr}}
end

function callExpr(scope::Scope, f::Union{Symbol, Expr}, args::Vector{Any})
	func = inferExpr(scope, f)
	arguments = []
	for arg in args
		argument = inferExpr(scope, arg)
		push!(arguments, argument)
	end
	return CallExpr(func, arguments)
end

function symbols(a::Vector{T}) where T <: Union{Ref{WGPUVariable}, Scalar, JLExpr}
    syms = union(Set(), map(symbols, a))
    return syms
end

symbols(s::Set, a::Vector{T}) where T <: Union{Ref{WGPUVariable}, Scalar, JLExpr} = union(s, map(x->symbols(Set(), x), a)...)

function symbols(callexpr::CallExpr)
    argsyms = union(Set(), symbols(callexpr.args))
    union(Set(), symbols(callexpr.func), argsyms)
end

symbols(s::Set, callexpr::CallExpr) = union(s, symbols(Set(), callexpr.func), symbols(Set(), callexpr.args))

function typeInfer(scope::Scope, cExpr::CallExpr)
	# @assert allequal(cExpr.args) "All aguments are expected to be same"
	csyms = symbols(Set(), cExpr)
	csym = csyms |> first
	(found, location, rootScope) = findVar(scope, csym)
	if found && location == :typesym
		tVar = rootScope.typeVars[csym]
		if tVar[].dataType <: Number
			return tVar[].dataType
		end
	elseif found && location == :modulesym # TODO update this list
		# If call function is cast function force type output
       	if csym in (:Float32, :UInt32, :Int32) # TODO update this list
      		return eval(csym)
        elseif scope.moduleVars[][cExpr.func |> symbols |> first][].dataType == Function
			return typejoin(map(x -> typeInfer(scope, x), cExpr.args)...)
		else
			@error "typeInfer $(csym)($(csyms[2:end])) failed!!!"
		end
    elseif found && location == :localsym
        return typejoin(map(x -> typeInfer(scope, x), cExpr.args)...)
    else
        @error "Type inference failed for callExpr $csym !!!!"
    end
end

# IndexExpressions
struct IndexExpr <: JLExpr
	sym::Union{Ref{WGPUVariable}, JLExpr}
	idx::Union{Ref{WGPUVariable}, Scalar, JLExpr}
end

symbols(idxExpr::IndexExpr) = symbols(idxExpr.sym)
symbols(s::Set, idxExpr::IndexExpr) = union(s, symbols(Set(), idxExpr.sym))

function indexExpr(scope::Scope, sym::Union{Symbol, Expr}, idx::Union{Symbol, Number, Expr})
	symExpr = inferExpr(scope, sym)
	idxExpr = inferExpr(scope, idx)
	return IndexExpr(symExpr, idxExpr)
end

isMutable(idxExpr::IndexExpr) = isMutable(idxExpr.sym)
setMutable!(idxExpr::IndexExpr, b::Bool) = setMutable!(idxExpr.sym, b)

function typeInfer(scope::Scope, idxExpr::IndexExpr)
	idx = idxExpr.idx
	# TODO handle scalar cases for indexing ...
	if typeof(idx) == Scalar
		idx = Scalar(idx.element |> UInt32)
	end
	ty = typeInfer(scope, idx)
	@assert ty <: Integer "types do not match $(symbols(idx))::$ty vs UInt32"
	# TODO we might have to deal with multi-indexing
	return eltype(typeInfer(scope, idxExpr.sym))
end


# AccessorExpression
struct AccessExpr<: JLExpr
	sym::Union{Ref{WGPUVariable}, JLExpr}
	field::Union{Ref{WGPUVariable}, JLExpr}
end

isMutable(axsExpr::AccessExpr) = isMutable(axsExpr.sym)
setMutable!(axsExpr::AccessExpr, b::Bool) = setMutable!(axsExpr.sym, b)

function accessExpr(scope::Scope, sym::Symbol, field::Symbol)
	symExpr = inferExpr(scope, sym)
	fieldExpr = inferExpr(scope, field)
	aExpr = AccessExpr(symExpr, fieldExpr)
	return aExpr
end

function accessExpr(scope::Scope, parent::Expr, field::Symbol)
	parentExpr = inferExpr(scope, parent)
	childExpr = inferExpr(scope, field)
	aExpr = AccessExpr(symExpr, fieldExpr)
	return aExpr
end

symbols(access::AccessExpr) = symbols(access.sym)
symbols(s::Set, access::AccessExpr) = union(s, symbols(Set(), access.sym))

typeInfer(scope::Scope, axsExpr::AccessExpr) = fieldtype(typeInfer(scope, axsExpr.sym), symbols(axsExpr.field) |> first)

struct TypeExpr <: JLExpr
	sym::Ref{WGPUVariable}
	types::Vector{Ref{WGPUVariable}}
end


function typeExpr(scope, a::Symbol, b::Vector{Any})
	aExpr = inferExpr(scope, a)
	bExpr = map(x -> inferExpr(scope, x), b)
	return TypeExpr(aExpr, bExpr)
end

symbols(tExpr::TypeExpr) = Set((symbols(tExpr.sym), map(symbols, tExpr.types)...))
symbols(s::Set, tExpr::TypeExpr) = union(
        s,
        symbols(s, tExpr.sym),
        map(x->symbols(s, x), tExpr.types)
)

function typeInfer(scope::Scope, tExpr::TypeExpr)
    tsym = symbols(tExpr.sym) |> first
	if (tsym) == :WgpuArray # Hardcoded
		return typeInfer(scope, tExpr, Val(tsym))
	else
		return typeInfer(scope, tExpr.sym)
	end
end

struct DeclExpr <: JLExpr
	sym::Ref{WGPUVariable}
	dataType::Union{DataType, TypeExpr}
end

function declExpr(scope, a::Symbol, b::Symbol)
	(found, location, rootScope) = findVar(scope, a)
	if found && location == :globalScope
		error("Duplication declaration of variable $a")
	end
	(found, location, rootScope) = findVar(scope, b)
	if found && location == :typesym
		b = rootScope.typeVars[b][].dataType
	end
	aExpr = inferExpr(scope, a)
	bExpr = Base.eval(b)
	aExpr[].dataType = bExpr
	return DeclExpr(aExpr, bExpr)
end

function declExpr(scope, a::Symbol, b::Expr)
	(found, location, rootScope) = findVar(scope, a)
	if found && location == :localScope
		error("Duplicate declaration of variable $a")
	end
	bExpr = inferExpr(scope, b)
	bType = typeInfer(scope, bExpr)
	aExpr = inferExpr(scope, a)
	aExpr[].dataType = bType
	return DeclExpr(aExpr, bExpr)
end

isMutable(decl::DeclExpr) = isMutable(decl.sym[])
setMutable!(decl::DeclExpr, b::Bool) = setMutable!(decl.sym[], b)

symbols(decl::DeclExpr) = symbols(decl.sym[])
symbols(s::Set, decl::DeclExpr) = union(s, symbols(s, decl.sym[]))

function typeInfer(scope::Scope, declexpr::DeclExpr)
	sym = symbols(declexpr) |> first
	(found, location, rootScope) = findVar(scope, sym)
	if found == false && location != :typesym
		scope.localVars[sym] = declexpr.sym
		var = scope.localVars[sym]
		setproperty!(var[], :dataType, declexpr.dataType)
	elseif (found == true) && location != :newsym && (scope.depth == rootScope.depth)
		error("duplicate declaration of a variable $sym is not allowed in wgsl though julia allows it")
	end
	typeInfer(scope, declexpr.sym)
end
