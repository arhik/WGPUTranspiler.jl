# BinaryExpressions
struct BinaryExpr <: BinaryOp
	op::Union{Symbol, Function}
	left::Union{Ref{WGPUVariable}, Scalar, JLExpr}
	right::Union{Ref{WGPUVariable}, Scalar, JLExpr}
end

function binaryOp(
	scope::Scope, 
	op::Union{Symbol, Function}, 
	a::Union{Symbol, Number, Expr}, 
	b::Union{Number, Symbol, Expr};
)	
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return BinaryExpr(op, lOperand, rOperand)
end

typeInfer(scope::Scope, binOp::BinaryExpr) = typeintersect(typeInfer(scope, binOp.left), typeInfer(scope, binOp.right))

function symbol(binOp::BinaryExpr)
	syms = map(symbol, (binOp.left[], binOp.right[])) 
	return syms
end

# Common inferScope! for all binary operations
function inferScope!(scope::Scope, jlexpr::BinaryOp)
	inferScope!(scope, jlexpr.left)
	inferScope!(scope, jlexpr.right)
end

# CallExpression 
struct CallExpr <: JLExpr
	func::Union{Ref{WGPUVariable}, JLExpr}
	args::Vector{Union{Ref{WGPUVariable}, Scalar, JLExpr}}
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

symbol(a::Vector{T}) where T <: Union{WGPUVariable, Scalar, JLExpr} = (map(symbol, a))
symbol(callexpr::CallExpr) = begin
	symbol(callexpr.func)
end

function inferScope!(scope::Scope, jlexpr::CallExpr)
	# We don't have to do anything for now
end

typeInfer(scope::Scope, cExpr::CallExpr) = begin
	# @assert allequal(cExpr.args) "All aguments are expected to be same"
	if symbol(cExpr) in [:Float32, :UInt32, :Int32, ] # TODO update this list
		return eval(symbol(cExpr))
	end
	(found, location, rootScope) = findVar(scope, symbol(cExpr.func))
	if found && location == :typeScope
		tVar = rootScope.typeVars[cExpr.func |> symbol]
		if tVar.dataType <: Number
			return tVar.dataType
		end
	end
	typejoin(map(x -> typeInfer(scope, x), cExpr.args)...)
end

# IndexExpressions
struct IndexExpr <: JLExpr
	sym::Union{Ref{WGPUVariable}, JLExpr}
	idx::Union{Ref{WGPUVariable}, Scalar, JLExpr}
end

symbol(idxExpr::IndexExpr) = symbol(idxExpr.sym)

function indexExpr(scope::Scope, sym::Union{Symbol, Expr}, idx::Union{Symbol, Number, Expr})
	symExpr = inferExpr(scope, sym)
	inferScope!(scope, symExpr)
	idxExpr = inferExpr(scope, idx)
	inferScope!(scope, idxExpr)
	return IndexExpr(symExpr, idxExpr)
end

isMutable(idxExpr::IndexExpr) = isMutable(idxExpr.sym)
setMutable!(idxExpr::IndexExpr, b::Bool) = setMutable!(idxExpr.sym, b)

function inferScope!(scope::Scope, jlexpr::IndexExpr)
	
end

typeInfer(scope::Scope, idxExpr::IndexExpr) = begin
	idx = idxExpr.idx
	# TODO handle scalar cases for indexing ...
	if typeof(idx) == Scalar
		idx = Scalar(idx.element |> UInt32)
	end
	ty = typeInfer(scope, idx)
	@assert ty <: Integer "types do not match $(symbol(idx))::$ty vs UInt32"
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
	inferScope!(scope, aExpr)
	return aExpr
end

function accessExpr(scope::Scope, parent::Expr, field::Symbol)
	parentExpr = inferExpr(scope, parent)
	childExpr = inferExpr(scope, field)
	aExpr = AccessExpr(symExpr, fieldExpr)
	inferScope!(scope, aExpr)
	return aExpr
end

symbol(access::AccessExpr) = symbol(access.sym)

function inferScope!(scope::Scope, jlexpr::AccessExpr)
	#inferScope!(scope, jlexpr.sym)
	#inferScope!(scope, jlexpr.field)
end

typeInfer(scope::Scope, axsExpr::AccessExpr) = fieldtype(typeInfer(scope, axsExpr.sym), symbol(axsExpr.field))

struct TypeExpr <: JLExpr
	sym::Ref{WGPUVariable}
	types::Vector{Ref{WGPUVariable}}
end


function typeExpr(scope, a::Symbol, b::Vector{Any})
	aExpr = inferExpr(scope, a)
	bExpr = map(x -> inferExpr(scope, x), b)
	return TypeExpr(aExpr, bExpr)
end

symbol(tExpr::TypeExpr) = (symbol(tExpr.sym), map(x -> symbol(x), tExpr.types)...)

typeInfer(scope::Scope, tExpr::TypeExpr) = begin
	if symbol(tExpr)[1] == :WgpuArray # Hardcoded
		return typeInfer(scope, tExpr, Val(symbol(tExpr)[1]))
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
	if found && location == :typeScope
		b = rootScope.typeVars[b].dataType
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

symbol(decl::DeclExpr) = symbol(decl.sym[])

typeInfer(scope::Scope, declexpr::DeclExpr) = begin
	sym = symbol(declexpr)
	(found, location, rootScope) = findVar(scope, sym)
	if found == false && location != :typeScope
		scope.locals[sym] = declexpr.sym
		var = scope.locals[sym]
		setproperty!(var[], :dataType, declexpr.dataType)
	else found == true && scope.depth == rootScope.depth
		error("duplicate declaration of a variable $sym is not allowed in wgsl though julia allows it")
	end
	typeInfer(scope, declexpr.sym)
end

