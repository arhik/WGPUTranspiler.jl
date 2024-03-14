# BinaryExpressions
struct BinaryExpr <: BinaryOp
	op::Union{Symbol, Function}
	left::Union{WGPUVariable, Scalar, JLExpr}
	right::Union{WGPUVariable, Scalar, JLExpr}
end

function binaryOp(scope::Scope, op::Union{Symbol, Function}, a::Union{Symbol, Number, Expr}, b::Union{Number, Symbol, Expr})
	lOperand = inferExpr(scope, a)
	inferScope!(scope, lOperand)
	rOperand = inferExpr(scope, b)
	inferScope!(scope, rOperand)
	return BinaryExpr(op, lOperand, rOperand)
end

typeInfer(scope::Scope, binOp::BinaryExpr) = typeintersect(typeInfer(scope, binOp.left), typeInfer(scope, binOp.right))

function symbol(binOp::BinaryExpr)
	syms = map(symbol, (binOp.left, binOp.right)) 
	return syms
end

# Common inferScope! for all binary operations
function inferScope!(scope::Scope, jlexpr::BinaryOp)
	inferScope!(scope, jlexpr.left)
	inferScope!(scope, jlexpr.right)
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

symbol(a::Vector{T}) where T <: Union{WGPUVariable, Scalar, JLExpr} = (map(symbol, a))
symbol(callexpr::CallExpr) = (symbol(callexpr.func), symbol(callexpr.args))

function inferScope!(scope::Scope, jlexpr::CallExpr)
	# We don't have to do anything for now
end

typeInfer(scope::Scope, cExpr::CallExpr) = begin
	# @assert allequal(cExpr.args) "All aguments are expected to be same"
	typejoin(map(x -> typeInfer(scope, x), cExpr.args)...)
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

isMutable(idxExpr::IndexExpr) = isMutable(idxExpr.sym)
setMutable!(idxExpr::IndexExpr, b::Bool) = setMutable!(idxExpr.sym, b)
isNew(idxExpr::IndexExpr) = isNew(idxExpr.sym) # TODO this should always be false

function inferScope!(scope::Scope, jlexpr::IndexExpr)
	
end

typeInfer(scope::Scope, idxExpr::IndexExpr) = begin
	@infiltrate
	ty = typeInfer(scope, idxExpr.idx)
	@assert ty == UInt32 "types do not match $ty UInt32"
	# TODO we might have to deal with multi-indexing
	return eltype(typeInfer(scope, idxExpr.sym))
end


# AccessorExpression
struct AccessExpr<: JLExpr
	sym::Union{WGPUVariable, JLExpr}
	field::Union{WGPUVariable, JLExpr}
end

isMutable(axsExpr::AccessExpr) = isMutable(axsExpr.sym)
setMutable!(axsExpr::AccessExpr, b::Bool) = setMutable!(axsExpr.sym, b)
isNew(axsExpr::AccessExpr) = isNew(axsExpr.sym) # TODO this should always be false

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
	sym::WGPUVariable
	types::Vector{WGPUVariable}
end

symbol(tExpr::TypeExpr) = (symbol(tExpr.sym), map(x -> symbol(x), tExpr.types)...)

typeInfer(scope::Scope, typeExpr::TypeExpr) = typeInfer(scope, typeExpr.sym)

struct DeclExpr <: JLExpr
	sym::WGPUVariable
	dataType::Union{DataType, TypeExpr}
end

isMutable(decl::DeclExpr) = isMutable(decl.sym)
setMutable!(decl::DeclExpr, b::Bool) = setMutable!(decl.sym, b)
isNew(decl::DeclExpr) = isNew(decl.sym)
setNew!(decl::DeclExpr, b::Bool) = setNew!(decl.sym, b)

symbol(decl::DeclExpr) = symbol(decl.sym)

typeInfer(scope::Scope, declexpr::DeclExpr) = begin
	sym = symbol(declexpr)
	(found, rootScope) = findVar(scope, sym)
	if found == false
		scope.locals[sym] = declexpr.dataType
	else found == true && scope.depth == rootScope.depth
		error("duplicate declaration of a variable $sym is not allowed in wgsl though julia allows it")
	end
	typeInfer(scope, declexpr.sym)
end
