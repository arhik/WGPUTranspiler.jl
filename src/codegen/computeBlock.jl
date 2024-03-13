using WGPUCompute
using MacroTools

struct WGPUKernelObject
	func::Function
end

struct WorkGroupDims
	x::UInt32
	y::UInt32
	z::UInt32
end

struct WArray
	
end

struct WArrayDims
	x::UInt32
	y::UInt32
	z::UInt32
end

struct ComputeBlock <: JLBlock
	fname::WGPUVariable
	fargs::Vector{DeclExpr}
	Targs::Vector{WGPUVariable}
	fbody::Vector{JLExpr}
	scope::Union{Nothing, Scope}
end

function computeBlock(scope, islaunch, wgSize, wgCount, fname, fargs)
	fexpr = @code_string(fname(fargs...)) |> Meta.parse |> MacroTools.striplines
	@info fexpr
	@capture(fexpr, function fname_(fargs__) where Targs__ fbody__ end)
	childScope = Scope([Targs...], [:ceil], 0, scope, quote end)
	fn = inferExpr(childScope, fname)
	fa = map(x -> inferExpr(childScope, x), fargs)
	fb = map(x -> inferExpr(childScope, x), fbody)
	return ComputeBlock(fn, fa, WGPUVariable[], fb, childScope)
end


