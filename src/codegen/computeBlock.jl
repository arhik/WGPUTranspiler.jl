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
	wgSize::NTuple{3, UInt32}
	wgCount::NTuple{3, UInt32}
	builtinArgs::Array{Expr}
end

function computeBlock(scope, islaunch, wgSize, wgCount, funcName, funcArgs)
	fexpr = @code_string(funcName(funcArgs...)) |> Meta.parse |> MacroTools.striplines
	@capture(fexpr, function fname_(fargs__) where Targs__ fbody__ end)
	workgroupSize = Base.eval(wgSize)
    if workgroupSize |> length < 3
    	workgroupSize = (workgroupSize..., repeat([1,], inner=(3 - length(workgroupSize)))...)
    end
	workgroupCount = Base.eval(wgCount)
    if workgroupCount |> length < 3
    	workgroupCount = (workgroupCount..., repeat([1,], inner=(3 - length(workgroupCount)))...)
    end
	builtinArgs = [
		:(@builtin(global_invocation_id, global_id::Vec3{UInt32})),
		:(@builtin(local_invocation_id, local_id::Vec3{UInt32})),
		:(@builtin(num_workgroups, num_workgroups::Vec3{UInt32})),
		:(@builtin(workgroup_id, workgroup_id::Vec3{UInt32})),
	]
	childScope = Scope([Targs...], [:ceil], 0, scope, quote end)
	fn = inferExpr(childScope, fname)
	fa = map(x -> inferExpr(childScope, x), fargs)
	# make fargs to storage read write
	for (i, arg) in enumerate(fa)
		arg.sym.varType = StorageReadWrite
		arg.sym.varAttr = WGPUVariableAttribute(0, i)
	end
	fb = map(x -> inferExpr(childScope, x), fbody)
	ta = map(x -> inferExpr(childScope, x), Targs)
	return ComputeBlock(fn, fa, ta, fb, childScope, workgroupSize, workgroupCount, builtinArgs)
end


