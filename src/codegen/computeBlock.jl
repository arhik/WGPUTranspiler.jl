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

    push!(scope.code.args, quote 
			@const workgroupDims = Vec3{UInt32}($(UInt32.(workgroupSize)...))
    	end
    )
	builtinArgs = [
		:(@builtin(global_invocation_id, global_id::Vec3{UInt32})),
		:(@builtin(local_invocation_id, local_id::Vec3{UInt32})),
		:(@builtin(num_workgroups, num_workgroups::Vec3{UInt32})),
		:(@builtin(workgroup_id, workgroup_id::Vec3{UInt32})),
	]
	ins = Dict{Symbol, Any}()
	
	for (inArg, symbolArg) in zip(funcArgs, fargs)
		if @capture(symbolArg, iovar_::ioType_{T_, N_})
			ins[T] = eltype(inArg)
			ins[N] = N
		end
	end
	
	for (idx, (inarg, symbolarg)) in enumerate(zip(funcArgs, fargs))
		if @capture(symbolarg, iovar_::ioType_{T_, N_})
		# TODO instead of assert we should branch for each case of argument
			if ioType == :WgpuArray
				dimsVar = Symbol(iovar, :Dims)
				dims = size(inarg)
			    if dims |> length < 3
		    		dims = (dims..., repeat([1,], inner=(3 - length(dims)))...)
		    	end
				push!(
					scope.code.args,
					quote
						@const $dimsVar = Vec3{UInt32}($(UInt32.(dims)...))
					end
				)
				ins[dimsVar] = dimsVar
			end
		elseif @capture(symbolarg, iovar_::ioType_)
			if eltype(inarg) in [Float32, Int32, UInt32, Bool] # TODO we need to update this
				push!(
					scope.code.args, 
					quote
						@const $iovar::$(eltype(inarg)) = $(Meta.parse((wgslType(inarg))))
					end
				)
				ins[iovar] = iovar
			else
				push!(
					scope.locals,
					iovar
				)
			end
		end
	end

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


