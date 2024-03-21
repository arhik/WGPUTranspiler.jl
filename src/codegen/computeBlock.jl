using MacroTools
using WGSLTypes
using WGSLTypes: wgslfunctions

struct WGPUKernelObject
	func::Function
end

struct WorkGroupDims
	x::UInt32
	y::UInt32
	z::UInt32
end


struct WArray{T, N} # TODO define these properly
end

struct WArrayDims
	x::UInt32
	y::UInt32
	z::UInt32
end

struct ComputeBlock <: JLBlock
	fname::Ref{WGPUVariable}
	fargs::Vector{DeclExpr}
	Targs::Vector{Ref{WGPUVariable}}
	fbody::Vector{JLExpr}
	scope::Union{Nothing, Scope}
	wgSize::NTuple{3, UInt32}
	wgCount::NTuple{3, UInt32}
	builtinArgs::Array{Expr}
end

makeVarPair(p::Pair{Symbol, DataType}) = WGPUVariable(p.first, p.second, Generic, nothing, false, false)

function computeBlock(scope, islaunch, wgSize, wgCount, funcName, funcArgs, fexpr::Expr)
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
    # TODO include these only based on `symbols(funcblock)``
    scope.globals[:workgroupDims] = makeVarPair(:workgroupDims => WorkGroupDims)
    scope.globals[:workgroupId] = makeVarPair(:workgroupId=>WorkGroupId)
    scope.globals[:localId] = makeVarPair(:localId=>LocalInvocationId)
    scope.globals[:globalId] = makeVarPair(:globalId=>GlobalInvocationId)
	scope.globals[:ceil] = makeVarPair(:ceil=>Function)
	
	for wgslf in wgslfunctions
	    scope.globals[wgslf] = makeVarPair(wgslf=>Function)
    end
    
	builtinArgs = [
		:(@builtin(global_invocation_id, globalId::Vec3{UInt32})),
		:(@builtin(local_invocation_id, localId::Vec3{UInt32})),
		:(@builtin(num_workgroups, numWorkgroups::Vec3{UInt32})),
		:(@builtin(workgroup_id, workgroupId::Vec3{UInt32})),
	]
	
	for (inArg, symbolArg) in zip(funcArgs, fargs)
		if @capture(symbolArg, iovar_::ioType_{T_, N_})
			#scope.globals[T] = eltype(inArg)
			#scope.globals[N] = Val{ndims(inArg)}
			scope.typeVars[T] = makeVarPair(T=>eltype(inArg))
			scope.typeVars[N] = makeVarPair(N=>Val{ndims(inArg)})
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
				scope.globals[dimsVar] = makeVarPair(dimsVar=>WArrayDims)
			end
		elseif @capture(symbolarg, iovar_::ioType_)
			if eltype(inarg) in [Float32, Int32, UInt32, Bool] # TODO we need to update this
				push!(
					scope.code.args, 
					quote
						@const $iovar::$(eltype(inarg)) = $(Meta.parse((wgslType(inarg))))
					end
				)
				scope.globals[iovar] = makeVarPair(iovar=>Base.eval(ioType))
			else
				scope.globals[iovar] = makeVarPair(iovar=>Val(iovar))
			end
		end
	end

	for (idx, (inarg, symbolarg)) in enumerate(zip(funcArgs, fargs))
		if @capture(symbolarg, iovar_::ioType_{T_, N_})
			# TODO instead of assert we should branch for each case of argument
			@assert ioType == :WgpuArray #"Expecting WgpuArray Type, received $ioType instead"
			arrayLen = reduce(*, size(inarg))
			push!(
				scope.code.args,
				quote
					@var StorageReadWrite 0 $(idx-1) $(iovar)::Array{$(eltype(inarg)), $(arrayLen)}
				end
			)
			# scope.globals[iovar] = iovar
		end
	end

	fn = inferExpr(scope, fname)
	fa = map(_x -> inferExpr(scope, _x), fargs)
	# make fargs to storage read write
	for (i, arg) in enumerate(fa)
		# (arg.sym[]).dataType = typeInfer(scope, fa)
		(arg.sym[]).varType = StorageReadWrite
		(arg.sym[]).varAttr = WGPUVariableAttribute(0, i)
	end
	fb = map(x -> inferExpr(scope, x), fbody)
	ta = map(x -> inferExpr(scope, x), Targs)
	return ComputeBlock(fn, fa, ta, fb, scope, workgroupSize, workgroupCount, builtinArgs)
end


