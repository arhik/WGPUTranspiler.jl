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

struct WorkGroupCount
    x::UInt32
    y::UInt32
    z::UInt32
end

struct WArray{T, N} end # TODO define these properly

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

function computeBlock(scope, islaunch, wgSize, wgCount, shmem, funcName, funcArgs, fexpr::Expr)
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
    push!(scope.code.args, quote
        @const workgroupCount = Vec3{UInt32}($(UInt32.(workgroupCount)...))
    end)
    # TODO include these only based on `symbols(funcblock)``
    scope.moduleVars[][:workgroupCount] = makeVarPair(:workgroupCount => WorkGroupCount)
    scope.moduleVars[][:workgroupDims] = makeVarPair(:workgroupDims => WorkGroupDims)
    scope.moduleVars[][:workgroupId] = makeVarPair(:workgroupId=>WorkGroupId)
    scope.moduleVars[][:localId] = makeVarPair(:localId=>LocalInvocationId)
    scope.moduleVars[][:globalId] = makeVarPair(:globalId=>GlobalInvocationId)

	for wgslf in wgslfunctions
	    scope.moduleVars[][wgslf] = makeVarPair(wgslf=>Function)
    end

    # Put some builtins
    dataTypes = [:Float32, :UInt32, :Int32, :Bool]
    for dtype in dataTypes
        scope.moduleVars[][dtype] = makeVarPair(dtype=>Function)
    end

    # Put common ops in module scope
    ops = [:(+), :(-), :(*), :(/), :(%)]
    for op in ops
       scope.moduleVars[][op] = makeVarPair(op=>Function)
    end

    scope.moduleVars[][:atomicAdd] = makeVarPair(:atomicAdd=>Function)

	builtinArgs = [
		:(@builtin(global_invocation_id, globalId::Vec3{UInt32})),
		:(@builtin(local_invocation_id, localId::Vec3{UInt32})),
		:(@builtin(num_workgroups, numWorkgroups::Vec3{UInt32})),
		:(@builtin(workgroup_id, workgroupId::Vec3{UInt32})),
	]

	# First consider only typesym
	for (inArg, symbolArg) in zip(funcArgs, fargs)
		if @capture(symbolArg, iovar_::ioType_{T_, N_})
			if getkey(scope.typeVars, T, nothing) == nothing
				scope.typeVars[T] = makeVarPair(T=>eltype(inArg))
			else
				v = getindex(scope.typeVars[T])
				@assert v.dataType == eltype(inArg) "type parameter $T has conflicting types"
			end
			if getkey(scope.typeVars, N, nothing) == nothing
				scope.typeVars[N] = makeVarPair(N=>Val{ndims(inArg)})
			else
				v = getindex(scope.typeVars[N])
				@assert v.dataType == Val{ndims(inArg)} "type parameter $T has conflicting types"
			end
		end
	end

	# Second consider other variables
	for (inArg, symbolArg) in zip(funcArgs, fargs)
		if @capture(symbolArg, iovar_::ioType_{T_, N_})
			# do nothing
		elseif @capture(symbolArg, iovar_::ioType_)
			if ioType == :Function
				scope.typeVars[iovar] = makeVarPair(nameof(inArg)=>Val{nameof(inArg)})
			elseif getkey(scope.typeVars, ioType, nothing) == nothing
				#scope.moduleVars[][iovar] = makeVarPair(iovar=>eval(ioType))
			else
				v = getindex(scope.typeVars[ioType])
				@assert v.dataType == eltype(inArg)
			end
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
				scope.moduleVars[][dimsVar] = makeVarPair(dimsVar=>WArrayDims)
			end
		elseif @capture(symbolarg, iovar_::ioType_)
			if eltype(inarg) <: Number
				push!(
					scope.code.args,
					quote
						@const $iovar::$(eltype(inarg)) = $(wgslType(inarg))
					end
				)
			elseif typeof(inarg) <: Function
				#scope.moduleVars[][iovar] = makeVarPair(iovar=>typeof(inarg))
			else
				scope.moduleVars[][iovar] = makeVarPair(iovar=>Val{iovar})
			end
		end
	end
	bindingCount = 0
	for (idx, (inarg, symbolarg)) in enumerate(zip(funcArgs, fargs))
		if @capture(symbolarg, iovar_::ioType_{T_, N_})
			# TODO instead of assert we should branch for each case of argument
			@assert ioType == :WgpuArray # "Expecting WgpuArray Type, received $ioType instead"
			arrayLen = reduce(*, size(inarg))
			push!(
				scope.code.args,
				quote
					@var StorageReadWrite 0 $(bindingCount) $(iovar)::Array{$(eltype(inarg)), $(arrayLen)}
				end
			)
			bindingCount += 1
			# scope.moduleVars[][iovar] = iovar
		end
	end

	shmem = Base.eval(shmem)

	for (idx, (symbolarg, (inType, inSize))) in enumerate(shmem)
		arrayLen = reduce(*, inSize)
		push!(
			scope.code.args,
			quote
				@var WorkGroup $symbolarg::Array{$(inType), $(arrayLen)}
			end
		)
		scope.moduleVars[][symbolarg] = makeVarPair(symbolarg=>inType)
	end

	# Infer Function Name but promote to module var
	fn = inferExpr(scope, fname)
	scope.moduleVars[][fname] = fn
	delete!(scope.newVars, fname)
	# TODO fname should be module scope var

	# repeat it twice to promote args to local
	fa = map(_x -> inferExpr(scope, _x), fargs)
	fa = map(_x -> inferExpr(scope, _x), fargs)
	# make fargs to storage read write
	bindingCount = 0
	for (i, arg) in enumerate(fa)
		# (arg.sym[]).dataType = typeInfer(scope, fa)
		if arg.sym[].dataType == Function
			continue
		end
		(arg.sym[]).varType = StorageReadWrite
		(arg.sym[]).varAttr = WGPUVariableAttribute(0, bindingCount)
		bindingCount += 1
	end
	fb = map(x -> inferExpr(scope, x), fbody)
	ta = map(x -> inferExpr(scope, x), Targs)
	for b in fb
	   if b isa AtomicExpr
			push!(scope.code.args,
			quote
			    @var WorkGroup $(b.expr |> symbols |> first)::Atomic{$(b.expr.dataType)}
			end
			)
	   end
	end
	return ComputeBlock(fn, fa, ta, fb, scope, workgroupSize, workgroupCount, builtinArgs)
end
