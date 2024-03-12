function kernelFunc(funcExpr)
	if 	@capture(funcExpr, function fname_(fargs__) where Targs__ fbody__ end)
		kernelfunc = quote
			function $fname(args::Tuple{WgpuArray}, workgroupSizes, workgroupCount)
				$preparePipeline($(funcExpr), args...)
				$compute($(funcExpr), args...; workgroupSizes=workgroupSizes, workgroupCount=workgroupCount)
				return nothing
			end
		end |> unblock
		return esc(quote $kernelfunc end)
	else
		error("Couldnt capture function")
	end
end

function getFunctionBlock(func, args)
	fString = CodeTracking.definition(String, which(func, args))
	return Meta.parse(fString |> first)
end

function wgpuCall(kernelObj::WGPUKernelObject, args...)
	kernelObj.kernelFunc(args...)
end

macro wgpukernel(launch, wgSize, wgCount, ex)
	code = quote end
	@gensym f_var kernel_f kernel_args kernel_tt kernel
	if @capture(ex, fname_(fargs__))
		(vars, var_exprs) = assign_args!(code, fargs)
		push!(code.args, quote
				$kernel_args = ($(var_exprs...),)
				$kernel_tt = Tuple{map(Core.Typeof, $kernel_args)...}
				kernel = function wgpuKernel(args...)
					$preparePipeline($fname, args...; workgroupSizes=$wgSize, workgroupCount=$wgCount)
					$compute($fname, args...; workgroupSizes=$wgSize, workgroupCount=$wgCount)
				end
				if $launch == true
					wgpuCall(WGPUKernelObject(kernel), $(kernel_args)...)
				else
					WGPUKernelObject(kernel)
				end
			end
		)
	end
	esc(code)
end
