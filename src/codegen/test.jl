using Revise
using WGPUTranspiler
using WGPUCompute
using CodeTracking

scope = Scope([:a, :b, :c], [], 0, nothing, quote end)
aExpr = inferExpr(scope, :(a::Int32 = b + c))
transpile(scope, aExpr)

scope = Scope([:a, :b, :c], [:+, :g], 0, nothing, quote end)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(a + b + b + c) + g(2, 3, c)))
transpile(scope, cExpr)

scope = Scope([:a, :b, :c], [:g, :+], 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a::Int32 = (a + b + g(b + c))))
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :+], 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a[d] = c + b[1]))
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :+], 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a[c] = g.b[1]))
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :+], 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a.c[1] = g.b[1]))
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :println, :+], 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(for i in 0:1:12
		println(i) 
		a[i] = b[i]
		c[i] = d[i] + c[i] + 1.0
	end)
)
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :println, :+], 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(for i in 0:1:12
		for j in 1:2:40
			println(i, j) 
			a[j] = b[i]
			c[i] = d[i] + c[i] + 1.0
		end
	end)
)
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :i, :println, :+], 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(if a > 0
		println(i) 
		a[i] = b[i]
		c[i] = d[i] + c[i] + 1.0
	end)
)
transpile(scope, inferredExpr)

scope = Scope([:a, :d, :b, :c], [:g, :i, :println, :+], 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(function test(a::Int32, b::Float32)
		if a > 0
			println(i) 
			a[i] = b[i]
			c[i] = d[i] + c[i] + 1.0
		end
	end)
)
transpile(scope, inferredExpr)


function cast_kernel(x::WgpuArray{T, N}, out::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	out[gId] = S(ceil(x[gId]))
end	


a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope([], [], 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)


