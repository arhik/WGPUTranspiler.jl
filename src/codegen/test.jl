using Revise
using WGPUTranspiler
using WGPUTranspiler: WorkGroupDims
using WGPUCompute
using CodeTracking
using Chairmarks


scope = Scope(Dict(:b=>Int32, :c=>Int32), Dict(), Dict(), 0, nothing, quote end)
aExpr = inferExpr(scope, :(a::Int32 = b + c))
transpile(scope, aExpr)

scope = Scope(Dict(:b=>Int32, :c=>Int32), Dict(), Dict(), 0, nothing, quote end)
aExpr = inferExpr(scope, :(a = b + c))
transpile(scope, aExpr)
# TODO rerunning transpile(scope, aExpr) will have declexpr for a lik a::Int32 which is a bug

# This should fail because of type mismatches
scope = Scope(Dict(:workgroupDims => WorkGroupDims, :b=>Int32, :c=>Int32), Dict(), Dict(), 0, nothing, quote end)
aExpr = inferExpr(scope, :(a::Int32 = workgroupDims.x))
transpile(scope, aExpr)

scope = Scope(Dict(:workgroupDims => WorkGroupDims, :b=>Int32, :c=>Int32), Dict(), Dict(), 0, nothing, quote end)
aExpr = inferExpr(scope, :(a = workgroupDims.x))
transpile(scope, aExpr)

scope = Scope(Dict(:workgroupDims => WorkGroupDims, :b=>Int32, :c=>Int32), Dict(), Dict(), 0, nothing, quote end)
aExpr = inferExpr(scope, :(a::UInt32 = workgroupDims.x))
transpile(scope, aExpr)

# This should fail variable a is not in scope
scope = Scope(Dict(:b=>Int32, :c=>UInt32), Dict(:(+)=>Function, :g=>Function), Dict(), 0, nothing, quote end)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(a + b + c) + g(2, 3, c)))
transpile(scope, cExpr)

# This should fail too datatypes are different
scope = Scope(Dict(:b=>Int32, :c=>UInt32), Dict(:(+)=>Function, :g=>Function), Dict(), 0, nothing, quote end)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(b + c) + g(2, 3, c)))
transpile(scope, cExpr)

# This should fail too 
scope = Scope(Dict(:b=>UInt32, :c=>UInt32), Dict(:(+)=>Function, :g=>Function), Dict(), 0, nothing, quote end)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(b + c) + g(2.0, 3, c)))
transpile(scope, cExpr)


scope = Scope(Dict(:b => UInt32, :c => UInt32), Dict(:(+)=>Function, :g=>Function), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a::UInt32 = ( b + g(b + c))))
transpile(scope, inferredExpr)

scope = Scope(Dict(:a=>WgpuArray{UInt32, 16}, :d=>Int32, :b=>WgpuArray{UInt32, 16}, :c=>UInt32), 
	Dict(:g=>Function, :(+)=>Function), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a[d] = c + b[1]))
transpile(scope, inferredExpr)

# ----- 
struct B
	b::WgpuArray{Int32, 16}
end

scope = Scope(Dict(:a=>WgpuArray{Int32, 16}, :b=>Int32, :c=>Int32, :g=>B, :(+)=>Function), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a[c] = g.b[1]))
transpile(scope, inferredExpr)

# -----
struct B
	b::WgpuArray{Int32, 16}
end

struct C
	c::WgpuArray{Int32, 16}
end

scope = Scope(Dict(:a=>C, :g=>B), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a.c[1] = g.b[1]))
transpile(scope, inferredExpr)
# -----


scope = Scope(
	Dict(
		:a=>WgpuArray{Float32, 16}, 
		:b=>WgpuArray{Float32, 16},
		:c=>WgpuArray{Float32, 16},
		:d=>WgpuArray{Float32, 16},
	), 
	Dict(:println=>Function,:(+)=>Function), Dict(), 0, nothing, quote end
)

inferredExpr = inferExpr(
	scope, 
	:(for i in 0:1:12
		println(i) 
		a[i] = b[i]
		c[i] = d[i] + c[i] + 1.0
	end)
)
transpile(scope, inferredExpr)

# -----

scope = Scope(
	Dict(
		:a=>WgpuArray{Float32, 16}, 
		:b=>WgpuArray{Float32, 16},
		:c=>WgpuArray{Float32, 16},
		:d=>WgpuArray{Float32, 16},
	), 
	Dict(:println=>Function,:(+)=>Function), Dict(), 0, nothing, quote end
)
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

# -----

scope = Scope(
	Dict(
		:x=>Int32,
		:a=>WgpuArray{Float32, 16}, 
		:b=>WgpuArray{Float32, 16},
		:c=>WgpuArray{Float32, 16},
		:d=>WgpuArray{Float32, 16},
	), 
	Dict(:println=>Function,:(+)=>Function), Dict(), 0, nothing, quote end
)
inferredExpr = inferExpr(
	scope, 
	:( for i in 1:10
		if x > 0
			println(i) 
			a[i] = b[i]
			c[i] = d[i] + c[i] + 1.0
		end
		end
	)
)
transpile(scope, inferredExpr)

# ----
scope = Scope(
	Dict(:d=>WgpuArray{Float32, 16}, :c=>WgpuArray{Float32, 16}), 
	Dict(:g=>WgpuArray{Float32, 16}, :println=>Function, :(+)=>Function), 
	Dict(), 0, nothing, quote end
)
inferredExpr = inferExpr(
	scope, 
	:(function test(a::Float32, b::Float32)
		for i in 1:10
			println(i) 
		end
	end)
)
transpile(scope, inferredExpr)

# ----- 

function cast_kernel(x::WgpuArray{T, N}, out::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	out[gId] = S(ceil(x[gId]))
end	

a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Int32, 4, 4));

scope = Scope(Dict(:out=>WgpuArray{Float32, 16}, :x=>WgpuArray{Float32, 16}), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# -------

function cast_kernel(x::WgpuArray{T, N}, out::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	gId = gId
	out[gId] = S(ceil(x[gId]))
end	

a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(Dict(:out=>WgpuArray{Float32, 16}, :x=>WgpuArray{Float32, 16}), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# ---------

function cast_kernel(x::WgpuArray{T, N}, out::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	for i in 1:19
		for j in 1:20
			a[i][j] = 1.0
		end
	end
end	


a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(Dict(:a=>WgpuArray{Float32, 16}, :x=>WgpuArray{Float32, 16}), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# ----------
