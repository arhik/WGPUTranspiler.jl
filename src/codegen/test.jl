using Revise
using WGPUCompute
using WGPUTranspiler
using WGPUTranspiler: WorkGroupDims, Generic, WGPUVariable
using CodeTracking
using Chairmarks

makeVarPair(p::Pair{Symbol, DataType}) = p.first => WGPUVariable(
	p.first, p.second, Generic, nothing, false, true
)

# ------

scope = Scope(
	Dict(
		makeVarPair(:b=>Int32),
		makeVarPair(:c=>Int32),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a::Int32 = b + c))
#bExpr = inferExpr(scope, :(a::Int32 = b + c))
transpile(scope, aExpr)

# ------
scope = Scope(
	Dict(
		makeVarPair(:b=>Int32),
		makeVarPair(:c=>Int32),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a::Int32 = b + c))
bExpr = inferExpr(scope, :(a = b + c))
transpile(scope, aExpr)
transpile(scope, bExpr)

# ------

scope = Scope(
	Dict(
		makeVarPair(:b=>Int32),
		makeVarPair(:c=>Int32),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a = b + c))
vExpr = inferExpr(scope, :(a = b + c))

transpile(scope, aExpr)
transpile(scope, vExpr)

# ------

scope = Scope(
	Dict(
		makeVarPair(:b=>Int32),
		makeVarPair(:c=>Int32),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a = b + c))
bExpr = inferExpr(scope, :(a = c))
transpile(scope, aExpr)
transpile(scope, bExpr)

# ------

scope = Scope(
	Dict(
		makeVarPair(:b=>Int32),
		makeVarPair(:c=>Int32),
	),
	Dict(),	Dict(), 0, nothing, quote end
)


aExpr = inferExpr(scope, :(a = b + c))
transpile(scope, aExpr)

# ------- 

# TODO rerunning transpile(scope, aExpr) will have declexpr for a lik a::Int32 which is a bug

# This should fail because of type mismatches
scope = Scope(
	Dict(
		makeVarPair(:workgroupDims=>WorkGroupDims),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a::Int32 = workgroupDims.x))
transpile(scope, aExpr)

# ----
scope = Scope(
	Dict(
		makeVarPair(:workgroupDims=>WorkGroupDims),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a::UInt32 = workgroupDims.x))
transpile(scope, aExpr)

# ----
scope = Scope(
	Dict(
		makeVarPair(:workgroupDims=>WorkGroupDims),
	),
	Dict(),	Dict(), 0, nothing, quote end
)

aExpr = inferExpr(scope, :(a::UInt32 = workgroupDims.x))
transpile(scope, aExpr)

# This should fail variable a in rhs is not in scope
scope = Scope(
	Dict(
		makeVarPair(:b=>Int32), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:(+)=>Function),
		makeVarPair(:g=>Function)
	),
	Dict(),
	Dict(), 0, nothing, quote end
)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(a + b + c) + g(2, 3, c)))
transpile(scope, cExpr)

# This should fail too datatypes are different
scope = Scope(
	Dict(
		makeVarPair(:b=>Int32), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:(+)=>Function),
		makeVarPair(:g=>Function)
	),
	Dict(),
	Dict(), 0, nothing, quote end
)
cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(b + c) + g(2, 3, c)))
transpile(scope, cExpr)

# ------
# This should fail too 
scope = Scope(
	Dict(
		makeVarPair(:b=>Int32), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:(+)=>Function),
		makeVarPair(:g=>Function)
	),
	Dict(),
	Dict(), 0, nothing, quote end
)

cExpr = inferredExpr = inferExpr(scope, :(a::Int32 = g(b + c) + g(2.0, 3, c)))
transpile(scope, cExpr)

# ------
scope = Scope(
	Dict(
		makeVarPair(:b=>UInt32), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:(+)=>Function),
		makeVarPair(:g=>Function)
	),
	Dict(),
	Dict(), 0, nothing, quote end
)
inferredExpr = inferExpr(scope, :(a::UInt32 = ( b + g(b + c))))
transpile(scope, inferredExpr)

# ----
scope = Scope(
	Dict(
		makeVarPair(:a=>WgpuArray{UInt32, 16}), 
		makeVarPair(:d=>UInt32), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:b=>WgpuArray{UInt32, 16}), 
		makeVarPair(:(+)=>Function),
		makeVarPair(:g=>Function)
	),
	Dict(),
	Dict(), 0, nothing, quote end
)

inferredExpr = inferExpr(scope, :(a[d] = c + b[1])) #TODO 1 is a scalar Int32 will fail
transpile(scope, inferredExpr)

# ----- 
struct B
	b::WgpuArray{Int32, 16}
end

scope = Scope(
	Dict(
		makeVarPair(:a=>WgpuArray{Int32, 16}), 
		makeVarPair(:c=>UInt32), 
		makeVarPair(:g=>B), 
		makeVarPair(:(+)=>Function)
	), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a[c] = g.b[1]))
transpile(scope, inferredExpr)

# -----
struct B
	b::WgpuArray{Int32, 16}
end

struct C
	c::WgpuArray{Int32, 16}
end

scope = Scope(Dict(
	makeVarPair(:a=>C), 
	makeVarPair(:g=>B)
	), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(scope, :(a.c[1] = g.b[1])) # scalar indexes will fail
transpile(scope, inferredExpr)
# -----

scope = Scope(
	Dict(
		makeVarPair(:a=>WgpuArray{Float32, 16}), 
		makeVarPair(:b=>WgpuArray{Float32, 16}),
		makeVarPair(:c=>WgpuArray{Float32, 16}),
		makeVarPair(:d=>WgpuArray{Float32, 16}),
		makeVarPair(:println=>Function),
		makeVarPair(:(+)=>Function)
	), Dict(), Dict(), 0, nothing, quote end
)

inferredExpr = inferExpr(
	scope, 
	:(for i in 0:1:12
		println(i) 
		a[10 % i] = b[i]
		c[i] = d[i] + c[i] + 1.0
	end)
)
transpile(scope, inferredExpr)

# -----
scope = Scope(
	Dict(
		makeVarPair(:a=>WgpuArray{Float32, 16}), 
		makeVarPair(:b=>WgpuArray{Float32, 16}),
		makeVarPair(:c=>WgpuArray{Float32, 16}),
		makeVarPair(:d=>WgpuArray{Float32, 16}),
		makeVarPair(:println=>Function),
		makeVarPair(:(+)=>Function)
	), Dict(), Dict(), 0, nothing, quote end
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
		#makeVarPair(:x=>Int32), 
		makeVarPair(:a=>WgpuArray{Float32, 16}), 
		makeVarPair(:b=>WgpuArray{Float32, 16}),
		makeVarPair(:c=>WgpuArray{Float32, 16}),
		makeVarPair(:d=>WgpuArray{Float32, 16}),
		makeVarPair(:println=>Function),
		makeVarPair(:(+)=>Function)
	), Dict(), Dict(), 0, nothing, quote end
)

inferredExpr = inferExpr(
	scope, 
	:( for i in 1:10
		if x == 0
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
	Dict(
		makeVarPair(:println=>Function)
	), 
	Dict(), 
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
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(
	Dict(
	), 
	Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) shmem=() $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# -------

function cast_kernel(x::WgpuArray{T, N}, out::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId::UInt32 = xDims.x*gIdy + gIdx
	gId = 1
	out[gId] = S(ceil(x[gId]))
end	

a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(
	Dict(
		#makeVarPair(:out=>WgpuArray{Float32, 16}),
		#makeVarPair(:x=>WgpuArray{Float32, 16})
	),
	Dict(), Dict(), 0, nothing, quote end)

inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) shmem=() $cast_kernel($a, $b))
)

transpile(scope, inferredExpr)

# ---------

function cast_kernel(x::WgpuArray{T, N}, a::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	for i in 1:19
		for j in 1:20
			d = 1.0
			a[i][j] = 1.0
			d += a[i][j]
		end
	end
end	

a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Int32, 4, 4));

scope = Scope(
	Dict(
		#makeVarPair(:a=>WgpuArray{Float32, 16}), 
		#makeVarPair(:x=>WgpuArray{Float32, 16})
	), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSize=(4, 4) workgroupCount=(1, 1) shmem=() $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# ---------

function cast_kernel(x::WgpuArray{T, N}, a::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	for i in 1:19
		for j in 1:20
			d = 10
			a[i][j] = 1
			d = d + 1
		end
	end
end	


a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(
	Dict(
		#makeVarPair(:a=>WgpuArray{Float32, 16}), 
		#makeVarPair(:x=>WgpuArray{Float32, 16})
	), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSizes=(4, 4) workgroupCount=(1, 1) shmem=() $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# ----------
function cast_kernel(x::WgpuArray{T, N}, a::WgpuArray{S, N}) where {T, S, N}
	xdim = workgroupDims.x
	ydim = workgroupDims.y
	gIdx = workgroupId.x*xdim + localId.x
	gIdy = workgroupId.y*ydim + localId.y
	gId = xDims.x*gIdy + gIdx
	for i in 1:19
		for j in 1:20
			d = 10
			if i > 10 && j > 10
				a[i][j] = 1
				d = d + 1
			end
		end
	end
end	


a = WgpuArray(rand(Float32, 4, 4));
b = WgpuArray(rand(Float32, 4, 4));

scope = Scope(
	Dict(
		#makeVarPair(:a=>WgpuArray{Float32, 16}), 
		#makeVarPair(:x=>WgpuArray{Float32, 16})
	), Dict(), Dict(), 0, nothing, quote end)
inferredExpr = inferExpr(
	scope, 
	:(@wgpukernel launch=true workgroupSizes=(4, 4) workgroupCount=(1, 1) shmem=() $cast_kernel($a, $b))
)
transpile(scope, inferredExpr)

# ----------
