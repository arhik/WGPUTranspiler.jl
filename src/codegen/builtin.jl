@enum BuiltinType begin
	vertexIndex
	instanceIndex
	position # might collide
	frontFacing
	fragDepth
	localInvocationId
	localInvocationIndex
	globalInvocationId
	workgroupId
	numWorkGroups
	sampleIndex
	sampleMask
end

struct WorkGroupId <: JLBuiltIn
	x::UInt32
	y::UInt32
	z::UInt32
end

struct GlobalInvocationId <: JLBuiltIn
	x::UInt32
	y::UInt32
	z::UInt32
end

struct LocalInvocationId <: JLBuiltIn
	x::UInt32
	y::UInt32
	z::UInt32
end

struct LocalInvocationIndex <: JLBuiltIn
	x::UInt32
	y::UInt32
	z::UInt32
end
