# Following https://www.w3.org/TR/WGSL/#numeric-builtin-functions

function builtin_abs(a::T) where T<:Number
	return abs(a)
end


