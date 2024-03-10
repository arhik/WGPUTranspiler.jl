export transpile 

transpile(scope, s::Scalar) = s.element
transpile(scope, var::WGPUVariable) = var.sym
transpile(scope, lhs::LHS) = transpile(scope, lhs.variable)
transpile(scope, rhs::RHS) = transpile(scope, lhs.variable)
function transpile(scope, a::AssignmentExpr)
	
end
