Test.@testset "DeclExpr" begin
	scope = Scope(
		Dict(
			makeVarPair(:b=>Int32),
			makeVarPair(:c=>Int32),
		),
		Dict(),	Dict(), 0, nothing, quote end
	)
	
	aExpr = inferExpr(scope, :(a::Int32 = b + c))
	transpile(scope, aExpr)
	
	Test.@test transpile(scope, aExpr) == :(@let a::Int32 = b + c)
end
