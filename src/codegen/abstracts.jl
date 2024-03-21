abstract type JLExpr end
abstract type JLVariable <: JLExpr end
abstract type JLBlock <: JLExpr end
abstract type BinaryOp <: JLExpr end
abstract type JLBuiltIn <: JLExpr end
