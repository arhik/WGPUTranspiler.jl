using AbstractTrees

AbstractTrees.children(t::AssignmentExpr) = symbols(t)
AbstractTrees.children(t::JLExpr) = symbols(t)
