using AbstractTrees

AbstractTrees.children(t::AssignmentExpr) = symbol(t)
AbstractTrees.children(t::JLExpr) = symbol(t)
