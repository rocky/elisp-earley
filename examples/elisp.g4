// -*- Antlr -*-
//
// This grammar is taken from our elisp decompiler
//

exprs ::= exprs expr_stmt | expr_stmt

expr_stmt   ::= expr opt_discard
opt_discard ::= DISCARD |

expr  ::= DUP

// Expressions
expr  ::= binary_expr
expr  ::= unary_expr
expr  ::= nullary_op

binary_expr ::= expr expr binary_op

binary_op ::= DIFF
binary_op ::= EQLSIGN

unary_op ::= ADD1
unary_op ::= CAR

nullary_op ::= POINT
