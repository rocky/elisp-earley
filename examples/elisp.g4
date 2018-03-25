// -*- Antlr -*-
//
// This grammar is taken from our elisp decompiler
//

<exprs> ::= <exprs> <expr_stmt> | <expr_stmt>

<expr_stmt>   ::= <expr> | <expr> <DISCARD>

// Expressions

<expr>  ::= <binary_expr>
<expr>  ::= <unary_expr>
<expr>  ::= <nullary_op>

<binary_expr> ::= <expr> <expr> <binary_op>

// Binary operators
<binary_op> ::= <DIFF>
<binary_op> ::= <EQLSIGN>

// Unary operators
<unary_op> ::= <ADD1>
<unary_op> ::= <CAR>


// Nulary operators
<nullary_op> ::= <POINT>
<nullary_op> ::= <DUP>
