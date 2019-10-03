// -*- Antlr -*-
//
// This is a more complete grammar is taken from our elisp decompiler
//

<fn_body> ::= <body> <opt_label> <opt_return>
<fn_body> ::= <body> <opt_come_from> <opt_label> <stacked_return>
<fn_body> ::= <body> <opt_come_from> <opt_label> <expr_return>

// expr_stmt is an <expr> where the value it produces
// might not be needed. List-like things like
// progn or let fall into this category.
// expr_stmt's are maximal in that the instruction after
// the expr_stmt doesn't consume it unless it is consumed
// for control-flow decision.
// For example  "constant" is an "expr', but not an
// "<expr_stmt>" unless it is followed by something like "return" or
// "goto-if-nil", or "discard"
//
<expr_stmt>  ::= <expr> <opt_discard>

// By its very nature of being sequenced
// <exprs> must use a list-like or stmt_expr

<exprs>     ::= <expr_stmt>+
<opt_exprs> ::= <expr_stmt>*

<progn> ::= <body>

<expr_stacked>  ::= DUP
<expr_stacked>  ::= <unary_expr_stacked>
<expr_stacked>  ::= <binary_expr_stacked>
<expr_stacked>  ::= <ternary_expr_stacked>
<expr_stacked>  ::= <set_expr_stacked>
<expr_stacked>  ::= <if_form_stacked>

<expr>  ::= DUP
<expr>  ::= <setq_form>
<expr>  ::= <setq_form_dup>
<expr>  ::= <set_expr>
<expr>  ::= STACK-REF
<expr>  ::= VARREF

// Function related
<expr>  ::= <binary_expr>
<expr>  ::= <binary_expr_stacked>
<expr>  ::= <ternary_expr>
<expr>  ::= <unary_expr>
<expr>  ::= <unary_expr_stacked>
<expr>  ::= <nullary_expr>
<expr>  ::= <name_expr>
<expr>  ::= <pop_expr>

// Control-flow related
<expr>  ::= <if_form>
<expr>  ::= <if_else_form>
<expr>  ::= <when_macro>
<expr>  ::= <cond_form>
<expr>  ::= <or_form>
<expr>  ::= <and_form>
<expr>  ::= <not_expr>
<expr>  ::= <dolist_macro>
<expr>  ::= <dolist_macro_result>
<expr>  ::= <while_form1>
<expr>  ::= <while_form2>
<expr>  ::= <unwind_protect_form>

// Block related
<expr>  ::= <let_form_star>
<expr>  ::= <let_form_stacked>

// Buffer related
<expr>  ::= <save_excursion_form>
<expr>  ::= <save_current_buffer_form>
<expr>  ::= <with_current_buffer_macro>
<expr>  ::= <with_current_buffer_safe_macro>
<expr>  ::= <set_buffer>

<body>  ::= <exprs>

<body_stacked>  ::= <expr_stacked> <opt_discard> <exprs>
<body_stacked>  ::= <expr_stacked>

<expr> ::= <setq_form_stacked>

<save_excursion_form>      ::= SAVE-EXCURSION <body> UNBIND
<save_current_buffer_form> ::= SAVE-CURRENT-BUFFER <body> UNBIND
<with_current_buffer_macro> ::= SAVE-CURRENT-BUFFER VARREF SET-BUFFER DISCARD <exprs> UNBIND
<with_current_buffer_safe_macro> ::= VARREF NOT GOTO-IF-NOT-NIL-ELSE-POP
                                    CONSTANT VARREF CALL_1
                                    COME_FROM LABEL STACK-ACCESS
                                    NOT GOTO-IF-NIL-ELSE-POP
                                    <with_current_buffer_macro>
                                    <opt_come_from> <opt_label>

<set_buffer>          ::= <expr> SET-BUFFER

// FIXME: Are the STACK-ACCESS and without similar but
// different notions of "expr_stacked"that should to be
// disambiguated?

<unary_expr_stacked>  ::= STACK-ACCESS <unary_op>
<unary_expr_stacked>  ::= <unary_op>
<binary_expr_stacked> ::= <expr> STACK-ACCESS <binary_op>
<binary_expr_stacked> ::= <expr_stacked> <binary_op>


// We keep nonterminals at position 0 and 2
<if_form> ::= <expr> GOTO-IF-NIL <expr> <opt_come_from> <opt_label>
<if_form_stacked> ::= GOTO-IF-NIL <expr> <opt_come_from> <opt_label>

<filler>  ::=
<if_form> ::= <expr> <filler> <expr_stmt> COME_FROM LABEL

// <if_form> ::= <expr> GOTO-IF-NIL-ELSE-POP <expr> LABEL
// <if_form> ::= <expr> GOTO-IF-NIL-ELSE-POP <progn> LABEL
<if_form> ::= <expr> GOTO-IF-NIL <expr>
<if_form> ::= <expr> GOTO-IF-NOT-NIL <expr> <opt_come_from> <opt_label>

<while_form1> ::= <expr> COME_FROM LABEL <expr>
                GOTO-IF-NIL-ELSE-POP <body>
                GOTO <come_froms> LABEL

<while_form1> ::= <expr> COME_FROM LABEL <expr>
                GOTO-IF-NIL <body>
                GOTO <come_froms> LABEL

<while_form2> ::= COME_FROM LABEL <expr>
                GOTO-IF-NIL-ELSE-POP <body>
                GOTO <come_froms> LABEL

<while_form2> ::= COME_FROM LABEL <expr>
                GOTO-IF-NIL <body>
                GOTO <come_froms> LABEL

<when_macro> ::= <expr> GOTO-IF-NIL <body> <come_froms> LABEL
<when_macro> ::= <expr> GOTO-IF-NIL-ELSE-POP <body> <come_froms> LABEL


<unwind_protect_form> ::= <expr> UNWIND-PROTECT <opt_exprs>

// Note: the VARSET's have special names which we could
// check in a reduce rule.
<dolist_macro> ::= <dolist_list> <dolist_init_var>
                GOTO-IF-NIL-ELSE-POP COME_FROM LABEL
                <dolist_loop_iter_set> <body>
                DUP VARSET GOTO-IF-NOT-NIL
                CONSTANT COME_FROM LABEL
                UNBIND

<dolist_macro> ::= <dolist_list> <dolist_init_var>
                GOTO-IF-NIL COME_FROM LABEL
                <dolist_loop_iter_set> <body>
                DUP VARSET GOTO-IF-NOT-NIL
                COME_FROM LABEL
                UNBIND

<dolist_macro> ::= <dolist_list> <dolist_init_var>
                GOTO-IF-NIL-ELSE-POP COME_FROM LABEL
                <dolist_loop_iter_set_stacking> <body_stacked>
                DUP VARSET GOTO-IF-NOT-NIL
                CONSTANT COME_FROM LABEL
                UNBIND


<dolist_macro_result> ::= <dolist_list> <dolist_init_var>
                GOTO-IF-NIL COME_FROM LABEL
                <dolist_loop_iter_set> <body>
                VARREF CDR DUP VARSET GOTO-IF-NOT-NIL
                COME_FROM LABEL CONSTANT VARSET <expr>
                UNBIND

<dolist_loop_iter_set> ::= VARREF CAR VARSET
<dolist_loop_iter_set_stacking> ::= VARREF CAR DUP VARSET
<dolist_init_var>      ::= <varbind> DUP VARBIND
<dolist_list>          ::= <expr>


// <if_else_form> ::= <expr> GOTO-IF-NIL <expr> RETURN LABEL
// <if_else_form> ::= <expr_stacked> GOTO-IF-NIL <progn> RETURN LABEL

<if_else_form> ::= <expr> GOTO-IF-NIL-ELSE-POP <expr_stmt> RETURN
<if_else_form> ::= <expr> GOTO-IF-NIL <expr> GOTO <come_froms> LABEL <expr>
                         <opt_come_from> <opt_label>

// <if_else_form> ::= <expr> GOTO-IF-NIL-ELSE-POP <expr_stmt> GOTO-IF-NIL

// Keep nonterminals at positions  0 and 2
<or_form>    ::= <expr> GOTO-IF-NOT-NIL-ELSE-POP <expr> <opt_come_from> <opt_label>
<or_form>    ::= <expr> GOTO-IF-NOT-NIL          <expr> GOTO-IF-NIL-ELSE-POP COME_FROM LABEL
<or_form>    ::= <expr> GOTO-IF-NOT-NIL <expr>

// "<not_expr>" is (not expr) or (null expr). We use
// not_ instead of null_ to to avoid confusion with nil
<not_expr>   ::= <expr> GOTO-IF-NOT-NIL

<and_form>   ::= <expr> GOTO-IF-NIL-ELSE-POP <expr> <opt_come_from> <opt_label>
<and_form>   ::= <expr> GOTO-IF-NIL          <expr> <opt_come_from> <opt_label>

<expr_or_stacked> ::= <expr>
<expr_or_stacked> ::= STACK-ACCESS

<expr>       ::= <call_exprn>
<expr>       ::= <call_expr0>
<expr>       ::= <call_expr1>
<expr>       ::= <call_expr2>
<expr>       ::= <call_expr3>
<expr>       ::= <call_expr4>
<expr>       ::= <call_expr5>
<call_expr0> ::= <name_expr> CALL_0
<call_expr1> ::= <name_expr> <expr_or_stacked> CALL_1
<call_expr2> ::= <name_expr> <expr_or_stacked> <expr_or_stacked> CALL_2
<call_expr3> ::= <name_expr> <expr_or_stacked> <expr_or_stacked> <expr_or_stacked> CALL_3
<call_expr4> ::= <name_expr> <expr_or_stacked> <expr_or_stacked> <expr_or_stacked>
                 <expr_or_stacked> CALL_4
<call_expr5> ::= <name_expr> <expr_or_stacked> <expr_or_stacked> <expr_or_stacked>
                 <expr_or_stacked> <expr_or_stacked> CALL_5

<name_expr> ::= CONSTANT

<expr_stacking> ::=> <setq_form_stacking> <binary_op>

<binary_expr> ::= <expr> <expr> <binary_op>
<binary_expr> ::= <expr_stacking> <binary_op>
<binary_expr_stacked>  ::= STACK-ACCESS <expr> <binary_op>

<binary_op> ::= CONS
<binary_op> ::= DIFF
<binary_op> ::= ELT
<binary_op> ::= EQLSIGN
<binary_op> ::= EQ
<binary_op> ::= EQUAL
<binary_op> ::= GEQ
<binary_op> ::= GET
<binary_op> ::= GTR
<binary_op> ::= LEQ
<binary_op> ::= LSS
<binary_op> ::= MAX
<binary_op> ::= MIN
<binary_op> ::= MULT
<binary_op> ::= NCONC
<binary_op> ::= PLUS
<binary_op> ::= QUO
<binary_op> ::= REM
<binary_op> ::= STRING=
<binary_op> ::= TIMES

<ternary_expr> ::= <expr> <expr> <expr> <ternary_op>
<ternary_expr_stacked>  ::= STACK-ACCESS <expr> <expr> <ternary_op>
<ternary_expr_stacked>  ::= <expr> <expr> <ternary_op>
<ternary_op>   ::= ASET
<ternary_op>   ::= SUBSTRING

// A Hack for testing since stacked values not working right
// <binary_op>   ::= SUBSTRING

<unary_expr> ::= <expr> <unary_op>
<unary_expr> ::= STACK-ACCESS <unary_op>

<unary_op> ::= ADD1
<unary_op> ::= CAR
<unary_op> ::= CAR-SAFE
<unary_op> ::= CDR
<unary_op> ::= CDR-SAFE
<unary_op> ::= CONSP
<unary_op> ::= GOTO-CHAR
<unary_op> ::= INSERT
<unary_op> ::= INTEGERP
<unary_op> ::= KEYWORDP
<unary_op> ::= LENGTH
<unary_op> ::= LISTP
<unary_op> ::= NATNUMP
<unary_op> ::= NLISTP
<unary_op> ::= NOT
<unary_op> ::= NUMBERP
<unary_op> ::= NULL
<unary_op> ::= RECORDP
<unary_op> ::= SEQUENCEP
<unary_op> ::= STACK-SET
<unary_op> ::= STRINGP
<unary_op> ::= SUBR-ARITY
<unary_op> ::= SUB1
<unary_op> ::= SUBRP
<unary_op> ::= SYMBOL-FUNCTION
<unary_op> ::= SYMBOL-NAME
<unary_op> ::= SYMBOL-PLIST
<unary_op> ::= SYMBOLP
<unary_op> ::= THREADP
<unary_op> ::= TYPE-OF
<unary_op> ::= USER-PTRP
<unary_op> ::= VECTOR-OR-CHAR-TABLEP
<unary_op> ::= VECTORP

<nullary_expr> ::= <nullary_op>

<nullary_op> ::= BOLP
<nullary_op> ::= CURRENT-BUFFER
<nullary_op> ::= CURRENT-COLUMN
<nullary_op> ::= EOLP
<nullary_op> ::= FOLLOWING-CHAR
<nullary_op> ::= POINT
<nullary_op> ::= POINT-MAX
<nullary_op> ::= POINT-MIN
<nullary_op> ::= PRECEDING-CHAR
<nullary_op> ::= WIDEN

// We could have a checking rule that the VARREF and VARSET refer to the same thing
pop_<expr> ::= VARREF DUP CDR VARSET CAR-SAFE

<setq_form> ::= <expr> VARSET
<setq_form_dup> ::= <expr> DUP VARSET
<setq_form_stacked> ::= <expr_stacked> DUP VARSET
<setq_form_stacking> ::= <expr> DUP VARSET

set_<expr>  ::= <expr> <expr> SET
set_<expr>  ::= <expr> <expr> STACK-SET SET
<set_expr_stacked>  ::= <expr_stacked> <expr> SET


// FIXME: this is probably to permissive
<end_clause> ::= GOTO COME_FROM
<end_clause> ::= RETURN COME_FROM
<end_clause> ::= RETURN
<end_clause> ::= <stacked_return>

<cond_form>  ::= <clause> <labeled_clauses> <come_froms> LABEL
<cond_form>  ::= <clause> <labeled_clauses>

<opt_come_froms> ::= <come_froms>
<opt_come_froms> ::=
<come_froms> ::= COME_FROM <come_froms>
<come_froms> ::=

// We use labeled_clause+ rather than labeled_clause* because
// labeled_clause* wreaks havoc in reductions and gives
// produces things like (cond (t 5)) when what we want is just
// 5. labeled_clause+ won't match (cond (foo bar baz)) where
// there is a single cond clause but we'll handle that as an
// "if" rule, e.g. (if foo (progn bar baz))

<labeled_clauses> ::= <labeled_clause> <labeled_clauses>
<labeled_clauses> ::= <labeled_clause>
<labeled_clauses> ::= <labeled_final_clause>

<labeled_clause>  ::= LABEL <clause>

// The "<opt_come_from> <opt_label>" below reflects the fact that
// <expr> might be a short-circuit expression like "and" or "or"
// which acts like and early false on the GOTO-IF-NIL

<condition>       ::= <expr> GOTO-IF-NIL <opt_come_from> <opt_label>
<condition>       ::= <expr> GOTO-IF-NIL-ELSE-POP <opt_come_from> <opt_label>

<clause>          ::= <condition> <body> <end_clause>

// The final clause of a cond doesn't need a GOTO or a return.
// But it must have a label, and must have several COME_FROMS for
// each of the clauses in the cond.
<labeled_final_clause>    ::= LABEL <condition> <body> <come_froms>

// <clause>          ::= <body> <end_clause>

// cond (t *body*) compiles to no <condition>
// If this is the first clause, then possibly
// no label
<clause>          ::= <opt_label> <body> <end_clause>

<opt_come_from> ::= COME_FROM
<opt_come_from> ::=
<opt_label>     ::= LABEL
<opt_label>     ::=

<let_form_stacked> ::= <varlist_stacked> <body_stacked> UNBIND

<varlist_stacked> ::= <expr> <varlist_stacked_inner> DUP VARBIND
<varlist_stacked_inner> ::= <expr> <varlist_stacked_inner> VARBIND
<varlist_stacked_inner> ::=

<let_form_star> ::= <varlist> <body> UNBIND
// Sometimes the last item in "body" is "UNBIND" so we don't need
// to add it here. We could have a reduce check to ensure this.
<let_form_star> ::= <varlist> <body>
<let_form_star> ::= <varlist_stacked> <body_stacked>
<let_form_star> ::= <varlist_stacked> <body_stacked> UNBIND

<varlist>  ::= <varbind> <varlist>
<varlist>  ::= <varbind>
<varbind>  ::= <expr> VARBIND
<varbind>  ::= <expr> STACK-ACCESS VARBIND

<opt_discard>     ::= DISCARD
<opt_discard>     ::=
<opt_return>      ::= RETURN
<opt_return>      ::=

<stacked_return>  ::= STACK-ACCESS RETURN
<expr_return>     ::= <expr> RETURN
