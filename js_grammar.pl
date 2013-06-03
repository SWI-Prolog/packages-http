/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(javascript_grammar,
	  [ js_token//1,
	    js_program//1
	  ]).
:- use_module(library(record)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).	% syntax_error//1

/** <module> JavaScript grammar

This file provides a complete grammar  for JavaScript (EcmaScript). This
code is using for the quasi quotation syntax =javascript=, as defined in
library(http/js_write).

@tbd	This file is a very incomplete attempt to see whether we can
	use a grammar that does not produce an abstract syntax tree for
	syntax checking, syntax highlighting and limited rewriting if the
	quasi quoted material.  *THIS FILE CHANGE DRASTICALLY*.

@see	http://tomcopeland.blogs.com/EcmaScript.html is used for the
	high-level syntax.
@see	http://www.ecma-international.org/ecma-262/5.1/ is used for
	implementing the tokenization code.
*/

:- op(100, xf, ?).
:- op(100, xf, +).
:- op(100, xf, *).

:- meta_predicate
	?(//,?,?), +(//,?,?), *(//,?,?).

%%	?(Rule).
%%	*(Rule).
%%	+(Rule).
%
%	The common meta-rules.

?(Rule) --> (phrase(Rule) -> [] ; []).
*(Rule) --> (phrase(Rule) -> *(Rule) ; []).
+(Rule) --> phrase(Rule), *(Rule).

?(Rule, Node) -->
	{ extend_body(Rule, RuleEx, N0) },
	(   phrase(RuleEx)
	->  {Node = [N0]}
	;   {Node = []}
	).

*(Rule, Node) -->
	{ extend_body(Rule, RuleEx, N0) },
	(   phrase(RuleEx)
	->  {Node = [N0|T]},
	    *(Rule, T)
	;   {Node = []}
	).

+(Rule, [N0|T]) -->
	{ extend_body(Rule, RuleEx, N0) },
	phrase(RuleEx),
	*(Rule, T).

here(Here, Here, Here).

terminal(Rule, node(Rule, Start, End, [])) -->
	here(Start),
	phrase(Rule),
	here(End).

punct(Punct, node(punct(Punct), Start, End, [])) -->
	here(Start),
	punct(Punct),
	here(End).

keyword(KeyWord, node(keyword(KeyWord), Start, End, [])) -->
	here(Start),
	keyword(KeyWord),
	here(End).

user:portray(node(Type, Start, End, Children)) :-
	diff_list(Start, End, List),
	format('node(~q, "~s", ~p)', [Type, List, Children]).

diff_list(Start, End, List) :-
	Start == End, !,
	List = [].
diff_list([H|Start], End, [H|List]) :-
	diff_list(Start, End, List).


		 /*******************************
		 *	      EXPAND		*
		 *******************************/

:- op(1200, xfx, ::=).

:- record
	node(type, start, end, children).

term_expansion((Head ::= terminal(Body)),
	       (HeadEx --> (ws, here(Start), Body, here(End), ws,
			    {simplify_node(Node0, Node)}))) :- !,
	make_node([ type(Head),
		    start(Start),
		    end(End),
		    children([])
		  ], Node0),
	extend(Head, Node, HeadEx).
term_expansion((Head ::= Body),
	       (HeadEx --> (ws, here(Start), BodyEx, here(End), ws,
			    {simplify_node(Node0, Node)}))) :-
	make_node([ type(Head),
		    start(Start),
		    end(End),
		    children(Children)
		  ], Node0),
	extend(Head, Node, HeadEx),
	extend_body(Body, BodyEx, Children).

extend(Callable, Arg, CallableEx) :-
	Callable =.. List,
	append(List, [Arg], ListEx),
	CallableEx =.. ListEx.

extend_body(Prim, Prim, Prim) :-
	primitive(Prim).
extend_body(Prim, PrimEx, N) :-
	terminal_rule(Prim), !,
	extend(Prim, N, PrimEx).
extend_body((A0,B0), (A,B,{Node=(NA,NB)}), Node) :- !,
	extend_body(A0, A, NA),
	extend_body(B0, B, NB).
extend_body((A0|B0), (A|B), Node) :- !,
	extend_body(A0, A, Node),
	extend_body(B0, B, Node).
extend_body(*(A), *(A, Node), Node).
extend_body(+(A), +(A, Node), Node).
extend_body(?(A), ?(A, Node), Node).
extend_body(NonTerm, NonTermEx, Node) :-
	extend(NonTerm, Node, NonTermEx).

primitive(!).
primitive(ws).
primitive(eof).

terminal_rule(punct(_)).
terminal_rule(keyword(_)).
terminal_rule(terminal(_)).

simplify_node(node(_T,S0,E0,Child), Child) :-
	Child = node(_,S1,E1,_),
	S1 == S0,
	E1 == E0, !,
	writeln(removed).
simplify_node(Node, Node).


		 /*******************************
		 *	        BNF		*
		 *******************************/

% this BNF comes from http://tomcopeland.blogs.com/EcmaScript.html after
% the following modifications
%
%   - CamelCase to underscore mapping
%   - Removing redundant ()
%   - Flag terminals using terminal, keyword//1 or punct//1

primary_expression ::=
	  keyword("this")
	| object_literal
	| punct("("), expression, punct(")")
	| identifier
	| array_literal
	| literal.
literal ::= terminal(literal_token).
identifier ::= terminal(identifier_name).
array_literal ::=
	punct("["),
	(   elision?, punct("]") | element_list, elision, punct("]")
	|   element_list?, punct("]")
	).
element_list ::= elision?, assignment_expression, (elision, assignment_expression)* .
elision ::= punct(",")+ .
object_literal ::= punct("{"), property_name_and_value_list?, punct("}") .
property_name_and_value_list ::= property_name_and_value, (punct(","), property_name_and_value | punct(","))* .
property_name_and_value ::= property_name, punct(":"), assignment_expression.
property_name ::=
	( identifier
	| terminal(string_literal)
	| terminal(decimal_literal)
	).
member_expression ::=
	((function_expression | primary_expression), member_expression_part*)
	| allocation_expression .
member_expression_for_in ::=
	((function_expression | primary_expression), member_expression_part*).
allocation_expression ::= (keyword("new"), member_expression, ((arguments, member_expression_part*)*)).
member_expression_part ::=
	  (punct("["), expression, punct("]"))
	| (punct("."), identifier).
call_expression ::= member_expression, arguments, call_expression_part* .
call_expression_for_in ::= member_expression_for_in, arguments, call_expression_part* .
call_expression_part ::=
	  arguments
	| (punct("["), expression, punct("]"))
	| (punct("."), identifier).
arguments ::= punct("("), argument_list?, punct(")") .
argument_list ::= assignment_expression, (punct(","),  assignment_expression)* .
left_hand_side_expression ::=
	  call_expression
	| member_expression.
left_hand_side_expression_for_in ::=
	  call_expression_for_in
	| member_expression_for_in.
postfix_expression ::= left_hand_side_expression, postfix_operator? .
postfix_operator ::= (punct("++") | punct("--")).
unary_expression ::= (postfix_expression | (unary_operator, unary_expression)+) .
unary_operator ::= (keyword("delete") | keyword("void") | keyword("typeof") | punct("++") | punct("--") | punct("+") | punct("-") | punct("~") | punct("!")).
multiplicative_expression ::= unary_expression, (multiplicative_operator, unary_expression)* .
multiplicative_operator ::= (punct("*") | slash | punct("%")) .
additive_expression ::= multiplicative_expression, (additive_operator, multiplicative_expression)* .
additive_operator ::= (punct("+") | punct("-")) .
shift_expression ::= additive_expression, (shift_operator, additive_expression)* .
shift_operator ::= (punct("<<") | punct(">>") | punct(">>>")).
relational_expression ::= shift_expression, (relational_operator, shift_expression)* .
relational_operator ::= (punct("<") | punct(">") | punct("<=") | punct(">=") | keyword("instanceof") | keyword("in")) .
relational_expression_no_in ::= shift_expression, (relational_no_in_operator, shift_expression)* .
relational_no_in_operator ::= (punct("<") | punct(">") | punct("<=") | punct(">=") | keyword("instanceof")).
equality_expression ::= relational_expression, (equality_operator, relational_expression)* .
equality_expression_no_in ::= relational_expression_no_in, (equality_operator, relational_expression_no_in)* .
equality_operator ::= (punct("==") | punct("!=") | punct("===") | punct("!==")) .
bitwise_andexpression ::= equality_expression, (bitwise_andoperator, equality_expression)* .
bitwise_andexpression_no_in ::= equality_expression_no_in, (bitwise_andoperator, equality_expression_no_in)* .
bitwise_andoperator ::= punct("&") .
bitwise_xorexpression ::= bitwise_andexpression, (bitwise_xoroperator, bitwise_andexpression)* .
bitwise_xorexpression_no_in ::= bitwise_andexpression_no_in, (bitwise_xoroperator, bitwise_andexpression_no_in)* .
bitwise_xoroperator ::= punct("^") .
bitwise_orexpression ::= bitwise_xorexpression, (bitwise_oroperator, bitwise_xorexpression)* .
bitwise_orexpression_no_in ::= bitwise_xorexpression_no_in, (bitwise_oroperator, bitwise_xorexpression_no_in)* .
bitwise_oroperator ::= punct("|") .
logical_andexpression ::= bitwise_orexpression, (logical_andoperator, bitwise_orexpression)* .
logical_andexpression_no_in ::= bitwise_orexpression_no_in, (logical_andoperator, bitwise_orexpression_no_in)* .
logical_andoperator ::= punct("&&") .
logical_orexpression ::= logical_andexpression, (logical_oroperator, logical_andexpression)* .
logical_orexpression_no_in ::= logical_andexpression_no_in, (logical_oroperator, logical_andexpression_no_in)* .
logical_oroperator ::= punct("||") .
conditional_expression ::= logical_orexpression, (punct("?"), assignment_expression, punct(":"), assignment_expression)? .
conditional_expression_no_in ::= logical_orexpression_no_in, (punct("?"), assignment_expression, punct(":"), assignment_expression_no_in)? .
assignment_expression ::= (left_hand_side_expression, assignment_operator, assignment_expression | conditional_expression) .
assignment_expression_no_in ::= (left_hand_side_expression, assignment_operator, assignment_expression_no_in | conditional_expression_no_in) .
assignment_operator ::= (punct("=") | punct("*=") | slashassign | punct("%=") | punct("+=") | punct("-=") | punct("<<=") | punct(">>=") | punct(">>>=") | punct("&=") | punct("^=") | punct("|=")) .
expression ::= assignment_expression, (punct(","), assignment_expression)* .
expression_no_in ::= assignment_expression_no_in, (punct(","), assignment_expression_no_in)* .
statement ::= block
	| jscript_var_statement
	| variable_statement
	| empty_statement
	| labelled_statement
	| expression_statement
	| if_statement
	| iteration_statement
	| continue_statement
	| break_statement
	| import_statement
	| return_statement
	| with_statement
	| switch_statement
	| throw_statement
	| try_statement .
block ::= punct("{"), statement_list?, punct("}") .
statement_list ::= statement+ .
variable_statement ::= keyword("var"), variable_declaration_list, punct(";")? .
variable_declaration_list ::= variable_declaration, (punct(","), variable_declaration)* .
variable_declaration_list_no_in ::= variable_declaration_no_in, (punct(","), variable_declaration_no_in)* .
variable_declaration ::= identifier, initialiser? .
variable_declaration_no_in ::= identifier, initialiser_no_in? .
initialiser ::= punct("="), assignment_expression .
initialiser_no_in ::= punct("="), assignment_expression_no_in .
empty_statement ::= punct(";") .
expression_statement ::= expression, punct(";")? .
if_statement ::= keyword("if"), punct("("), expression, punct(")"), statement, (keyword("else"), statement)? .
iteration_statement ::=
	  (keyword("do"), statement, keyword("while"), punct("("), expression, punct(")"), punct(";")?)
	| (keyword("while"), punct("("), expression, punct(")"), statement)
	| (keyword("for"), punct("("), expression_no_in?, punct(";"), expression?, punct(";"), expression?, punct(")"), statement)
	| (keyword("for"), punct("("), keyword("var"), variable_declaration_list, punct(";"), expression?, punct(";"), expression?, punct(")"), statement)
	| (keyword("for"), punct("("), keyword("var"), variable_declaration_no_in, keyword("in"), expression, punct(")"), statement)
	| (keyword("for"), punct("("), left_hand_side_expression_for_in, keyword("in"), expression, punct(")"), statement) .
continue_statement ::= keyword("continue"), identifier?, punct(";")? .
break_statement ::= keyword("break"), identifier?, punct(";")? .
return_statement ::= keyword("return"), expression?, punct(";")? .
with_statement ::= keyword("with"), punct("("), expression, punct(")"), statement .
switch_statement ::= keyword("switch"), punct("("), expression, punct(")"), case_block .
case_block ::= punct("{"), case_clauses?, (punct("}") | default_clause, case_clauses?, punct("}")) .
case_clauses ::= case_clause+ .
case_clause ::= ((keyword("case"), expression, punct(":"))), statement_list? .
default_clause ::= ((keyword("default"), punct(":"))), statement_list? .
labelled_statement ::= identifier, punct(":"), statement .
throw_statement ::= keyword("throw"), expression, punct(";")? .
try_statement ::= keyword("try"), block, ((finally | js_catch, finally?)) .
js_catch ::= keyword("catch"), punct("("), identifier, punct(")"), block .
finally ::= keyword("finally"), block .
function_declaration ::= keyword("function"), identifier, (punct("("), formal_parameter_list?, punct(")")), function_body .
function_expression ::= keyword("function"), identifier?, (punct("("), formal_parameter_list?, punct(")")), function_body .
formal_parameter_list ::= identifier, (punct(","), identifier)* .
function_body ::= punct("{"), source_elements?, punct("}") .
js_program ::= source_elements?, eof .
source_elements ::= source_element+ .
source_element ::=
	(   function_declaration
	|   statement
	).
import_statement ::= keyword("import"), js_name, (punct("."), punct("*"))?, punct(";") .
js_name ::= identifier_name, (punct("."), identifier_name)* .
jscript_var_statement ::= keyword("var"), jscript_var_declaration_list, punct(";")? .
jscript_var_declaration_list ::= jscript_var_declaration, (punct(","), jscript_var_declaration)* .
jscript_var_declaration ::= identifier, punct(":"), identifier_name, initialiser? .
%insert_semi_colon ::= java code .


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

slashassign ::= punct("/=").		% Why is this.  Regex ambiguity?
slash ::= punct("/").

%%	keyword(+String)// is semidet.
%
%	Matches a keyword.

keyword(String) -->
	string(String), !,
	\+ js_id_cont(_).

punct(Punct) -->
	string(Punct), !.

ws --> blank, !, ws.
ws --> comment, !, ws.
ws --> "".

literal_token -->
	token(Type),
	{ literal_token_type(Type) }.

literal_token_type(number).
literal_token_type(string).
literal_token_type(regex).
literal_token_type(identifier(Id)) :-
	literal_identifier(Id).

literal_identifier(true).
literal_identifier(false).
literal_identifier(null).

		 /*******************************
		 *	   TOKENIZATION		*
		 *******************************/

%%	js_token(-TokenType)//
%
%	Matches and classifies the next JavaScript token.

js_token(Type) -->
	token(Type).

%%	token(-Type) is semidet.
%
%	Get the next token from the   input. Fails when encountering the
%	end of the input.
%
%	@error syntax_error(Culprit)

token(comment)	      --> comment, !.
token(string)	      --> string_literal, !.
token(number)	      --> numeric_literal, !.
token(identifier(Id)) --> identifier_name(Id), !.
token(regex)	      --> regex_literal, !.
token(ws)	      --> blank, !, blanks.
token(punct(Char))    --> [Code], { char_code(Char, Code) }.

%%	comment// is semidet.

comment -->
	"/*", !,
	(   string(_), "*/"
	->  []
	;   syntax_error(eof_in_comment)
	).
comment -->
	"//", !,
	(   string(_), eol
	->  []
	;   string(_), eof
	->  []
	).


%%	string_literal// is semidet.
%
%	Matches a string literal

string_literal -->
	"\"", !,
	(   q_codes, "\""
	->  []
	;   syntax_error(eof_in_string)
	).
string_literal -->
	"\'", !,
	(   q_codes, "\'"
	->  []
	;   syntax_error(eof_in_string)
	).


%%	numeric_literal//
%
%	Matches JavaScript notion of a numeric constant

numeric_literal -->
	(   decimal_literal
	->  []
	;   hex_integer
	),
	(   (   decimal_digit
	    ;   js_id_start(_)
	    )
	->  syntax_error(js(illegal_number))
	;   []
	).

decimal_literal -->
	decimal_integer, ".", opt_decimal_digits, opt_exponent.
decimal_literal -->
	".", decimal_digits, opt_exponent.
decimal_literal -->
	decimal_integer,
	opt_exponent.

decimal_integer -->
	"0", !.
decimal_integer -->
	non_zero_digit, opt_decimal_digits.

decimal_digits -->
	decimal_digit, !,
	opt_decimal_digits.

opt_decimal_digits -->
	decimal_digit, !,
	opt_decimal_digits.
opt_decimal_digits -->
	[].

decimal_digit --> [C], { code_type(C, digit) }.
non_zero_digit --> [C], { code_type(C, digit), C \== 0'0 }.

opt_exponent -->
	exponent, !.
opt_exponent -->
	[].

exponent -->
	exponent_indictor,
	signed_integer.

exponent_indictor --> "e", !.
exponent_indictor --> "E".

signed_integer --> "+", !, decimal_digits.
signed_integer --> "-", !, decimal_digits.
signed_integer -->         decimal_digits.

hex_integer --> "0", x, hex_digit, hex_digits.

x --> "x".
x --> "X".


%%	regex_literal// is semidet.
%
%	Matches regex expression /.../flags

regex_literal -->
	"/", regex_body, "/", !, regex_flags.

regex_body -->
	regex_first_char,
	regex_chars.

regex_chars --> regex_char, !, regex_chars.
regex_chars --> [].

regex_first_char -->
	regex_non_terminator(C), !,
	{ \+ memberchk(C, "*\\/[") }.
regex_first_char -->
	regex_backslash_sequence.
regex_first_char -->
	regex_class.

regex_char -->
	regex_non_terminator(C), !,
	{ \+ memberchk(C, "\\/[") }.
regex_char -->
	regex_backslash_sequence.
regex_char -->
	regex_class.

regex_backslash_sequence -->
	"\\", !, regex_non_terminator(_).

regex_class -->
	"[", regex_class_chars, "]".

regex_class_chars --> regex_class_char, !, regex_class_chars.
regex_class_chars --> "".

regex_class_char -->
	regex_non_terminator(C), !,
	{ \+ memberchk(C, "]\\") }.

regex_non_terminator(_) -->
	eol, !, {fail}.
regex_non_terminator(C) -->
	source_char(C).

regex_flags -->
	js_id_conts(_).

source_char(C) -->
	[C].


%%	q_codes//
%
%	Shortest list of quoted characters.

q_codes --> [] ; q_code, q_codes.

q_code --> "\\", !, char_esc.
q_code --> eol, !, {fail}.
q_code --> [_].

char_esc --> single_escape_char, !.
char_esc --> "x", !, hex_digit, hex_digit.
char_esc --> "u", !, hex_digit, hex_digit, hex_digit, hex_digit.
char_esc --> eol, !.

hex_digits --> hex_digit, !, hex_digits.
hex_digits --> [].

hex_digit --> [C], {code_type(C, xdigit(_))}.

single_escape_char --> "'".
single_escape_char --> "\"".
single_escape_char --> "\\".
single_escape_char --> "b".
single_escape_char --> "f".
single_escape_char --> "n".
single_escape_char --> "r".
single_escape_char --> "t".
single_escape_char --> "v".

eol --> "\r\n", !.
eol --> "\n", !.
eol --> "\r".

eof -->
	\+ [_].


%	js_identifier classification. Now  based  on   Prolog.  This  is
%	pretty close, but I'm afraid there are corner cases.

identifier_name -->
	identifier_name(_).

identifier_name(Id) -->
	js_id_start(C0), !,
	js_id_conts(Rest),
	{ atom_codes(Id, [C0|Rest]),
	  (   keyword(Id)
	  ->  fail, syntax_error(reserved(Id))
	  ;   true
	  )
	}.


js_id_start(C) --> [C], {js_id_start(C)}.

js_id_start(C) :- code_type(C, prolog_var_start), !.
js_id_start(C) :- code_type(C, prolog_atom_start), !.
js_id_start(0'$).

js_id_conts([H|T]) --> js_id_cont(H), !, js_id_conts(T).
js_id_conts([]) --> [].

js_id_cont(C) --> [C], {js_id_cont(C)}.

js_id_cont(C) :- code_type(C, prolog_identifier_continue), !.
js_id_cont(0'$) :- !.


keyword(break).				% standard keywords
keyword(do).
keyword(instanceof).
keyword(typeof).
keyword(case).
keyword(else).
keyword(new).
keyword(var).
keyword(catch).
keyword(finally).
keyword(return).
keyword(void).
keyword(continue).
keyword(for).
keyword(switch).
keyword(while).
keyword(debugger).
keyword(function).
keyword(this).
keyword(with).
keyword(default).
keyword(if).
keyword(throw).
keyword(delete).
keyword(in).
keyword(try).

keyword(class).				% reserved keywords
keyword(enum).
keyword(extends).
keyword(super).
keyword(const).
keyword(export).
keyword(import).

keyword(implements).			% future reserved keywords
keyword(let).
keyword(private).
keyword(public).
keyword(yield).
keyword(interface).
keyword(package).
keyword(protected).
keyword(static).
