/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Michiel Hildebrand
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2013, University of Amsterdam
			      VU University Amsterdam

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

:- module(javascript,
	  [ js_script//1,		% +Content

	    js_call//1,			% +Function(Arg..)
	    js_new//2,			% +Id, +Function(+Args)
	    js_expression//1,		% +Expression
	    js_arg_list//1,		% +ListOfExpressions
	    js_arg//1,			% +Arg
	    js_args//1,			% +Args

	    javascript/4		% Quasi Quotation handler
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(quasi_quotations)).
:- use_module(library(dcg/basics)).
:- use_module(js_grammar).

:- html_meta
	js_script(html, ?, ?).

:- quasi_quotation_syntax(javascript).

/** <module> Utilities for including JavaScript

This library is a supplement   to library(http/html_write) for producing
JavaScript fragments. Its main role is  to   be  able to call JavaScript
functions  with  valid  arguments  constructed  from  Prolog  data.  For
example, suppose you want to call a   JavaScript  functions to process a
list of names represented as Prolog atoms.   This  can be done using the
call below, while without this library you   would have to be careful to
properly escape special characters.

    ==
    numbers_script(Names) -->
	html(script(type('text/javascript'),
	     [ \js_call('ProcessNumbers'(Names)
	     ]),
    ==

The accepted arguments are described with js_expression//1.
*/

%%	js_script(+Content)// is det.
%
%	Generate a JavaScript =script= element with the given content.

js_script(Content) -->
	html(script(type('text/javascript'),
		    Content)).


		 /*******************************
		 *	  QUASI QUOTATION	*
		 *******************************/

%%	javascript(+Content, +Vars, +VarDict, -DOM) is det.
%
%	Quasi quotation parser for JavaScript  that allows for embedding
%	Prolog variables to substitude _identifiers_   in the JavaScript
%	snippet. Parameterizing a JavaScript string   is  achieved using
%	the JavaScript `+` operator, which   results in concatenation at
%	the client side.
%
%	  ==
%	      ...,
%	      js_script({|javascript(Id, Config)||
%			  $(document).ready(function() {
%			     $("#"+Id).tagit(Config);
%			   });
%			 |}),
%	      ...
%	  ==
%
%	The current implementation tokenizes the   JavaScript  input and
%	yields syntax errors on unterminated  comments, strings, etc. No
%	further parsing is  implemented,  which   makes  it  possible to
%	produce syntactically incorrect and   partial JavaScript. Future
%	versions are likely to include a  full parser, generating syntax
%	errors.
%
%	The parser produces a  term  `\List`,   which  is  suitable  for
%	js_script//1 and html//1.  Embedded  variables   are  mapped  to
%	`\js_expression(Var)`, while the remaining  text   is  mapped to
%	atoms.
%
%	@tbd	Implement a full JavaScript parser. Users should _not_
%		rely on the ability to generate partial JavaScript
%		snippets.

javascript(Content, Vars, Dict, \Parts) :-
	include(qq_var(Vars), Dict, QQDict),
	phrase_from_quasi_quotation(
	    js(QQDict, Parts),
	    Content).

qq_var(Vars, _=Var) :-
	member(V, Vars),
	V == Var, !.

js(Dict, [Pre, Subst|More]) -->
	here(Here0),
	js_tokens(_),
	here(Here1),
	js_token(identifier(Name)),
	{ memberchk(Name=Var, Dict), !,
	  Subst = \js_expression(Var),
	  diff_to_atom(Here0, Here1, Pre)
	},
	js(Dict, More).
js(_, [Last]) -->
	string(Codes),
	\+ [_], !,
	{ atom_codes(Last, Codes) }.

js_tokens([]) --> [].
js_tokens([H|T]) -->
	js_token(H),
	js_tokens(T).


%	diff_to_atom(+Start, +End, -Atom)
%
%	True when Atom is an atom that represents the characters between
%	Start and End, where End must be in the tail of the list Start.

diff_to_atom(Start, End, Atom) :-
	diff_list(Start, End, List),
	atom_codes(Atom, List).

diff_list(Start, End, List) :-
	Start == End, !,
	List = [].
diff_list([H|Start], End, [H|List]) :-
	diff_list(Start, End, List).

here(Here, Here, Here).


		 /*******************************
		 *     PROLOG --> JAVASCRIPT	*
		 *******************************/

%%	js_call(+Term)// is det.
%
%	Emit a call to a Javascript function.  The Prolog functor is the
%	name of the function. The arguments are converted from Prolog to
%	JavaScript using js_arg_list//1. Please not that Prolog functors can
%	be quoted atom and thus the following is legal:
%
%	    ==
%		...
%		html(script(type('text/javascript'),
%		     [ \js_call('x.y.z'(hello, 42)
%		     ]),
%	    ==

js_call(Term) -->
	{ Term =.. [Function|Args] },
	html(Function), js_arg_list(Args), [';\n'].


%%	js_new(+Id, +Term)// is det.
%
%	Emit a call to a Javascript object declaration. This is the same
%	as:
%
%	    ==
%	    ['var ', Id, ' = new ', \js_call(Term)]
%	    ==


js_new(Id, Term) -->
	{ Term =.. [Function|Args] },
	html(['var ', Id, ' = new ', Function]), js_arg_list(Args), [';\n'].

%%	js_arg_list(+Expressions:list)// is det.
%
%	Write javascript (function) arguments.  This   writes  "(", Arg,
%	..., ")".  See js_expression//1 for valid argument values.


js_arg_list(Args) -->
	['('], js_args(Args), [')'].

js_args([]) -->
	[].
js_args([H|T]) -->
	js_expression(H),
	(   { T == [] }
	->  []
	;   html(', '),
	    js_args(T)
	).

%%	js_expression(+Expression)// is det.
%
%	Emit a single JSON argument.  Expression is one of:
%
%	    $ Variable :
%	    Emitted as Javascript =null=
%	    $ List :
%	    Produces a Javascript list, where each element is processed
%	    by this library.
%	    $ object(Attributes) :
%	    Where Attributes is a Key-Value list where each pair can be
%	    written as Key-Value, Key=Value or Key(Value), accomodating
%	    all common constructs for this used in Prolog.
%	    $ { K:V, ... }
%	    Same as object(Attributes), providing a more JavaScript-like
%	    syntax.  This may be useful if the object appears literally
%	    in the source-code, but is generally less friendlyto produce
%	    as a result from a computation.
%	    $ json(Term) :
%	    Emits a term using json_write/3.
%	    $ @(Atom) :
%	    Emits these constants without quotes.  Normally used for the
%	    symbols =true=, =false= and =null=, but can also be use for
%	    emitting JavaScript symbols (i.e. function- or variable
%	    names).
%	    $ Number :
%	    Emited literally
%	    $ symbol(Atom) :
%	    Synonym for @(Atom).  Deprecated.
%	    $ Atom or String :
%	    Emitted as quoted JavaScript string.

js_expression(Expr) -->
	js_arg(Expr), !.
js_expression(Expr) -->
	{ type_error(js(expression), Expr) }.

%%	js_arg(+Expression)// is semidet.
%
%	Same as js_expression//1, but fails if Expression is invalid,
%	where js_expression//1 raises an error.
%
%	@deprecated	New code should use js_expression//1.

js_arg(H) -->
	{ var(H) }, !,
	[null].
js_arg(object(H)) -->
	{ is_list(H) }, !,
	html([ '{', \js_kv_list(H), '}' ]).
js_arg({}(Attrs)) --> !,
	html([ '{', \js_kv_cslist(Attrs), '}' ]).
js_arg(@(Id)) --> js_identifier(Id).
js_arg(symbol(Id)) --> js_identifier(Id).
js_arg(json(Term)) -->
	{ json_to_string(json(Term), String),
	  debug(json_arg, '~w~n', String)
	},
	[ String ].
js_arg(H) -->
	{ is_list(H) }, !,
	html([ '[', \js_args(H), ']' ]).
js_arg(H) -->
	{ number(H) }, !,
	[H].
js_arg(H) -->
	{ atomic(H), !,
	  js_quoted_string(H, Q)
	},
	[ '"', Q, '"'
	].

js_kv_list([]) --> [].
js_kv_list([H|T]) -->
	(   js_kv(H)
	->  (   { T == [] }
	    ->	[]
	    ;	html(', '),
		js_kv_list(T)
	    )
	;   { type_error(javascript_key_value, H) }
	).

js_kv(Key:Value) --> !,
	js_key(Key), [:], js_expression(Value).
js_kv(Key-Value) --> !,
	js_key(Key), [:], js_expression(Value).
js_kv(Key=Value) --> !,
	js_key(Key), [:], js_expression(Value).
js_kv(Term) -->
	{ compound(Term),
	  Term =.. [Key,Value]
	}, !,
	js_key(Key), [:], js_expression(Value).

js_key(Key) -->
	(   { must_be(atom, Key),
	      js_identifier(Key)
	    }
	->  [Key]
	;   { js_quoted_string(Key, QKey) },
	    html(['\'', QKey, '\''])
	).

js_kv_cslist((A,B)) --> !,
	js_kv(A),
	html(', '),
	js_kv_cslist(B).
js_kv_cslist(A) -->
	js_kv(A).

%%	js_quoted_string(+Raw, -Quoted)
%
%	Quote text for use in JavaScript.  Quoted does _not_ include the
%	leading and trailing quotes.
%
%	@tbd	Join with json stuff.

js_quoted_string(Raw, Quoted) :-
	atom_codes(Raw, Codes),
	phrase(js_quote_codes(Codes), QuotedCodes),
	atom_codes(Quoted, QuotedCodes).

js_quote_codes([]) -->
	[].
js_quote_codes([0'\r,0'\n|T]) --> !,
	"\\n",
	js_quote_codes(T).
js_quote_codes([H|T]) -->
	js_quote_code(H),
	js_quote_codes(T).

js_quote_code(0'') --> !,
	"\\'".
js_quote_code(0'") --> !,
	"\\\"".
js_quote_code(0'\\) --> !,
	"\\\\".
js_quote_code(0'\n) --> !,
	"\\n".
js_quote_code(0'\r) --> !,
	"\\r".
js_quote_code(0'\t) --> !,
	"\\t".
js_quote_code(C) -->
	[C].

%%	js_identifier(+Id:atom)// is det.
%
%	Emit an identifier if it is a valid one

js_identifier(Id) -->
	{ must_be(atom, Id),
	  js_identifier(Id)
	}, !,
	[ Id ].
js_identifier(Id) -->
	{ domain_error(js(identifier), Id)
	}.

%%	js_identifier(+Id:atom) is semidet.
%
%	True if Id is a  valid   identifier.  In traditional JavaScript,
%	this means it starts  with  [$_:letter:]   and  is  followed  by
%	[$_:letter:digit:]

js_identifier(Id) :-
	sub_atom(Id, 0, 1, _, First),
	char_type(First, csymf),
	forall(sub_atom(Id, _, 1, _, Char), char_type(Char, csym)).


%%	json_to_string(+JSONTerm, -String)
%
%	Write JSONTerm to String.

json_to_string(JSON, String) :-
	with_output_to(string(String),
		       json_write(current_output,JSON,[width(0)])).

