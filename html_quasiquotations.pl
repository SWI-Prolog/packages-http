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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301 USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(html_quasi_quotations, [ html/4 ]).
:- use_module(library(sgml)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(quasi_quotations)).

/** <module> HTML quasi quotations

This  module  implements  quasi  quotations   for  HTML.  Together  with
library(http/html_write),  this  allows  for  inclusion   of  long  HTML
fragments in the Prolog  source  code   while  replacing  attributes and
content with variable that come from the surrounding Prolog clause.

This module is included and re-exported from library(http/html_write).

@see	library(http/js_write) provides quasi quotation for JavaScript.
*/

%%	html(+Content, +Vars, +VarDict, -DOM) is det.
%
%	The predicate html/4 implements  HTML   quasi  quotations. These
%	quotations produce a DOM term that   is suitable for html//1 and
%	other predicates that are declared to   consume this format. The
%	quasi quoter only  accepts  valid,   but  possibly  partial HTML
%	documents. The document *must* begin  with   a  tag.  The quoter
%	replaces attributes or content whose value  is a Prolog variable
%	that appears in the argument list   of  the =html= indicator. If
%	the variable defines content, it must  be the only content. Here
%	is  an  example,  replacing  both  a   content  element  and  an
%	attribute. Note that the document is valid HTML.
%
%	  ==
%	    html({|html(Name, URL)||
%		   <p>Dear <span class="name">Name</span>,
%
%		   <p>You can <a href="URL">download</a> the requested
%		   article now.
%		   |}
%	  ==

:- quasi_quotation_syntax(html).

html(Content, Vars, Dict, DOM) :-
	must_be(list, Dict),
	include(qq_var(Vars), Dict, QQDict),
	with_quasi_quotation_input(
	    Content, In,
	    load_html(In, DOM0,
		      [ max_errors(0),
			syntax_errors(print),
			case_preserving_attributes(true)
		      ])),
	xml_content(QQDict, DOM0, DOM).

qq_var(Vars, _=Var) :- member(V, Vars), V == Var, !.

xml_content(Dict, [Name], [Var]) :-
	atom(Name),
	memberchk(Name=Var, Dict), !.
xml_content(Dict, Content0, Content) :-
	maplist(xml_content_element(Dict), Content0, Content).

xml_content_element(Dict,
		    element(Tag, Attrs0, Content0),
		    element(Tag, Attrs, Content)) :- !,
	maplist(xml_attribute(Dict), Attrs0, Attrs),
	xml_content(Dict, Content0, Content).
xml_content_element(_, Element, Element).

xml_attribute(Dict, Attr=Name, Attr=Var) :-
	memberchk(Name=Var, Dict), !.
xml_attribute(_, Attr, Attr).
