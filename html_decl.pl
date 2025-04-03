/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(html_decl,
          [ (html_meta)/1,              % +Spec

            op(1150, fx, html_meta)
          ]).
:- autoload(library(apply),[maplist/3,maplist/4]).
:- if(exists_source(library(http/http_dispatch))).
:- autoload(library(http/http_dispatch), [http_location_by_id/2]).
:- endif.

/** <module> HTML emitter analysis and IDE support

This library supports declaring DCG rules   that  process HTML terms. It
supports the cross-referencer as well  as   syntax  highlighting that is
based on library(prolog_colour).
*/


                 /*******************************
                 *     META-PREDICATE SUPPORT   *
                 *******************************/

%!  html_meta(+Heads) is det.
%
%   This directive can be used  to   declare  that an HTML rendering
%   rule takes HTML content as  argument.   It  has  two effects. It
%   emits  the  appropriate  meta_predicate/1    and  instructs  the
%   built-in editor (PceEmacs) to provide   proper colouring for the
%   arguments.  The  arguments  in  Head  are    the   same  as  for
%   meta_predicate or can be constant `html`.  For example:
%
%   ```
%   :- html_meta
%       page(html,html,?,?).
%   ```

html_meta(Spec) :-
    throw(error(context_error(nodirective, html_meta(Spec)), _)).

html_meta_decls(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
html_meta_decls((A,B), (MA,MB), [MH|T]) :-
    !,
    html_meta_decl(A, MA, MH),
    html_meta_decls(B, MB, T).
html_meta_decls(A, MA, [MH]) :-
    html_meta_decl(A, MA, MH).

html_meta_decl(Head, MetaHead,
               html_decl:html_meta_head(GenHead, Module, Head)) :-
    functor(Head, Name, Arity),
    functor(GenHead, Name, Arity),
    prolog_load_context(module, Module),
    Head =.. [Name|HArgs],
    maplist(html_meta_decl, HArgs, MArgs),
    MetaHead =.. [Name|MArgs].

html_meta_decl(html, :) :- !.
html_meta_decl(Meta, Meta).

system:term_expansion((:- html_meta(Heads)),
                      [ (:- meta_predicate(Meta))
                      | MetaHeads
                      ]) :-
    html_meta_decls(Heads, Meta, MetaHeads).

:- multifile
    html_meta_head/3.

html_meta_colours(Head, Goal, built_in-Colours) :-
    Head =.. [_|MArgs],
    Goal =.. [_|Args],
    maplist(meta_colours, MArgs, Args, Colours).

meta_colours(html, HTML, Colours) :-
    !,
    html_colours(HTML, Colours).
meta_colours(I, _, Colours) :-
    integer(I), I>=0,
    !,
    Colours = meta(I).
meta_colours(_, _, classify).

html_meta_called(Head, Goal, Called) :-
    Head =.. [_|MArgs],
    Goal =.. [_|Args],
    meta_called(MArgs, Args, Called, []).

meta_called([], [], Called, Called).
meta_called([html|MT], [A|AT], Called, Tail) :-
    !,
    phrase(called_by(A), Called, Tail1),
    meta_called(MT, AT, Tail1, Tail).
meta_called([0|MT], [A|AT], [A|CT0], CT) :-
    !,
    meta_called(MT, AT, CT0, CT).
meta_called([I|MT], [A|AT], [A+I|CT0], CT) :-
    integer(I), I>0,
    !,
    meta_called(MT, AT, CT0, CT).
meta_called([_|MT], [_|AT], Called, Tail) :-
    !,
    meta_called(MT, AT, Called, Tail).



                 /*******************************
                 *      PCE EMACS SUPPORT       *
                 *******************************/

:- multifile
    prolog_colour:goal_colours/2,
    prolog_colour:style/2,
    prolog_colour:message//1,
    prolog:called_by/2,
    prolog:xref_update_syntax/2.        % +Term, +Module

prolog_colour:goal_colours(Goal, Colours) :-
    (   html_meta_head(Goal, _Module, Head)
    ->  true
    ;   dyn_html_meta_head(Goal, _Module, Head)
    ),
    html_meta_colours(Head, Goal, Colours).
prolog_colour:goal_colours(html_meta(_),
                           built_in-[meta_declarations([html])]).

                                        % TBD: Check with do_expand!
html_colours(Var, classify) :-
    var(Var),
    !.
html_colours(\List, html_raw-[list-Colours]) :-
    is_list(List),
    !,
    list_colours(List, Colours).
html_colours(\_, html_call-[dcg]) :- !.
html_colours(_:Term, built_in-[classify,Colours]) :-
    !,
    html_colours(Term, Colours).
html_colours(&(Entity), functor-[entity(Entity)]) :- !.
html_colours(List, list-ListColours) :-
    List = [_|_],
    !,
    list_colours(List, ListColours).
html_colours(Var=Spec, functor-[classify,SpecColors]) :-
    var(Var),
    !,
    html_colours(Spec, SpecColors).
html_colours(Format-Args, functor-[FormatColor,ArgsColors]) :-
    !,
    format_colours(Format, FormatColor),
    format_arg_colours(Args, Format, ArgsColors).
html_colours(Term, TermColours) :-
    compound(Term),
    compound_name_arguments(Term, Name, Args),
    Name \== '.',
    !,
    (   Args = [One]
    ->  TermColours = html(Name)-ArgColours,
        (   layout(Name, _, empty)
        ->  attr_colours(One, ArgColours)
        ;   html_colours(One, Colours),
            ArgColours = [Colours]
        )
    ;   Args = [AList,Content]
    ->  TermColours = html(Name)-[AColours, Colours],
        attr_colours(AList, AColours),
        html_colours(Content, Colours)
    ;   TermColours = error
    ).
html_colours(_, classify).

list_colours(Var, classify) :-
    var(Var),
    !.
list_colours([], []).
list_colours([H0|T0], [H|T]) :-
    !,
    html_colours(H0, H),
    list_colours(T0, T).
list_colours(Last, Colours) :-          % improper list
    html_colours(Last, Colours).

attr_colours(Var, classify) :-
    var(Var),
    !.
attr_colours([], classify) :- !.
attr_colours(Term, list-Elements) :-
    Term = [_|_],
    !,
    attr_list_colours(Term, Elements).
attr_colours(Name=Value, built_in-[html_attribute(Name), VColour]) :-
    !,
    attr_value_colour(Value, VColour).
attr_colours(NS:Term, built_in-[ html_xmlns(NS),
                                 html_attribute(Name)-[classify]
                               ]) :-
    compound(Term),
    compound_name_arity(Term, Name, 1).
attr_colours(Term, html_attribute(Name)-[VColour]) :-
    compound(Term),
    compound_name_arity(Term, Name, 1),
    !,
    Term =.. [Name,Value],
    attr_value_colour(Value, VColour).
attr_colours(Name, html_attribute(Name)) :-
    atom(Name),
    !.
attr_colours(Term, classify) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    !.
attr_colours(_, error).

attr_list_colours(Var, classify) :-
    var(Var),
    !.
attr_list_colours([], []).
attr_list_colours([H0|T0], [H|T]) :-
    attr_colours(H0, H),
    attr_list_colours(T0, T).

attr_value_colour(Var, classify) :-
    var(Var).
attr_value_colour(location_by_id(ID), sgml_attr_function-[Colour]) :-
    !,
    location_id(ID, Colour).
attr_value_colour(#(ID), sgml_attr_function-[Colour]) :-
    !,
    location_id(ID, Colour).
attr_value_colour(A+B, sgml_attr_function-[CA,CB]) :-
    !,
    attr_value_colour(A, CA),
    attr_value_colour(B, CB).
attr_value_colour(encode(_), sgml_attr_function-[classify]) :- !.
attr_value_colour(Atom, classify) :-
    atomic(Atom),
    !.
attr_value_colour([_|_], classify) :- !.
attr_value_colour(_Fmt-_Args, classify) :- !.
attr_value_colour(Term, classify) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    !.
attr_value_colour(_, error).

location_id(ID, classify) :-
    var(ID),
    !.
:- if(current_predicate(http_location_for_id/1)).
location_id(ID, Class) :-
    (   catch(http_location_by_id(ID, Location), _, fail)
    ->  Class = http_location_for_id(Location)
    ;   Class = http_no_location_for_id(ID)
    ).
:- endif.
location_id(_, classify).

format_colours(Format, format_string) :- atom(Format), !.
format_colours(Format, format_string) :- string(Format), !.
format_colours(_Format, type_error(text)).

format_arg_colours(Args, _Format, classify) :- is_list(Args), !.
format_arg_colours(_, _, type_error(list)).

:- op(990, xfx, :=).                    % allow compiling without XPCE
:- op(200, fy, @).

prolog_colour:style(html(_),                    [colour(magenta4), bold(true)]).
prolog_colour:style(entity(_),                  [colour(magenta4)]).
prolog_colour:style(html_attribute(_),          [colour(magenta4)]).
prolog_colour:style(html_xmlns(_),              [colour(magenta4)]).
prolog_colour:style(format_string(_),           [colour(magenta4)]).
prolog_colour:style(sgml_attr_function,         [colour(blue)]).
prolog_colour:style(http_location_for_id(_),    [bold(true)]).
prolog_colour:style(http_no_location_for_id(_), [colour(red), bold(true)]).


prolog_colour:message(html(Element)) -->
    [ '~w: SGML element'-[Element] ].
prolog_colour:message(entity(Entity)) -->
    [ '~w: SGML entity'-[Entity] ].
prolog_colour:message(html_attribute(Attr)) -->
    [ '~w: SGML attribute'-[Attr] ].
prolog_colour:message(sgml_attr_function) -->
    [ 'SGML Attribute function'-[] ].
prolog_colour:message(http_location_for_id(Location)) -->
    [ 'ID resolves to ~w'-[Location] ].
prolog_colour:message(http_no_location_for_id(ID)) -->
    [ '~w: no such ID'-[ID] ].


                /*******************************
                *        XREF HTML_META        *
                *******************************/

:- dynamic dyn_html_meta_head/3 as volatile.

prolog:xref_update_syntax((:- html_meta(Decls)), Module) :-
    dyn_meta_heads(Decls, Module).

dyn_meta_heads((A,B), Module) =>
    dyn_meta_heads(A, Module),
    dyn_meta_heads(B, Module).
dyn_meta_heads(QHead, Module) =>
    strip_module(Module:QHead, M, Head),
    most_general_goal(Head, Gen),
    retractall(dyn_html_meta_head(Gen, M, _)),
    asserta(dyn_html_meta_head(Gen, M, Head)).

%       prolog:called_by(+Goal, -Called)
%
%       Hook into library(pce_prolog_xref).  Called is a list of callable
%       or callable+N to indicate (DCG) arglist extension.

prolog:called_by(Goal, Called) :-
    (   html_meta_head(Goal, _Module, Head)
    ->  true
    ;   dyn_html_meta_head(Goal, _Module, Head)
    ),
    html_meta_called(Head, Goal, Called).

called_by(Term) -->
    called_by(Term, _).

called_by(Var, _) -->
    { var(Var) },
    !,
    [].
called_by(\G, M) -->
    !,
    (   { is_list(G) }
    ->  called_by(G, M)
    ;   {atom(M)}
    ->  [(M:G)+2]
    ;   [G+2]
    ).
called_by([], _) -->
    !,
    [].
called_by([H|T], M) -->
    !,
    called_by(H, M),
    called_by(T, M).
called_by(M:Term, _) -->
    !,
    (   {atom(M)}
    ->  called_by(Term, M)
    ;   []
    ).
called_by(Var=Term, M) -->
    { var(Var) },
    called_by(Term, M).
called_by(Term, M) -->
    { compound(Term),
      !,
      Term =.. [_|Args]
    },
    called_by(Args, M).
called_by(_, _) -->
    [].
