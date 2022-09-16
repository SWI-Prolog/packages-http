/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2022, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_term_html,
          [ test_term_html/0,
            term_html_string/3                  % @Term, -String, +Options
          ]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/html_write)).

:- use_module(library(http/term_html)).

test_term_html :-
    run_tests([ term_html
              ]).


term_html_string(Term, HTMLString, Options) :-
    phrase(term(Term, Options), Tokens),
    with_output_to(string(HTMLString), print_html(Tokens)).

string_dom(String, DOM) :-
    setup_call_cleanup(
        new_memory_file(MF),
        ( setup_call_cleanup(
              open_memory_file(MF, write, Out),
              write(Out, String),
              close(Out)),
          setup_call_cleanup(
              open_memory_file(MF, read, In),
              load_html(stream(In), DOM, []),
              close(In))
        ),
        free_memory_file(MF)).

string_plain(HTMLString, Plain) :-
    string_dom(HTMLString, [DOM]),
    xpath(DOM, /(*(text)), PlainAtom),
    atom_string(PlainAtom, Plain).

trip(TermIn, StringOut, TermOut, Options) :-
    (   option(numbervars(true), Options)
    ->  copy_term(TermIn, Term1),
        numbervars(Term1, 1, _, [singletons(true)])
    ;   Term1 = TermIn
    ),
    term_html_string(Term1, HTMLString, Options),
    string_plain(HTMLString, StringOut),
    debug(term_html(plain), 'Plain: ~p', [StringOut]),
    (   option(quoted(true), Options),
        \+ option(max_depth(_), Options)
    ->  term_string(TermOut, StringOut)
    ;   true
    ).

test_term_html(String) :-
    test_term_html(String, String,
                   [ quoted(true),
                     numbervars(true)
                   ]).

test_term_html(In, Out, Options) :-
    term_string(TermIn, In),
    trip(TermIn, StringOut, TermOut, Options),
    assertion(Out == StringOut),
    (   option(quoted(true), Options),
        \+ option(max_depth(_), Options)
    ->  assertion(TermIn =@= TermOut)
    ;   true
    ).

:- begin_tests(term_html).

test(op) :- test_term_html("_ is 1/9").
test(op) :- test_term_html("_ is (*)/9").
test(op) :- test_term_html("_ is a mod b").
test(op) :- test_term_html("_ is ... mod b").
test(op) :- test_term_html("[cat, is, no, dog]").
test(op) :- test_term_html("[dynamic, :-]").

test(compound) :- test_term_html("a(b)").
test(compound) :- test_term_html("a(1/c)").
test(compound) :- test_term_html("a((a,b))").
test(compound) :- test_term_html("a((h:-b))").

test(dict) :- test_term_html("_{}").
test(dict) :- test_term_html("_{a:1, b:2}").
%test(dict) :- test_term_html("_{* :1}").       % FIXME: quoted keys do not work
test(dict) :- test_term_html("_{a: *}").

test(list) :- test_term_html("[]").
test(list) :- test_term_html("[a]").
test(list) :- test_term_html("[a|b]").

test(curl) :- test_term_html("{}").
test(curl) :- test_term_html("{a}").
test(curl) :- test_term_html("{a,b}").
test(curl) :- test_term_html("{a->b;c}").

test(ellipsis) :- test_term_html("a(b(c))",    "a(...)",       [max_depth(1)]).
test(ellipsis) :- test_term_html("[a,b,c]",    "[...|...]",    [max_depth(1)]).
test(ellipsis) :- test_term_html("[a,b,c]",    "[a|...]",      [max_depth(2)]).
test(ellipsis) :- test_term_html("[a,b,c]",    "[a, b|...]",   [max_depth(3)]).
test(ellipsis) :- test_term_html("[a,b,c]",    "[a, b, c]",    [max_depth(4)]).
test(ellipsis) :- test_term_html("[a(b),c]",   "[a(...)|...]", [max_depth(2)]).
test(ellipsis) :- test_term_html("a{b:1,c:2}", "a{...}",       [max_depth(1)]).
test(ellipsis) :- test_term_html("a{b:1,c:2}", "a{b:..., c:...}",
    [max_depth(2)]).
test(ellipsis) :- test_term_html("+a",         "+ ...",        [max_depth(1)]).

:- end_tests(term_html).
