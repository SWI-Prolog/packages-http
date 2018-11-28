/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2014, University of Amsterdam
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

:- module(test_json,
          [ test_json/0
          ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '..')).

:- use_module(library(plunit)).
:- use_module(library(http/json)).

test_json :-
    run_tests([ json_read,
                json_convert,
                json_http
              ]).

:- begin_tests(json_read).

test(true, X == @(true)) :-
    atom_json_term(true, X, []).
test(true, X == true) :-
    atom_json_term(true, X, [true(true)]).

test(string, X == hello) :-
    atom_json_term('"hello"', X, []).
test(string, X == '\\\b\f\n\r\t') :-
    atom_json_term('"\\\\\\b\\f\\n\\r\\t"', X, []).
test(string, X == '\u1234') :-
    atom_json_term('"\\u1234"', X, []).

test(int, X == 42) :-
    atom_json_term('42', X, []).
test(int, X == -42) :-
    atom_json_term('-42', X, []).

test(float, X == 3.14) :-
    atom_json_term('3.14', X, []).
test(float, X == -3.14) :-
    atom_json_term('-3.14', X, []).
test(float, X == 1000.0) :-
    atom_json_term('1e3', X, []).
test(float, X == 1000.0) :-
    atom_json_term('1.0e3', X, []).
test(float, X == 0.001) :-
    atom_json_term('1e-3', X, []).
test(float, X == 0.001) :-
    atom_json_term('1.0e-3', X, []).

test(empty, X == json([])) :-
    atom_json_term({}, X, []).
test(empty, X == json([])) :-
    atom_json_term('  {  } ', X, []).
test(empty, X == json([])) :-
    atom_json_term('  { } ', X, []).


:- end_tests(json_read).


                 /*******************************
                 *            CONVERT           *
                 *******************************/

:- begin_tests(json_convert).

:- use_module(library(http/json_convert)).

:- json_object
    point(x:integer, y:integer),
    tpoint(x:integer, y:integer)+[type=point],
    fpoint(x:float, y:float).

test(pt2json, JSON == json([x=25,y=50])) :-
    prolog_to_json(point(25,50), JSON).
test(pt2json, JSON == json([x=25,y=50,type=point])) :-
    prolog_to_json(tpoint(25,50), JSON).

test(json2pt, X == point(25,50)) :-
    json_to_prolog(json([x=25,y=50]), X).
test(json2pt, X == point(25,50)) :-
    json_to_prolog(json([y=50,x=25]), X).
test(json2pt, X == fpoint(25.1,50.0)) :-
    json_to_prolog(json([y=50.0,x=25.1]), X).
test(json2pt, T == T2) :-
    T = json([y=50,x=25.1]),
    json_to_prolog(json([y=50,x=25.1]), T2).
test(json2pt, X == tpoint(25,50)) :-
    json_to_prolog(json([x=25,y=50,type=point]), X).

:- end_tests(json_convert).


                 /*******************************
                 *             HTTP             *
                 *******************************/

:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).

:- dynamic
    port/1.

make_server :-
    retractall(port(_)),
    http_server(reply,
                [ port(Port),
                  workers(1)
                ]),
    assert(port(Port)).

kill_server :-
    retract(port(Port)),
    http_stop_server(Port, []).

reply(Request) :-
    memberchk(path('/json/echo'), Request),
    !,
    http_read_json(Request, JSON),
    reply_json(JSON).

echo(Term, Reply) :-
    port(Port),
    format(string(URL), 'http://localhost:~w/json/echo', [Port]),
    http_post(URL, json(Term), Reply, []).

:- begin_tests(json_http, [ setup(make_server),
                            cleanup(kill_server)
                          ]).

test(echo, X == 42) :-
    echo(42, X).
test(echo, X == -3.14) :-
    echo(-3.14, X).
test(echo, X == name) :-
    echo(name, X).
test(echo, X == [1,2,3]) :-
    echo([1,2,3], X).
test(echo, X == json([name=json, arity=2])) :-
    echo(json([name=json, arity=2]), X).

test(unicode, X == Atom) :-
    Atom = '\u0411\u0435\u0437\u0443\u043f\u0440\u0435\u0447\u043d\u043e\u0435',
    echo(Atom, X).
test(quote, X == Atom) :-
    Atom = 'hello, "world"',
    echo(Atom, X).
test(control, X == Atom) :-
    Atom = 'hello\n\t\r\b\003\',
    echo(Atom, X).

:- end_tests(json_http).

:- multifile
    user:message_hook/3.

user:message_hook(httpd_stopped_worker(_, true), _Kind, _Lines).
