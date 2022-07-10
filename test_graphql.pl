/*  Part of SWI-Prolog

    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions B.V.
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

:- module(test_graphql,
          [ test_graphql/0
          ]).
:- use_module(library(plunit)).
:- use_module(graphql).

test_graphql :-
    run_tests([ graphql_round_trip
              ]).

:- begin_tests(graphql_round_trip).

test(round_trip) :-
    Var = [_{key:"string"}, true, variable("foo")],
    Doc = {| graphql(Var) ||
             query($foo: [some_type!]!) {
                 spam(field: [<Var>, true, false, null])
             }
           |},
    graphql_document_to_codes(Doc, Codes, []),
    graphql_read_document(codes(Codes), Doc, []).

:- end_tests(graphql_round_trip).


:- begin_tests(graphql_strings).

test(escape_line_terminators) :-
    String = "Hello,\r\nnewline!",
    graphql_document_to_string(
        {| graphql(String) ||
           query { foo(bar: <String>
                       baz: """
                            Hello,
                            multi-
                            lines!
                            """
                      ) } |},
        Text,
        []
    ),
    string_lines(Text, [_]).

:- end_tests(graphql_strings).
