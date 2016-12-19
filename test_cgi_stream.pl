/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(test_cgi_stream,
          [ test_cgi_stream/0
          ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(http_stream).
:- use_module(http_header).
:- use_module(http_client).

/** <module> Test CGI stream object

This module defines a series of tests   outside  the context of the HTTP
server to validate correct  processing  of   the  CGI  header,  handling
different  encodings  and  transfer    encodings  (chunked/traditional).
Instead of using real sockets, we use temporary storage on a file.

@tbd    Validate error processing
*/

test_cgi_stream :-
    run_tests([ cgi_stream,
                cgi_chunked,
                cgi_errors
              ]).

                 /*******************************
                 *          DESTINATION         *
                 *******************************/

open_dest(TmpF, Out) :-
    tmp_file(http, TmpF),
    open(TmpF, write, Out, [type(binary)]).

free_dest(TmpF) :-
    delete_file(TmpF).

free_dest(TmpF, Out) :-
    close(Out),
    delete_file(TmpF).

http_read_mf(TmpF, Header, Data) :-
    open(TmpF, read, In, [type(binary)]),
    http_read_reply_header(In, Header),
    http_read_data(Header, Data, [to(atom)]),
    close(In).


cat(TmpF) :-
    open(TmpF, read, In),
    call_cleanup(copy_stream_data(In, current_output),
                 close(In)).


                 /*******************************
                 *            MAKE DATA         *
                 *******************************/

%!  data_atom(+Length, +Min, +Max, -Atom) is det.
%
%   Create an atom of Length codes.  It contains repeating sequences
%   Min..Max.

data_atom(Length, Min, Max, Atom) :-
    data_list(Length, Min, Max, List),
    atom_codes(Atom, List).

data_list(Length, Min, Max, List) :-
    Span is Max - Min,
    data_list(Length, Min, 0, Span, List).

data_list(Len, Min, I, Span, [H|T]) :-
    Len > 0,
    !,
    H is Min + I mod Span,
    Len2 is Len - 1,
    I2 is I+1,
    data_list(Len2, Min, I2, Span, T).
data_list(_, _, _, _, []).

%!  data(+Name, -Data, -ContentType) is det.
%
%   Create data-sets to be used with the PlUnit forall option.

data(short_ascii, Data, 'text/plain') :-
    data_atom(10, 97, 128, Data).
data(ascii, Data, 'text/plain') :-
    data_atom(126, 1, 128, Data).
data(unicode, Data, 'text/plain') :-
    data_atom(10, 1000, 1010, Data).
data(long_unicode, Data, 'text/plain') :-
    data_atom(10000, 1, 1000, Data).
data(long_binary, Data, 'text/plain') :-
    data_atom(10000, 0, 255, Data).

%!  current_data(-Name) is nondet.
%
%   Enumerate available datasets.

current_data(Name) :-
    clause(data(Name, _, _), _).


                 /*******************************
                 *             TEST             *
                 *******************************/

assert_header(Header, Field) :-
    memberchk(Field, Header),
    !.
assert_header(_Header, Field) :-
    format(user_error, 'ERROR: ~p expected in header~n', [Field]),
    fail.


                 /*******************************
                 *            HOOK              *
                 *******************************/

cgi_hook(What, _CGI) :-
    debug(http(hook), 'Running hook: ~q', [What]),
    fail.
cgi_hook(header, CGI) :-
    cgi_property(CGI, header_codes(HeadText)),
    http_parse_header(HeadText, CgiHeader),
    cgi_property(CGI, request(Request)),
    http_update_connection(CgiHeader, Request, Connection, Header1),
    http_update_transfer(Request, Header1, Transfer, Header2),
    http_update_encoding(Header2, Encoding, Header),
    set_stream(CGI, encoding(Encoding)),
    cgi_set(CGI, connection(Connection)),
    cgi_set(CGI, header(Header)),
    cgi_set(CGI, transfer_encoding(Transfer)). % must be LAST
cgi_hook(send_header, CGI) :-
    cgi_property(CGI, header(Header)),
    cgi_property(CGI, client(Out)),
    (   cgi_property(CGI, transfer_encoding(chunked))
    ->  phrase(http_header:reply_header(chunked_data, Header, _), String)
    ;   cgi_property(CGI, content_length(Len))
    ->  phrase(http_header:reply_header(cgi_data(Len), Header, _), String)
    ),
    format(Out, '~s', [String]).
cgi_hook(close, _).


:- begin_tests(cgi_stream, [sto(rational_trees)]).

test(traditional,
     [ forall(current_data(Name)),
       Reply == Data,
       setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF))
     ]) :-
    data(Name, Data, ContentType),
    cgi_open(Out, CGI, cgi_hook, []),
    format(CGI, 'Content-type: ~w\n\n', [ContentType]),
    format(CGI, '~w', [Data]),
    close(CGI),
    close(Out),
    http_read_mf(TmpF, Header, Reply),
    assert_header(Header, status(_, ok, _)).

test(unicode,
     [ forall(Name=unicode),
       Reply == Data,
       setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF))
     ]) :-
    data(Name, Data, ContentType),
    cgi_open(Out, CGI, cgi_hook, []),
    format(CGI, 'Content-type: ~w\n\n', [ContentType]),
    format(CGI, '~w', [Data]),
    close(CGI),
    close(Out),
    http_read_mf(TmpF, Header, Reply),
    assert_header(Header, status(_, ok, _)).

:- end_tests(cgi_stream).

:- begin_tests(cgi_chunked, [sto(rational_trees)]).

test(chunked,
     [ forall(current_data(Name)),
       Reply == Data,
       setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF))
     ]) :-
    data(Name, Data, ContentType),
    cgi_open(Out, CGI, cgi_hook,
             [ request([http_version(1-1)])
             ]),
    format(CGI, 'Transfer-encoding: chunked\n', []),
    format(CGI, 'Content-type: ~w\n\n', [ContentType]),
    format(CGI, '~w', [Data]),
    close(CGI),
    close(Out),
    http_read_mf(TmpF, Header, Reply),
    assert_header(Header, status(_, ok, _)),
    assert_header(Header, transfer_encoding(chunked)).

:- end_tests(cgi_chunked).


                 /*******************************
                 *         ERROR HANDLING       *
                 *******************************/

%!  collect_messages(:Goal, -Messages) is semidet.
%
%   Run Goal as once/1, collecting possible messages in Messages.

:- meta_predicate
    collect_messages(0, -, -).

collect_messages(Goal, True, Messages) :-
    strip_module(Goal, M, G),
    collect_messages2(M:G, True, Messages).

:- multifile
    user:message_hook/3.
:- dynamic
    msg_collecting/0,
    msg/2.

user:message_hook(Term, Kind, _Lines) :-
    msg_collecting,
    !,
    assert(msg(Term, Kind)).

collect_messages2(Goal, True, Messages) :-
    assert(msg_collecting, Ref),
    call_cleanup(call_result(Goal, True),
                 (   erase(Ref),
                     findall(message(Term, Kind), retract(msg(Term, Kind)),
                             Messages))).

call_result(Goal, true) :-
    Goal,
    !.
call_result(_, false).


:- begin_tests(cgi_errors, [sto(rational_trees)]).

cgi_fail_hook(Event, _) :-
    debug(http(hook), 'Failing hook for ~w', [Event]),
    fail.

cgi_error_hook(Event, _) :-
    debug(http(hook), 'Error hook for ~w', [Event]),
    throw(error(demo_error, _)).

test(hook_failed,
      [ setup(open_dest(TmpF, Out)),
        cleanup(free_dest(TmpF, Out)),
        error(io_error(_,_))
      ]) :-
    cgi_open(Out, CGI, cgi_fail_hook, []),
    close(CGI).

test(hook_error,
     [ setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF, Out)),
       error(demo_error)
     ]) :-
    cgi_open(Out, CGI, cgi_error_hook, []),
    close(CGI).

:- end_tests(cgi_errors).


                 /*******************************
                 *             PORTRAY          *
                 *******************************/

user:portray(Atom) :-
    atom(Atom),
    atom_length(Atom, Len),
    Len > 100,
    !,
    sub_atom(Atom, 0, 35, _, Start),
    sub_atom(Atom, _, 35, 0, End),
    format('~q...[~D codes]...~q', [Start, Len, End]).

