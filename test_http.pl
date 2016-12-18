:- module(test_http,
          [ test_http/0
          ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(foreign, '../zlib')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../zlib')).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_stream)).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(zlib), []).               % plugin

test_http :-
    run_tests([ http_open,
                http_get,
                http_server
              ]).

run_network_tests :-
    \+ getenv('USE_PUBLIC_NETWORK_TESTS', false).

:- begin_tests(http_open, [condition(run_network_tests)]).

test(read, true) :-
    http_open('http://www.swi-prolog.org/', In,
              [status_code(Code)]),
    assertion(Code == 200),
    read_stream_to_codes(In, Codes),
    close(In),
    contains_codes("href=\"/Download.html\"", Codes).
test(redirect, true) :-
    http_open('http://www.swi-prolog.org', In, []),
    read_stream_to_codes(In, Codes),
    close(In),
    contains_codes("href=\"/Download.html\"", Codes).
test(chunked, true(Codes == Ref)) :-
    http_open('http://www.swi-prolog.org/Tests/chunked/data', In, []),
    read_stream_to_codes(In, Codes),
    close(In),
    chunked_data(Ref).

:- end_tests(http_open).

:- begin_tests(http_get, [condition(run_network_tests)]).

test(read, true) :-
    http_get('http://www.swi-prolog.org/', Data, [to(codes)]),
    contains_codes("http://www.swi-prolog.org", Data).

test(chunked, true(Data == Ref)) :-
    http_get('http://www.swi-prolog.org/Tests/chunked/data',
             Data, [to(codes)]),
    chunked_data(Ref).

:- end_tests(http_get).


:- begin_tests(http_server).

test(connection, Close == close) :-
    setup_call_cleanup(
        http_server(http_dispatch, [port(localhost:Port)]),
        ( format(atom(URL), 'http://localhost:~w/reply-source', [Port]),
          setup_call_cleanup(
              open_null_stream(Out),
              setup_call_cleanup(
                  http_open(URL, Stream,
                            [ header(connection, Close)
                            ]),
                  copy_stream_data(Stream, Out),
                  close(Stream)),
              close(Out))
        ),
        http_stop_server(Port, [])).

:- http_handler('/reply-source', reply_source, []).

reply_source(Request):-
    module_property(test_http, file(File)),
    http_reply_file(File, [unsafe(true)], Request).

:- end_tests(http_server).


                 /*******************************
                 *             UTIL             *
                 *******************************/

contains_codes(String, Codes) :-
    string_codes(String, Needle),
    append(_Pre, Rest, Codes),
    append(Needle, _Post, Rest), 
    !.

%!  chunked_data(-String) is det.
%
%   Content of the chunked data that is sent by cgi-bin/chunked.

chunked_data(S) :-
    findall(C,
            (   between(1, 1000, X),
                C is "a" + X mod 26
            ), S0),
    append(S0, S0, S).


