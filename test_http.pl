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
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_stream)).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(option)).
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
        http_server(http_dispatch, [silent(true), port(localhost:Port)]),
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

test(ok_html_1, Code == 200) :-
     request('/ok/html_1', Code, Type, Content, []),
     assertion(html_content(Type, Content,
                           "world")).
test(ok_html_unicode, Code == 200) :-
     request('/ok/html_unicode', Code, Type, Content, []),
     assertion(html_content(Type, Content,
                           "world")).
test(not_found, Code == 404) :-
    request('/not-found', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "was not found on this server")).
test(forbidden, Code == 403) :-
    request('/forbidden', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "permission to access")).
test(method_not_allowed, Code == 405) :-
    request('/method_not_allowed', Code, Type, Content,
            [ method(delete) ]),
    assertion(html_content(Type, Content,
                           "not support method")).
test(not_modified, Code == 304) :-
    request('/not_modified', Code, _Type, Content, [redirect(false)]),
    assertion(Content == "").
test(moved, Code == 301) :-
    request('/moved', Code, Type, Content, [redirect(false)]),
    assertion(html_content(Type, Content,
                           "has moved")).
test(moved_temporary, Code == 302) :-
    request('/moved-temporary', Code, Type, Content, [redirect(false)]),
    assertion(html_content(Type, Content,
                           "is currently")).
test(see_also, Code == 303) :-
    request('/see-also', Code, Type, Content, [redirect(false)]),
    assertion(html_content(Type, Content,
                           "other document")).
test(server_error, Code == 500) :-
    request('/server-error', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "zero_divisor")).
test(unavailable, Code == 503) :-
    request('/unavailable', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "Shut down")).
test(resource_error, Code == 503) :-
    request('/resource-error', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "patience")).
test(not_acceptable, Code == 406) :-
    request('/not_acceptable', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "I reject")).
test(bad_request, Code == 400) :-
    request('/bad-request', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "hello")).
test(created, Code == 201) :-
    request('/created', Code, Type, Content,
            [ post(json("world")),
              header(location, Location)
            ]),
    assertion(Location == '/brave-new-world'),
    assertion(html_content(Type, Content,
                           "brave-new-world")).

request(Path, Code, Type, Content, ExtraHdrs) :-
    setup_call_cleanup(
        http_server(http_dispatch, [silent(true), port(localhost:Port)]),
        ( format(atom(URL), 'http://localhost:~w~w', [Port, Path]),
          setup_call_cleanup(
              http_open(URL, Stream,
                        [ status_code(Code),
                          header(content_type, Type),
                          header(content_length, Len)
                        | ExtraHdrs
                        ]),
              read_string(Stream, CLen, Content),
              close(Stream))
        ),
        http_stop_server(Port, [])),
    (   Len == ''
    ->  assertion(CLen == 0)
    ;   assertion(CLen == Len)
    ).

:- http_handler('/ok/html_1', ok_html_1, []).
:- http_handler('/ok/html_unicode', ok_html_unicode, []).
:- http_handler('/reply-source', reply_source, []).
:- http_handler('/forbidden', forbidden, []).
:- http_handler('/method_not_allowed', method_not_allowed,
                [methods([get])]).
:- http_handler('/moved', http_redirect(moved, '/moved-to'), []).
:- http_handler('/moved-temporary', http_redirect(moved_temporary, '/moved-to'), []).
:- http_handler('/see-also', http_redirect(see_other, '/moved-to'), []).
:- http_handler('/server-error', server_error, []).
:- http_handler('/not_modified', not_modified, []).
:- http_handler('/unavailable', unavailable, []).
:- http_handler('/not_acceptable', not_acceptable, []).
:- http_handler('/resource-error', resource_error_handler, []).
:- http_handler('/bad-request', bad_request_handler, []).
:- http_handler('/created', created, []).

ok_html_1(_Request) :-
    reply_html_page(
        title('Test'),
        h1('Hello world')).

ok_html_unicode(_Request) :-
    numlist(1000, 1050, Text),
    reply_html_page(
        title('Test'),
        [ h1('Hello world'),
          p([], '~s'-[Text])
        ]).

reply_source(Request) :-
    module_property(test_http, file(File)),
    http_reply_file(File, [unsafe(true)], Request).

forbidden(Request) :-
    option(path(Path), Request),
    throw(http_reply(forbidden(Path))).

method_not_allowed(Request) :-
    option(method(Method), Request),
    format('Content-type: text/plain~n~n'),
    format('Method ~p should not have been alowed', [Method]).

server_error(_Request) :-
    X is 1/0,
    format('Content-type: text/plain~n~n'),
    format('X = ~q~n', [X]).

not_modified(_Request) :-
    throw(http_reply(not_modified)).

unavailable(_Request) :-
    throw(http_reply(unavailable(p(['Shut down'])))).

not_acceptable(_Request) :-
    throw(http_reply(not_acceptable(p(['I reject'])))).

resource_error_handler(_Request) :-
    resource_error(patience).

bad_request_handler(Request) :-
    http_parameters(Request, [ hello(_,[]) ]).

created(Request) :-
    http_read_json_dict(Request, _Dict),
    throw(http_reply(created('/brave-new-world'))).

:- end_tests(http_server).


                 /*******************************
                 *             UTIL             *
                 *******************************/

html_content(Type, Content, Needle) :-
    http_parse_header_value(content_type, Type, media(text/html, _Attributes)),
    contains_codes(Needle, Content).

contains_codes(Needle, Haystack) :-
    to_string(Needle, NeedleS),
    to_string(Haystack, HaystackS),
    sub_string(HaystackS, _, _, _, NeedleS),
    !.

to_string(S, S) :-
    string(S),
    !.
to_string(A, S) :-
    atom(A),
    !,
    atom_string(A, S).
to_string(Codes, S) :-
    string_codes(S, Codes).



%!  chunked_data(-String) is det.
%
%   Content of the chunked data that is sent by cgi-bin/chunked.

chunked_data(S) :-
    findall(C,
            (   between(1, 1000, X),
                C is "a" + X mod 26
            ), S0),
    append(S0, S0, S).


