:- module(test_http,
          [ test_http/0
          ]).

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
:- use_module(library(time)).
:- use_module(library(settings)).
:- use_module(library(socket)).
:- use_module(library(http/websocket)).
:- use_module(library(uri)).
:- use_module(library(zlib), []).               % plugin

test_http :-
    run_tests([ http_open,
                http_get,
                http_server,
                http_read_data,
                http_header_limits,
                http_open_safety,
                http_streams,
                http_shutdown
              ]).

run_network_tests :-
    getenv('SWIPL_PUBLIC_NETWORK_TESTS', true),
    exists_source(library(ssl)).

:- begin_tests(http_open, [condition(run_network_tests)]).

:- meta_predicate
    with_server(-, 0, +).

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
test(ok_bytes_prolog, Code == 200) :-
     request('/ok/bytes/prolog', Code, Type, Content, []),
     assertion(prolog_content(Type, Content,
                              hello(world))).
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
test(fail, Code == 500) :-
    request('/fail', Code, Type, Content, []),
    assertion(html_content(Type, Content,
                           "goal unexpectedly failed")).
test(post_echo, Content == Data) :-
    Data = "Hello world!",
    request('/post/echo', _Code, _Type, Content,
            [ post(string(text/plain, Data))
            ]).
test(post_echo_keep_alive,
     [ Content2 == Data,
       cleanup(http_close_keep_alive(_))
     ]) :-
    Data = "Hello world!",
    with_server(
        ServerURL,
        ( client(ServerURL, '/post/echo', _, _, _Content1,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ]),
          client(ServerURL, '/post/echo', _, _, Content2,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ])
        ),
        [ keep_alive_timeout(0.1)
        ]).
test(post_echo_keep_alive_short_read,
     [ Content2 == Data,
       cleanup(http_close_keep_alive(_))
     ]) :-
    Data = "0123456789 Hello world!",
    with_server(
        ServerURL,
        ( client(ServerURL, '/post/echo/short_read', _, _, Content1,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ]),
          assertion(sub_string(Data, 0, 10, _, Content1)),
          client(ServerURL, '/post/echo', _, _, Content2,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ])
        ),
        [ keep_alive_timeout(0.1)
        ]).
test(post_echo_keep_alive_short_read_spawned,
     [ Content2 == Data,
       cleanup(http_close_keep_alive(_))
     ]) :-
    Data = "0123456789 Hello world!",
    with_server(
        ServerURL,
        ( client(ServerURL, '/spawn/post/echo/short_read', _, _, Content1,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ]),
          assertion(sub_string(Data, 0, 10, _, Content1)),
          client(ServerURL, '/post/echo', _, _, Content2,
                 [ post(string(text/plain, Data)),
                   connection('Keep-alive')
                 ])
        ),
        [ keep_alive_timeout(0.1)
        ]).

test(post_too_large, Code == 413) :-
    setting(http:max_request_size, Old),
    setup_call_cleanup(
        set_setting(http:max_request_size, 10),
        request('/post/echo', Code, _Type, _Content,
                [ post(string(text/plain, "Hello world!"))
                ]),
        set_setting(http:max_request_size, Old)).
test(post_max_size, Content == Data) :-
    Data = "Hello world!",
    setting(http:max_request_size, Old),
    setup_call_cleanup(
        set_setting(http:max_request_size, 12),
        request('/post/echo', _Code, _Type, Content,
                [ post(string(text/plain, Data))
                ]),
        set_setting(http:max_request_size, Old)).
test(post_infinite, Content == Data) :-
    Data = "Hello world!",
    setting(http:max_request_size, Old),
    setup_call_cleanup(
        set_setting(http:max_request_size, infinite),
        request('/post/echo', _Code, _Type, Content,
                [ post(string(text/plain, Data))
                ]),
        set_setting(http:max_request_size, Old)).

test(long_header, Reply == "HTTP/1.1 431 Request Header Fields Too Large") :-
    length(L, 70000),
    maplist(=(0'a), L),
    format(string(Req), "GET / HTTP/1.1\r\nX-A: ~s\r\n\r\n", [L]),
    raw_request(Req, Reply).
test(obs_fold, Reply == "HTTP/1.1 400 Bad Request") :-
    raw_request("GET / HTTP/1.1\r\nX-A: a\r\n b\r\n\r\n", Reply).

raw_request(Request, Reply) :-
    with_server(URL,
                ( uri_components(URL, uri_components(_,Auth,_,_,_)),
                  uri_authority_components(Auth, uri_authority(_,_,Host,Port)),
                  setup_call_cleanup(
                      tcp_connect(Host:Port, Pair, []),
                      ( format(Pair, "~s", [Request]),
                        flush_output(Pair),
                        read_line_to_string(Pair, Reply)
                      ),
                      close(Pair, [force(true)]))
                ),
                []).

with_server(URL, Goal, Options) :-
    setup_call_cleanup(
        http_server(http_dispatch,
                    [ silent(true),
                      port(localhost:Port)
                    | Options
                    ]),
        (   format(atom(URL), 'http://localhost:~w', [Port]),
            Goal
        ),
        http_stop_server(Port, [])).

client(ServerURL, Path, Code, Type, Content, ExtraHdrs) :-
    format(atom(URL), '~w~w', [ServerURL, Path]),
    setup_call_cleanup(
        http_open(URL, Stream,
                  [ status_code(Code),
                    header(content_type, Type),
                    header(content_length, Len)
                  | ExtraHdrs
                  ]),
        read_string(Stream, CLen, Content),
        close(Stream)),
    (   Len == ''
    ->  assertion(CLen == 0)
    ;   assertion(CLen == Len)
    ).

request(Path, Code, Type, Content, ExtraHdrs) :-
    with_server(ServerURL,
                client(ServerURL, Path, Code, Type, Content, ExtraHdrs),
                []).

:- http_handler('/ok/html_1', ok_html_1, []).
:- http_handler('/ok/html_unicode', ok_html_unicode, []).
:- http_handler('/ok/bytes/prolog', ok_bytes_prolog, []).
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
:- http_handler('/fail', fail, []).
:- http_handler('/post/echo', post_echo, [method(post)]).
:- http_handler('/post/echo/short_read', post_echo_short_read, [method(post)]).
:- http_handler('/spawn/post/echo/short_read', post_echo_short_read,
                [spawn, method(post)]).

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

ok_bytes_prolog(_Request) :-
    string_codes("hello(world)", Bytes),
    throw(http_reply(bytes('application/x-prolog', Bytes))).

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
    Num = 1,
    Den = 0,
    X is Num/Den,
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

fail(_Request) :-
    fail.

post_echo(Request) :-
    http_read_data(Request, Data, [to(string)]),
    format('Content-type: text/plain~n~n'),
    format('~s', [Data]).

post_echo_short_read(Request) :-
    option(input(In), Request),
    read_string(In, 10, Data),
    format('Content-type: text/plain~n~n'),
    format('~s', [Data]).

:- end_tests(http_server).


:- begin_tests(http_read_data).

read_body(Body, Extra, Data, Options) :-
    setup_call_cleanup(
        open_string(Body, In),
        http_read_data([input(In), method(post)|Extra], Data,
                       [to(string)|Options]),
        close(In)).

chunked("5\r\nHello\r\n7\r\n world!\r\n0\r\n\r\n").

test(length_ok, Data == "Hello") :-
    read_body("Hello world!", [content_length(5)], Data,
              [max_request_size(5)]).
test(length_too_large,
     error(resource_error(max_request_size), _)) :-
    read_body("Hello world!", [content_length(12)], _,
              [max_request_size(11)]).
test(chunked_ok, Data == "Hello world!") :-
    chunked(Body),
    read_body(Body, [transfer_encoding(chunked)], Data,
              [max_request_size(12)]).
test(chunked_too_large,
     error(resource_error(max_request_size), _)) :-
    chunked(Body),
    read_body(Body, [transfer_encoding(chunked)], _,
              [max_request_size(11)]).
test(no_length_too_large,
     error(resource_error(max_request_size), _)) :-
    read_body("Hello world!", [], _, [max_request_size(11)]).
test(reply_unlimited, Data == "Hello world!") :-
    setting(http:max_request_size, Old),
    setup_call_cleanup(
        set_setting(http:max_request_size, 5),
        setup_call_cleanup(
            open_string("Hello world!", In),
            http_read_data([input(In), status_code(200)], Data,
                           [to(string)]),
            close(In)),
        set_setting(http:max_request_size, Old)).

:- end_tests(http_read_data).

:- begin_tests(http_header_limits).

test(read_line, L == `abc`) :-
    setup_call_cleanup(open_string("abc\r\ndef", In),
                       http_read_line(In, L, 3),
                       close(In)).
test(read_line, L == end_of_file) :-
    setup_call_cleanup(open_string("", In),
                       http_read_line(In, L, 3),
                       close(In)).
test(read_line, error(resource_error(max_line_length))) :-
    setup_call_cleanup(open_string("abcd\r\n", In),
                       http_read_line(In, _, 3),
                       close(In)).
:- end_tests(http_header_limits).

:- begin_tests(http_open_safety).

test(request_uri, URI == '/a%0D%0AX:%20b') :-
    http_open:request_uri_encoded('/a\r\nX: b', URI).
test(header_value, error(domain_error(http_header_value, _))) :-
    http_open:header_value('a\r\nX-Injected: b').
test(url_prefix) :-
    http_open:url_prefix('http://example.com', 'http://example.com/a').
test(url_prefix, fail) :-
    http_open:url_prefix('http://example.com', 'http://example.com.evil.org/a').
test(url_prefix, fail) :-
    http_open:url_prefix('http://example.com/api', 'http://example.com/apix').
test(redirect_credentials, Options == [request_header('X-A'=b)]) :-
    http_open:parse_url_ex('http://a.org/x', P1),
    http_open:parse_url_ex('http://b.org/y', P2),
    http_open:redirect_options(P1, P2,
                               [ authorization(basic(u,p)),
                                 request_header('Cookie'=a),
                                 request_header('X-A'=b)
                               ], Options).

:- end_tests(http_open_safety).

:- begin_tests(http_streams).

chunked_data(Data, String) :-
    setup_call_cleanup(
        ( open_string(Data, In0),
          http_chunked_open(In0, In, [])
        ),
        read_string(In, _, String),
        close(In)).

test(chunked, S == "Hello world!") :-
    chunked_data("5\r\nHello\r\n7;ext=1\r\n world!\r\n0\r\nX: y\r\n\r\n", S).
test(chunked, error(io_error(read, _))) :-
    chunked_data("0x5\r\nHello\r\n0\r\n\r\n", _).
test(chunked, error(io_error(read, _))) :-
    chunked_data("5\nHello\r\n0\r\n\r\n", _).
test(chunked, error(io_error(read, _))) :-
    length(L, 2000),
    maplist(=(0'a), L),
    format(string(Data), "5;~s\r\nHello\r\n0\r\n\r\n", [L]),
    chunked_data(Data, _).
test(range_eof, error(io_error(read, _))) :-
    setup_call_cleanup(
        ( open_string("abc", In0),
          stream_range_open(In0, In, [size(10)])
        ),
        read_string(In, _, _),
        close(In)).
test(multipart_boundary, error(existence_error(option, boundary))) :-
    setup_call_cleanup(
        open_string("", In),
        multipart_open(In, _, []),
        close(In)).

ws_frames(Bytes, Message) :-
    setup_call_cleanup(
        ( open_codes_stream(Bytes, In0),
          ws_open(In0, In, [mode(client)])
        ),
        ws_receive(In, Message),
        close(In)).

test(ws_fragment_ping, Data == "abcde") :-
    ws_frames([0x01,3,0'a,0'b,0'c, 0x89,2,0'h,0'i, 0x80,2,0'd,0'e],
              Message),
    Data = Message.data.
test(ws_reserved_opcode, error(io_error(read, _))) :-
    ws_frames([0x83,1,0'a], _).
test(ws_long_control, error(io_error(read, _))) :-
    length(Payload, 126),
    maplist(=(0'a), Payload),
    ws_frames([0x89,126,0,126|Payload], _).
test(ws_truncated, error(io_error(read, _))) :-
    ws_frames([0x81,10,0'a,0'b,0'c], _).
test(ws_max_message_size, error(io_error(read, _))) :-
    setup_call_cleanup(
        ( open_codes_stream([0x81,5,0'a,0'b,0'c,0'd,0'e], In0),
          ws_open(In0, In, [mode(client), max_message_size(4)])
        ),
        ws_receive(In, _),
        close(In)).

:- end_tests(http_streams).

:- begin_tests(http_shutdown).

% Stop the server while the accept thread is inside the
% http:schedule_workers/1 hook.  The hook must not swallow the
% http_stop(_) signal. See packages-http issue #176.

test(stop_in_schedule_hook) :-
    thread_self(Me),
    http_server(http_dispatch,
                [ silent(true), port(localhost:Port), workers(1) ]),
    format(atom(Block),  'http://localhost:~w/block', [Port]),
    format(atom(Second), 'http://localhost:~w/ok/html_1', [Port]),
    asserta(block_handler(Me)),
    thread_create(get_ignore(Block), C1, []),
    thread_get_message(Me, handler_blocked(Worker), [timeout(10)]),
    asserta(block_scheduler(Me)),
    thread_create(get_ignore(Second), C2, []),
    thread_get_message(Me, scheduling, [timeout(10)]),
    thread_send_message(Worker, release),
    call_with_time_limit(10, http_stop_server(Port, [])),
    thread_join(C1),
    thread_join(C2).

:- dynamic
    block_handler/1,
    block_scheduler/1.
:- multifile
    http:schedule_workers/1.

http:schedule_workers(_Data) :-
    retract(block_scheduler(Main)),
    thread_send_message(Main, scheduling),
    thread_get_message(_Never).

:- http_handler('/block', block, []).

block(_Request) :-
    retract(block_handler(Main)),
    thread_self(Me),
    thread_send_message(Main, handler_blocked(Me)),
    thread_get_message(release),
    format('Content-type: text/plain~n~nreleased~n').

get_ignore(URL) :-
    catch(http_get(URL, _, []), _, true).

:- end_tests(http_shutdown).


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

prolog_content(Type, Content, Match) :-
    http_parse_header_value(content_type, Type,
                            media(application/'x-prolog', _Attributes)),
    term_string(Term, Content),
    Term =@= Match.

%!  chunked_data(-String) is det.
%
%   Content of the chunked data that is sent by cgi-bin/chunked.

chunked_data(S) :-
    findall(C,
            (   between(1, 1000, X),
                C is "a" + X mod 26
            ), S0),
    append(S0, S0, S).


