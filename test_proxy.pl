:- module(test_proxy,
	  [ test_proxy/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../ssl')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../ssl')).

:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_proxy)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(option)).
:- use_module(library(plunit)).
:- use_module(library(socket)).
:- if(exists_source(library(unix))).
:- use_module(library(unix), [pipe/2]).
:- endif.
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).
:- if(exists_source(library(ssl))).
:- use_module(library(ssl)).
:- use_module(library(http/http_ssl_plugin)).
test_https(true).
:- else.
test_https(false).
:- endif.

test_proxy :-
    assign_ports,
    run_tests([ proxy
	      ]).

test_input(Name, Path) :-
    source_file(test_proxy, MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, Name], /, Path).

:- dynamic
    port/2.                                 % Role, Port

assign_ports :-
    port(unused, _),
    !.
assign_ports :-
    free_ports(4, [P1,P2,P3,P4]),
    assertz(port(http_endpoint,  P1)),      % our HTTP target server
    (   test_https(true)
    ->  assertz(port(https_endpoint, P2))   % our HTTPS target server
    ;   true
    ),
    assertz(port(socks,          P3)),      % our socks server
    assertz(port(unused,         P4)).      % port without a server

free_ports(N, Ports) :-
    length(Sockets, N),
    maplist(alloc_port, Sockets, Ports),
    maplist(tcp_close_socket, Sockets).

alloc_port(Socket, Port) :-
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port).


:- begin_tests(proxy, [condition(current_predicate(pipe/2))]).

:- dynamic
    test_proxy/3,
    http_proxy_control/3,                   % Port, Thread, ControlWrite
    socks_control/3,
    expect_failure/0.

:- multifile
    socket:proxy_for_url/3.

socket:proxy_for_url(URL, Hostname, Proxy):-
    debug(proxy, 'Proxy requested for ~w (~w)~n', [URL, Hostname]),
    test_proxy(URL, Hostname, ProxyList),
    debug(proxy, '... -> ~w~n', [ProxyList]),
    member(Proxy, ProxyList).

start_http_proxy(Port):-
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    pipe(ControlRead, ControlWrite),
    format(atom(Alias), 'http-proxy@~w', [Port]),
    thread_create(http_proxy_server(Socket, ControlRead), ThreadId,
		  [alias(Alias)]),
    assert(http_proxy_control(Port, ThreadId, ControlWrite)).

stop_http_proxy_server:-
    debug(stop, 'Stopping http proxy server ...', []),
    retract(http_proxy_control(Port, ThreadId, ControlWrite)),
    close(ControlWrite),
    catch(setup_call_cleanup(
	      tcp_connect(localhost:Port, Tmp,
			  [ bypass_proxy(true)
			  ]),
	      true,
	      close(Tmp)),
	  E, print_message(warning, stop(http_proxy_server, E))),
    thread_join(ThreadId, _),
    debug(stop, 'ok', []).

http_proxy_server(Socket, ControlRead):-
    setup_call_cleanup(
	true,
	http_proxy_accept_loop(Socket, ControlRead),
	( tcp_close_socket(Socket),
	  close(ControlRead, [force(true)])
	)).

:- det(http_proxy_accept_loop/2).
http_proxy_accept_loop(ServerFd, Control) :-
    thread_self(Self),
    http_proxy_control(_, Self, _),
    setup_call_cleanup(
	( tcp_accept(ServerFd, ClientFd, _Peer),
	  tcp_open_socket(ClientFd, Stream)
	),
	catch(do_http_proxy_request(Stream, Control),
	      Error,
	      (   (   expect_failure
		  ->  true
		  ;   print_message(warning,
				    stop(http_proxy_accept_client, Error))
		  ),
		  format(Stream, 'HTTP/1.0 500 Something smells bad~n~n', [])
	      )),
	close(Stream, [force(true)])),
    http_proxy_accept_loop(ServerFd, Control).
http_proxy_accept_loop(_, _).


parse_http_proxy_request(Verb, Target)-->
    verb(Verb), " ", target(Target), " HTTP/", http_version(_).

verb(connect)--> "CONNECT", !.
verb(get)--> "GET", !.
target(Target)-->
    string_without(" ", TargetString),
    {atom_string(Target, TargetString)}.

http_version(1-1)--> "1.1".
http_version(1-0)--> "1.0".

read_headers(Read, Tail):-
    read_line_to_codes(Read, Codes),
    (   Codes == []
    ->  Tail = []
    ;   http_parse_header(Codes, Header)
    ->  append(Header, NewTail, Tail),
	read_headers(Read, NewTail)
    ;   read_headers(Read, Tail)
    ).

do_http_proxy_request(_, _) :-
    thread_self(Self),
    \+ http_proxy_control(_, Self, _),
    !.
do_http_proxy_request(Stream, Control) :-
    read_line_to_codes(Stream, Codes),
    read_headers(Stream, ReadHeaders),
    parse_http_proxy_request(Verb, Target, Codes, []),
    (   Verb == connect
    ->  atomic_list_concat([Hostname, PortAtom], ':', Target),
	atom_number(PortAtom, Port),
	(   test_http_connect_mapping(
		Hostname:Port, MappedHostname:MappedPort)
	->  true
	;   MappedHostname = Hostname,
	    MappedPort = Port
	),
	assert(http_proxy_connection_attempt(
		   connect(MappedHostname:MappedPort))),
	tcp_connect(MappedHostname:MappedPort, SlaveStream,
		    [bypass_proxy(true)]),
	debug(proxy, 'Connected via CONNECT to ~w', [Hostname:Port]),
	format(Stream, 'HTTP/1.1 200 Connection established~n~n', []),
	flush_output(Stream),
	stream_pair(SlaveStream, SlaveRead, SlaveWrite),
	shovel_loop(Stream, SlaveRead, SlaveWrite, Control)
    ;   Verb == get
    ->  setup_call_cleanup(
	    http_open(Target, Slave,
		      [ bypass_proxy(true),
			headers(Headers),
			version(Version),
			status_code(Code)
		      ]),
	    http_get_proxy(Code, Version, Headers, Slave, Stream),
	    close(Slave)),
	(   memberchk(proxy_authorization(_), ReadHeaders)
	->  assert(http_proxy_connection_attempt(
		       authenticated_get(Target)))
	;   assert(http_proxy_connection_attempt(get(Target)))
	),
	flush_output(Stream),
	close(Stream, [force(true)])
    ).

http_get_proxy(Code, (Major-Minor), _Headers, Slave, Write):-
    format(Write, 'HTTP/~w.~w ~w Whatever~n~n', [Major, Minor, Code]),
    copy_stream_data(Slave, Write).


:-dynamic
	test_socks_mapping/2,
	test_http_connect_mapping/2.

start_socks_server(Port):-
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    pipe(ControlRead, ControlWrite),
    format(atom(Alias), 'socks@~w', [Port]),
    thread_self(Me),
    thread_create(socks_server(Me, Socket, ControlRead), ThreadId,
		  [ alias(Alias) ]),
    assert(socks_control(Port, ThreadId, ControlWrite)),
    thread_get_message(started).

:- dynamic socks_waiting/1.

stop_socks_server(Port) :-
    socks_control(Port, ThreadId, ControlWrite),
    ignore(thread_wait(socks_waiting(ThreadId), [timeout(5)])),
    retract(socks_control(Port, ThreadId, ControlWrite)),
    thread_property(ThreadId, id(_0Id)),
    debug(stop, 'Stopping socks server ~p=~p ...', [ThreadId,_0Id]),
    catch_with_backtrace(
	setup_call_cleanup(
	    tcp_connect(localhost:Port, Tmp,
			[ bypass_proxy(true)
			]),
	    true,
	    close(Tmp)),
	E, print_message(warning, stop(socks_server, E))),
    thread_join(ThreadId, _),
    close(ControlWrite),
    debug(stop, 'ok', []).

socks_server(Initiator, Socket, ControlRead) :-
    thread_send_message(Initiator, started),
    thread_self(Me),
    thread_property(Me, id(Id)),
    debug(start, 'Started SOCKS server in thread ~p', [Id]),
    catch_with_backtrace(
	call_cleanup(socks_server_loop(Socket, ControlRead),
		     ( tcp_close_socket(Socket),
		       close(ControlRead)
		     )),
	E,
	print_message(warning, E)).

socks_server_loop(_, _) :-
    thread_self(Self),
    \+ socks_control(_, Self, _),
    debug(stop, 'Socks server ~p is done', [Self]),
    !.
socks_server_loop(ServerFd, Control) :-
    thread_self(Me),
    setup_call_cleanup(
	( setup_call_cleanup(
	      asserta(socks_waiting(Me), Ref),
	      tcp_accept(ServerFd, Socket, _Peer),
	      erase(Ref)),
	  tcp_open_socket(Socket, Stream)
	),
	handle_socks_client(Control, Stream),
	close(Stream)),
    socks_server_loop(ServerFd, Control).

:- det(handle_socks_client/2).
handle_socks_client(_Control, _Stream) :-
    thread_self(Self),
    \+ socks_control(_, Self, _),
    !.
handle_socks_client(Control, Stream) :-
    get_byte(Stream, _Version),
    get_byte(Stream, AuthCount),
    findall(AuthMethod,
	    ( between(1, AuthCount, _),
	      get_byte(Stream, AuthMethod)
	    ),
	    _AuthMethods),
    format(Stream, '~s', [[0x5, 0x0]]),
    flush_output(Stream),
    do_socks_request(Stream, Control).

do_socks_request(Stream, Control):-
    get_byte(Stream, _Version),
    get_byte(Stream, Action),
    get_byte(Stream, _Reserved),
    (   Action =:= 1
    ->  get_byte(Stream, AddressType),
	(   AddressType =:= 1
	->  get_byte(Stream, A),
	    get_byte(Stream, B),
	    get_byte(Stream, C),
	    get_byte(Stream, D),
	    format(atom(Hostname), '~w.~w.~w.~w', [A, B, C, D]),
	    AddressBytes = [A,B,C,D]
	;   AddressType =:= 3
	->  get_byte(Stream, Length),
	    findall(Code,
		    ( between(1, Length, _),
		      get_byte(Stream, Code)
		    ),
		    Codes),
	    AddressBytes = [Length|Codes],
	    atom_codes(Hostname, Codes)
	),
	get_byte(Stream, PortHi),
	get_byte(Stream, PortLo),
	Port is (PortHi << 8) \/ PortLo
    ;   format(Stream, '~s', [0x1]),
	fail
    ),
    assert(socks_proxy_connection_attempt(Hostname:Port)),
    (   test_socks_mapping(Hostname:Port, MappedHostname:MappedPort)
    ->  true
    ;   MappedHostname = Hostname,
	MappedPort = Port
    ),
    tcp_connect(MappedHostname:MappedPort, SlaveStream,
		[bypass_proxy(true)]),
    debug(proxy, 'Connected via SOCKS to ~w:~w', [Hostname, Port]),
    format(Stream, '~s~s~s',
	   [ [0x5, 0x0, 0x0, AddressType],
	     AddressBytes,
	     [PortHi, PortLo]
	   ]),
    flush_output(Stream),
    stream_pair(SlaveStream, SlaveRead, SlaveWrite),
    shovel_loop(Stream, SlaveRead, SlaveWrite, Control).

:- dynamic
    test_socks_mapping/2.

shovel_loop(Pair, SlaveRead, SlaveWrite, Control) :-
    wait_for_input([Pair, SlaveRead, Control], ReadyList, infinite),
    shovel_dispatch(Pair, SlaveRead, SlaveWrite, Control, ReadyList, Done),
    (   Done == true
    ->  true
    ;   shovel_loop(Pair, SlaveRead, SlaveWrite, Control)
    ).

shovel_dispatch(_, _SlaveRead, _SlaveWrite, _Control, [], _) :- !.
shovel_dispatch(Pair, SlaveRead, SlaveWrite, Control, [Stream|More], Done) :-
    (   at_end_of_stream(Stream)
    ->  close(Pair),
	close(SlaveWrite),
	close(SlaveRead),
	Done = true
    ;   (   Stream == Pair
	->  read_pending_codes(Stream, Bytes, []),
	    format(SlaveWrite, '~s', [Bytes]),
	    flush_output(SlaveWrite)
	;   Stream == SlaveRead
	->  read_pending_codes(Stream, Bytes, []),
	    format(Pair, '~s', [Bytes]),
	    flush_output(Pair)
	;   Stream == Control
	->  true
	),
	shovel_dispatch(Pair, SlaveRead, SlaveWrite, Control, More, Done)
    ).


:- dynamic
    socks_proxy_connection_attempt/1,
    http_proxy_connection_attempt/1,
    http_page_serve_attempt/1.

http_endpoint(_Request):-
    assert(http_page_serve_attempt(?)),
    format('Content-type: text/html~n~nHello', []).

start_servers :-
    port(http_endpoint, HTTP_port),
    port(socks, SOCKS_port),
    start_socks_server(SOCKS_port),
    http_server(http_endpoint,
		[ port(HTTP_port),
		  workers(2)
		]),
    (   port(https_endpoint, HTTPS_port)
    ->  test_input('../ssl/etc/server/server-cert.pem', ServerCert),
	test_input('../ssl/etc/server/server-key.pem', ServerKey),
	http_server(http_endpoint,
		    [ port(HTTPS_port),
		      workers(2),
		      ssl([ certificate_file(ServerCert),
			    key_file(ServerKey),
			    password("apenoot1")
			  ])
		    ])
    ;   true
    ),
    start_http_proxy(HTTP_PROXY_port),
    debug(start, 'Started servers (SOCKS@~w, HTTP@~w, HTTPS@~w, PROXY@~w)',
	  [SOCKS_port, HTTP_port, HTTPS_port, HTTP_PROXY_port]).

stop_servers :-
    port(socks, SOCKS_port),
    port(http_endpoint, HTTP_port),
    stop_socks_server(SOCKS_port),
    http_stop_server(HTTP_port, []),
    (   port(https_endpoint, HTTPS_port)
    ->  http_stop_server(HTTPS_port, [])
    ;   true
    ),
    stop_http_proxy_server.

add_http_proxy :-
    http_proxy_control(HTTP_PROXY_port, _, _),
    assert(test_proxy(_, _, [proxy(localhost, HTTP_PROXY_port)])).

add_auth_http_proxy :-
    http_proxy_control(HTTP_PROXY_port, _, _),
    assert(test_proxy(_, _, [ proxy(localhost, HTTP_PROXY_port,
				    username, password)
			    ])).


:- meta_predicate
    proxy_test(0,0,-,-).

proxy_test(Init, Goal, Cleanup, SocksAttempts, HTTPAttempts) :-
    retractall(socks_proxy_connection_attempt(_)),
    retractall(http_proxy_connection_attempt(_)),
    setup_call_cleanup((start_servers, Init),
		       setup_call_cleanup(Goal,
					  true,
					  Cleanup),
		       stop_servers),
    findall(SocksInfo,
	    retract(socks_proxy_connection_attempt(SocksInfo)),
	    SocksAttempts),
    findall(HTTPInfo,
	    retract(http_proxy_connection_attempt(HTTPInfo)),
	    HTTPAttempts).


		 /*******************************
		 *           THE TESTS          *
		 *******************************/

test('Direct connection for TCP'):-
    debug(proxy, 'Test ~p', ['Direct connection for TCP']),
    port(http_endpoint, Port),
    retractall(test_proxy(_,_,_)),
    retractall(test_socks_mapping(_,_)),
    proxy_test(true,
	       tcp_connect(localhost:Port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == []).

test('All connections via SOCKS'):-
    debug(proxy, 'Test ~p', ['All connections via SOCKS']),
    port(http_endpoint, HTTP_port),
    port(socks, SOCKS_port),
    retractall(test_proxy(_,_,_)),
    retractall(test_socks_mapping(_,_)),
    assert(test_proxy(_, _, [socks(localhost, SOCKS_port)])),
    proxy_test(true,
	       tcp_connect(localhost:HTTP_port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == [localhost:HTTP_port]),
    assertion(HTTPProxyAttempts == []).

test('Some TCP connections via SOCKS'):-
    debug(proxy, 'Test ~p', ['Some TCP connections via SOCKS']),
    port(http_endpoint, HTTP_port),
    port(socks, SOCKS_port),
    port(unused, UNUSED_port),
    format(atom(HTTP_socket_URL), 'socket://localhost:~w', [HTTP_port]),
    format(atom(UNUSED_URL), 'socket://localhost:~w', [UNUSED_port]),
    retractall(test_proxy(_,_,_)),
    retractall(test_socks_mapping(_,_)),
    assert(test_proxy(UNUSED_URL, _,
		      [socks(localhost, SOCKS_port)])),
    assert(test_proxy(HTTP_socket_URL, _, [direct])),
    proxy_test(true,
	       tcp_connect(localhost:HTTP_port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == []).

test('First try SOCKS then fall back to direct'):-
    debug(proxy, 'Test ~p', ['First try SOCKS then fall back to direct']),
    port(http_endpoint, HTTP_port),
    port(unused, UNUSED_port),
    format(atom(HTTP_socket_URL), 'socket://localhost:~w', [HTTP_port]),
    retractall(test_proxy(_,_,_)),
    assert(test_proxy(HTTP_socket_URL, _,
		      [socks(localhost, UNUSED_port), direct])),
    proxy_test(true,
	       tcp_connect(localhost:HTTP_port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == []).

test('First try direct to a nonexistent-host then fall back to SOCKS'):-
    debug(proxy, 'Test ~p',
	  ['First try direct to a nonexistent-host then fall back to SOCKS']),
    port(http_endpoint, HTTP_port),
    port(socks, SOCKS_port),
    port(unused, UNUSED_port),
    retractall(test_proxy(_,_,_)),
    retractall(test_socks_mapping(_,_)),
    assert(test_socks_mapping(localhost:UNUSED_port, localhost:HTTP_port)),
    assert(test_proxy(_, _, [direct, socks(localhost, SOCKS_port)])),
    proxy_test(true,
	       tcp_connect(localhost:UNUSED_port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == [localhost:UNUSED_port]),
    assertion(HTTPProxyAttempts == []).


test('All TCP connections via HTTP'):-
    debug(proxy, 'Test ~p', ['All TCP connections via HTTP']),
    port(http_endpoint, HTTP_port),
    retractall(test_proxy(_,_,_)),
    proxy_test(add_http_proxy,
	       tcp_connect(localhost:HTTP_port, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    SocksProxyAttempts == [],
    HTTPProxyAttempts == [connect(localhost:HTTP_port)].

test('All TCP connections via HTTP but to a non-existent server',
     [cleanup(retractall(expect_failure))]) :-
    debug(proxy, 'Test ~p',
	  ['All TCP connections via HTTP but to a non-existent server']),
    port(unused, UNUSED_port),
    retractall(test_proxy(_,_,_)),
    assert(expect_failure),
    catch(proxy_test(add_http_proxy,
	         tcp_connect(localhost:UNUSED_port, StreamPair, []),
		     close(StreamPair),
		     _SocksProxyAttempts,
		     _HTTPProxyAttempts),
	  Exception,
	  true),
    assertion(nonvar(Exception)).

test('Request URL directly'):-
    debug(proxy, 'Test ~p', ['Request URL directly']),
    port(http_endpoint, HTTP_port),
    format(atom(URL), 'http://localhost:~w', [HTTP_port]),
    retractall(test_proxy(_,_,_)),
    assert(test_proxy(_, _, [direct])),
    proxy_test(true,
	       http_open(URL, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == []).

test('Request URL when all connections go via SOCKS'):-
    debug(proxy, 'Test ~p', ['Request URL when all connections go via SOCKS']),
    port(http_endpoint, HTTP_port),
    port(socks, SOCKS_port),
    format(atom(URL), 'http://localhost:~w', [HTTP_port]),
    retractall(test_proxy(_,_,_)),
    assert(test_proxy(_, _, [socks(localhost, SOCKS_port)])),
    proxy_test(true,
	       http_open(URL, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == [localhost:HTTP_port]),
    assertion(HTTPProxyAttempts == []).

test('Request URL when all connections go via HTTP'):-
    debug(proxy, 'Test ~p', ['']),
    port(http_endpoint, HTTP_port),
    format(atom(URL), 'http://localhost:~w', [HTTP_port]),
    retractall(test_proxy(_,_,_)),
    proxy_test(add_http_proxy,
	       http_open(URL, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == [get(URL)]).

test('Request invalid URL directly and expect exception rather than failure'):-
    debug(proxy, 'Test ~p',
	  ['Request invalid URL directly and expect exception \c
	    rather than failure']),
    port(unused, UNUSED_port),
    format(atom(URL), 'http://localhost:~w', [UNUSED_port]),
    retractall(test_proxy(_,_,_)),
    catch(proxy_test(true,
	         http_open(URL, StreamPair, []),
		     close(StreamPair),
		     _SocksProxyAttempts,
		     _HTTPProxyAttempts),
	  Exception,
	  true),
    assertion(nonvar(Exception)).

test('Request HTTPS url via proxy - should get HTTP CONNECT and not HTTP GET',
     condition(test_https(true))) :-
    port(https_endpoint, HTTPS_port),
    debug(proxy, 'Test ~p',
	  ['Request HTTPS url via proxy - should get HTTP CONNECT \c
	    and not HTTP GET']),
    format(atom(URL), 'https://localhost:~w', [HTTPS_port]),
    retractall(test_proxy(_,_,_)),
    proxy_test(add_http_proxy,
	       http_open(URL,
			 StreamPair,
			 [ cert_verify_hook(cert_accept_any)
			 ]),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(HTTPProxyAttempts == [connect(localhost:HTTPS_port)]),
    assertion(SocksProxyAttempts == []).

:- multifile
    http:http_connection_over_proxy/6.

http:http_connection_over_proxy(
	 proxy(ProxyHost, ProxyPort, User, Pass),
	 _Parts, _, StreamPair, Options,
	 [proxy_authorization(basic(User, Pass))|Options]) :-
    tcp_connect(ProxyHost:ProxyPort, StreamPair,
		[ bypass_proxy(true)
		| Options
		]),
    stream_pair(StreamPair, In, _Out),
    set_stream(In, record_position(false)),
    (   option(timeout(Timeout), Options)
    ->  set_stream(In, timeout(Timeout))
    ;   true
    ).

test('Test an exotic application-level proxy - http with authentication'):-
    debug(proxy, 'Test ~p',
	  ['Test an exotic application-level proxy - http with authentication']),
    port(http_endpoint, HTTP_port),
    format(atom(URL), 'http://localhost:~w', [HTTP_port]),
    retractall(test_proxy(_,_,_)),
    proxy_test(add_auth_http_proxy,
	       http_open(URL, StreamPair, []),
	       close(StreamPair),
	       SocksProxyAttempts,
	       HTTPProxyAttempts),
    assertion(SocksProxyAttempts == []),
    assertion(HTTPProxyAttempts == [authenticated_get(URL)]).

:- end_tests(proxy).

		 /*******************************
		 *        MESSAGE TRICKS        *
		 *******************************/

:- meta_predicate
    catch_messages(0, -).

catch_messages(Goal, Messages) :-
    nb_setval(messages, []),
    thread_self(Me),
    setup_call_cleanup(assert((user:message_hook(Msg, _, _) :-
				    catch_message(Me, Msg)),
			      Ref),
		       once(Goal),
		       collect_messages(Messages, Ref)).

catch_message(Me, Msg) :-
    thread_self(Me),
    !,
    nb_getval(messages, L0),
    duplicate_term(Msg, Copy),
    nb_linkval(messages, [Copy|L0]).

collect_messages(Messages, Ref) :-
    erase(Ref),
    nb_getval(messages, L),
    nb_delete(messages),
    reverse(L, Messages).
