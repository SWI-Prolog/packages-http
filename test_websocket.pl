:- module(test_websocket,
	  [ test_websocket/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(library(plunit)).
:- use_module(library(http/websocket)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

test_websocket :-
	run_tests([ serialization,
		    http
		  ]).


:- begin_tests(serialization).

test(text, Reply == [ websocket{opcode:text,  format:string, data:"Hello world"}
		    ]) :-
	ws_loop_close([text("Hello world")], Reply, []).
test(unicode, Reply == [ websocket{opcode:text,  format:string, data:Data}
		       ]) :-
	unicode_data(Data),
	ws_loop_close([text(Data)], Reply, []).
test(prolog, Reply == [ websocket{opcode:text,  format:prolog, data:hello(world)}
		      ]) :-
	ws_loop_close([prolog(hello(world))], Reply, [format(prolog)]).
test(json, Reply =@= [ websocket{opcode:text,  format:json,   data:_{hello:world}}
		     ]) :-
	ws_loop_close([json(_{hello:world})], Reply,
		      [ format(json),
			value_string_as(atom)
		      ]).
test(split, Reply == [ websocket{opcode:text,  format:string, data:"0123456789"}
		     ]) :-
	ws_loop_close([text("0123456789"), close], Reply,
		      [ buffer_size(5)
		      ]).

:- end_tests(serialization).

:- begin_tests(http).

test(echo, Reply == [ websocket{opcode:text,  format:string, data:"Hello world"},
		      websocket{opcode:text,  format:string, data:Unicode},
		      websocket{opcode:close, code:1005, format:string, data:"Ciao"}
		    ]) :-
	Address = localhost:Port,
	unicode_data(Unicode),
	setup_call_cleanup(
	    server(Address),
	    client(Port,
		   [ text("Hello world"),
		     text(Unicode),
		     close(1005, "Ciao")
		   ],
		   Reply),
	    http_stop_server(Address, [])).

:- end_tests(http).

unicode_data(
    "\u0420\u0443\u0441\u0441\u043A\u0438\u0439 \u044F\u0437\u044B\u043A").


		 /*******************************
		 *    SERIALIZATION SUPPORT	*
		 *******************************/

ws_loop_close(Messages, Result, Options) :-
	append(Messages, [close], Messages1),
	ws_loop(Messages1, Result0, Options),
	once(append(Result, [Close], Result0)),
	assertion(Close == websocket{opcode:close,
				     format:string,
				     code:1000,
				     data:""}).

ws_loop(Messages, Result, Options) :-
	is_list(Messages), !,
	setup_call_cleanup(
	    tmp_file(ws, File),
	    ( ws_write_file(File, Messages, Options),
	      ws_read_file(File, Result, Options)
	    ),
	    delete_file(File)).
ws_loop(Message, Result, Options) :-
	ws_loop([Message], Result, Options).


ws_write_file(File, Messages, Options) :-
	option(close_parent(true), Options, true), !,
	open(File, write, Out, [type(binary)]),
	ws_write_stream(Out, Messages, Options).
ws_write_file(File, Messages, Options) :-
	setup_call_cleanup(
	    open(File, write, Out, [type(binary)]),
	    ws_write_stream(Out, Messages, Options),
	    close(Out)).

ws_write_stream(Stream, Messages, Options) :-
	setup_call_cleanup(
	    ws_open(Stream, WsStream, Options),
	    maplist(ws_send(WsStream), Messages),
	    close(WsStream)).


		 /*******************************
		 *	      READ		*
		 *******************************/

ws_read_file(File, Message, Options) :-
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    ws_read_stream(In, Message, Options),
	    close(In)).

ws_read_stream(Stream, Messages, Options) :-
	setup_call_cleanup(
	    ws_open(Stream, WsStream, [close_parent(false)]),
	    ws_receive_all(WsStream, Messages, Options),
	    close(WsStream)).

ws_receive_all(WsStream, Messages, Options) :-
	ws_receive(WsStream, H, Options),
	(   H == end_of_file
	->  Messages = []
	;   Messages = [H|T],
	    (	H.opcode == close
	    ->  T = []
	    ;   ws_receive_all(WsStream, T, Options)
	    )
	).

		 /*******************************
		 *	       HTTP		*
		 *******************************/

:- http_handler(root(echo),
		http_upgrade_to_websocket(echo,
					  [ subprotocols([echo])
					  ]),
		[spawn([])]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

echo(WebSocket) :-
	ws_receive(WebSocket, Message),
	debug(websocket, 'Got ~p', [Message]),
	ws_send(WebSocket, Message),
	(   Message.opcode == close
	->  true
	;   echo(WebSocket)
	).

client(Port, Messages, Reply) :-
	format(string(URL), 'ws://localhost:~d/echo', [Port]),
	http_open_websocket(URL, WebSocket, []),
	maplist(ws_send(WebSocket), Messages),
	ws_receive_all(WebSocket, Reply, []),
	ws_close(WebSocket, 1000, "bye").

