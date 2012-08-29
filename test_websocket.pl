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
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

test_websocket :-
	run_tests([ serialization,
		    http
		  ]).


:- begin_tests(serialization).

test(text, Reply == [ websocket{opcode:text,  data:"Hello world"},
		      websocket{opcode:close, data:""}
		    ]) :-
	ws_loop([text("Hello world"), close], Reply, []).
test(unicode, Reply == [ websocket{opcode:text,  data:Data},
			 websocket{opcode:close, data:""}
		       ]) :-
	unicode_data(Data),
	ws_loop([text(Data), close], Reply, []).
test(split, Reply == [ websocket{opcode:text,  data:"0123456789"},
		       websocket{opcode:close, data:""}
		     ]) :-
	ws_loop([text("0123456789"), close], Reply,
	        [ buffer_size(5)
		]).

:- end_tests(serialization).

:- begin_tests(http).

test(echo, Reply == [ websocket{opcode:text,  data:"Hello world"},
		      websocket{opcode:text,  data:Unicode},
		      websocket{opcode:close, data:"Ciao"}
		    ]) :-
	Address = localhost:Port,
	unicode_data(Unicode),
	setup_call_cleanup(
	    server(Address),
	    client(Port,
		   [ text("Hello world"),
		     text(Unicode),
		     close("Ciao")
		   ],
		   Reply),
	    http_stop_server(Address, [])).

:- end_tests(http).

unicode_data(
    "\u0420\u0443\u0441\u0441\u043A\u0438\u0439 \u044F\u0437\u044B\u043A").


		 /*******************************
		 *    SERIALIZATION SUPPORT	*
		 *******************************/

ws_loop(Messages, Result, Options) :-
	is_list(Messages), !,
	setup_call_cleanup(
	    tmp_file(ws, File),
	    ( ws_write_file(File, Messages, Options),
	      ws_read_file(File, Result)
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

ws_read_file(File, Message) :-
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    ws_read_stream(In, Message),
	    close(In)).

ws_read_stream(Stream, Messages) :-
	setup_call_cleanup(
	    ws_open(Stream, WsStream, [close_parent(false)]),
	    ws_receive_all(WsStream, Messages),
	    close(WsStream)).

ws_receive_all(WsStream, Messages) :-
	ws_receive(WsStream, H),
	(   H == end_of_file
	->  Messages = []
	;   Messages = [H|T],
	    (	H.opcode == close
	    ->  T = []
	    ;   ws_receive_all(WsStream, T)
	    )
	).

		 /*******************************
		 *	       HTTP		*
		 *******************************/

:- http_handler(root(echo), http_upgrade_to_websocket(echo, []), [spawn([])]).

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
	ws_receive_all(WebSocket, Reply),
	ws_close(WebSocket, "bye").

