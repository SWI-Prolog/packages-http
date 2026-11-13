:- module(test_websocket,
          [ test_websocket/0
          ]).

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
test(json, Reply =@= [ websocket{opcode:text,  format:json,   data: #{hello:world}}
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
test(split_unicode, Reply == [ websocket{opcode:text, format:string, data:Data}
                             ]) :-
    unicode_data(Data),                 % fragments split UTF-8 sequences
    ws_loop_close([text(Data), close], Reply,
                  [ buffer_size(5)
                  ]).
test(invalid_utf8, error(syntax_error(encoding(_)))) :-
    ws_read_frame([0x81, 3, 0'a, 0xC0, 0xAF], _).    % overlong "/"
test(surrogate, error(syntax_error(encoding(_)))) :-
    ws_read_frame([0x81, 4, 0'a, 0xED, 0xA0, 0x80], _).
test(truncated_utf8, error(syntax_error(encoding(_)))) :-
    ws_read_frame([0x81, 2, 0'a, 0xC3], _).
test(close_code, [Code, Data] == [1000, "ok"]) :-
    ws_read_frame([0x88, 4, 0x03, 0xE8, 0'o, 0'k], Message),
    Code = Message.code,
    Data = Message.data.
test(close_one_byte, error(syntax_error(websocket(close_payload)))) :-
    ws_read_frame([0x88, 1, 0x03], _).  % a close code takes 2 bytes
test(binary_is_bytes, Data == [0'a, 0xC0, 0xAF]) :-
    ws_read_frame([0x82, 3, 0'a, 0xC0, 0xAF], Message),
    string_codes(Message.data, Data).

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
        http_stop_server(Port, [])).
test(bad_close_closes_1002, Code == 1002) :-
    Address = localhost:Port,
    setup_call_cleanup(
        server(Address),
        send_raw(Port, 8, [0x03], Code),
        http_stop_server(Port, [])).
test(invalid_utf8_closes_1007, Code == 1007) :-
    Address = localhost:Port,
    setup_call_cleanup(
        server(Address),
        send_invalid_utf8(Port, Code),
        http_stop_server(Port, [])).

:- end_tests(http).

unicode_data(
    "\u0420\u0443\u0441\u0441\u043A\u0438\u0439 \u044F\u0437\u044B\u043A").


                 /*******************************
                 *    SERIALIZATION SUPPORT     *
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
    is_list(Messages),
    !,
    setup_call_cleanup(
        tmp_file(ws, File),
        ( ws_write_file(File, Messages, Options),
          ws_read_file(File, Result, Options)
        ),
        delete_file(File)).
ws_loop(Message, Result, Options) :-
    ws_loop([Message], Result, Options).


ws_write_file(File, Messages, Options) :-
    option(close_parent(true), Options, true),
    !,
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
                 *            READ              *
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

%!  ws_read_frame(+Bytes, -Message) is det.
%
%   Read Message from the raw frame Bytes.

ws_read_frame(Bytes, Message) :-
    setup_call_cleanup(
        tmp_file(ws, File),
        ( setup_call_cleanup(
              open(File, write, Out, [type(binary)]),
              maplist(put_byte(Out), Bytes),
              close(Out)),
          setup_call_cleanup(
              open(File, read, In, [type(binary)]),
              setup_call_cleanup(
                  ws_open(In, WsStream, [close_parent(false)]),
                  ws_receive(WsStream, Message),
                  close(WsStream)),
              close(In))
        ),
        delete_file(File)).

ws_receive_all(WsStream, Messages, Options) :-
    ws_receive(WsStream, H, Options),
    (   H == end_of_file
    ->  Messages = []
    ;   Messages = [H|T],
        (   H.opcode == close
        ->  T = []
        ;   ws_receive_all(WsStream, T, Options)
        )
    ).

                 /*******************************
                 *             HTTP             *
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

%   Send a text message that is not UTF-8.  The server must close the
%   connection with code 1007.  The low-level message API used by
%   send_raw/4 is not exported, hence the qualified calls.

send_invalid_utf8(Port, Code) :-
    send_raw(Port, 1, [0'a, 0xC0, 0xAF], Code).     % 1: text

%   Send a message with OpCode and the bytes Payload; Code is the close
%   code the server replies with.

send_raw(Port, OpCode, Payload, Code) :-
    format(string(URL), 'ws://localhost:~d/echo', [Port]),
    http_open_websocket(URL, WebSocket, []),
    stream_pair(WebSocket, _, Out),
    websocket:ws_start_message(Out, OpCode, 0),
    set_stream(Out, encoding(octet)),
    maplist(put_byte(Out), Payload),
    websocket:ws_send(Out),
    ws_receive(WebSocket, Reply),
    Code = Reply.code,
    close(WebSocket).

client(Port, Messages, Reply) :-
    format(string(URL), 'ws://localhost:~d/echo', [Port]),
    http_open_websocket(URL, WebSocket, []),
    maplist(ws_send(WebSocket), Messages),
    ws_receive_all(WebSocket, Reply, []),
    ws_close(WebSocket, 1000, "bye").

