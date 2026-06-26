:- module(test_sse,
          [ test_sse/0
          ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(library(plunit)).
:- use_module(library(http/sse)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(error)).

test_sse :-
    run_tests([ sse_format,
                sse_http
              ]).


                 /*******************************
                 *           FORMAT             *
                 *******************************/

:- begin_tests(sse_format).

test(string, Frame == "data: hello\n\n") :-
    with_output_to(string(Frame), sse_send("hello")).
test(atom, Frame == "data: hello\n\n") :-
    with_output_to(string(Frame), sse_send(hello)).
test(dict_data, Frame == "data: hello\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{data: "hello"})).
test(dict_event_data,
     Frame == "event: tick\ndata: 1\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{event: tick, data: 1})).
test(dict_all_fields,
     Frame == "event: tick\nid: 42\nretry: 5000\ndata: hello\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{event: tick, id: 42,
                              retry: 5, data: "hello"})).
test(multiline_lf,
     Frame == "data: a\ndata: b\ndata: c\n\n") :-
    with_output_to(string(Frame),
                   sse_send("a\nb\nc")).
test(multiline_crlf,
     Frame == "data: a\ndata: b\n\n") :-
    with_output_to(string(Frame),
                   sse_send("a\r\nb")).
test(trailing_newline_stripped,
     Frame == "data: hello\n\n") :-
    with_output_to(string(Frame),
                   sse_send("hello\n")).
test(data_list,
     Frame == "data: one\ndata: two\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{data: ["one", "two"]})).
test(empty_data,
     Frame == "data:\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{data: ""})).
test(comment_only,
     Frame == ": heartbeat\n\n") :-
    with_output_to(string(Frame),
                   sse_send(_{comment: "heartbeat"})).
test(comment_helper,
     Frame == ": ping\n\n") :-
    with_output_to(string(Frame), sse_comment("ping")).
test(list_of_events,
     Frame == "event: a\ndata: 1\n\nevent: b\ndata: 2\n\n") :-
    with_output_to(string(Frame),
                   sse_send([ _{event: a, data: 1},
                              _{event: b, data: 2}
                            ])).
test(unknown_key_rejected,
     [error(domain_error(sse_event_key, weird))]) :-
    with_output_to(string(_),
                   sse_send(_{weird: 1, data: 2})).
test(empty_dict_rejected,
     [error(domain_error(sse_event, _))]) :-
    with_output_to(string(_),
                   sse_send(_{})).
test(bad_retry_rejected,
     [error(domain_error(retry, -1))]) :-
    with_output_to(string(_),
                   sse_send(_{retry: -1, data: x})).

:- end_tests(sse_format).


                 /*******************************
                 *             HTTP             *
                 *******************************/

:- begin_tests(sse_http).

:- http_handler(root(events), sse_events_handler,
                [ spawn([]), time_limit(infinite) ]).

sse_events_handler(_Request) :-
    sse_open([ headers(['X-Test'-yes]) ]),
    sse_send(_{event: start, data: "ready"}),
    sse_send(_{event: tick, id: 1, data: "alpha"}),
    sse_send(_{event: tick, id: 2, data: "line one\nline two"}),
    sse_comment("bye").

test(round_trip,
     [setup(http_server(http_dispatch, [port(Port)])),
      cleanup(http_stop_server(Port, []))]) :-
    format(string(URL), 'http://localhost:~d/events', [Port]),
    http_open(URL, In, [header(content_type, CT)]),
    call_cleanup(read_string(In, _, Body), close(In)),
    assertion(sub_string(CT, _, _, _, "text/event-stream")),
    parse_sse(Body, Events),
    assertion(Events == [ event{event:"start", data:"ready"},
                          event{event:"tick",  id:"1", data:"alpha"},
                          event{event:"tick",  id:"2",
                                data:"line one\nline two"},
                          event{comment:"bye"}
                        ]).

:- end_tests(sse_http).


%!  parse_sse(+Body:string, -Events:list) is det.
%
%   Minimal parser for the SSE wire format used by the round-trip
%   test. Splits Body into blocks separated by blank lines and turns
%   each block into a dict that mirrors the keys used to produce the
%   event. `data` fields spanning multiple lines are re-joined with
%   `\n`.

parse_sse(Body, Events) :-
    split_string(Body, "\n", "\r", Lines),
    blocks(Lines, Blocks),
    maplist(block_to_event, Blocks, Events0),
    exclude(==(none), Events0, Events).

blocks([], []) :- !.
blocks(Lines, [Block|Blocks]) :-
    take_block(Lines, Block, Rest),
    blocks(Rest, Blocks).

take_block([], [], []) :- !.
take_block([""|Rest], [], Rest) :- !.
take_block([L|Rest], [L|Block], Tail) :-
    take_block(Rest, Block, Tail).

block_to_event([], none) :- !.
block_to_event(Lines, Event) :-
    foldl(parse_line, Lines, _{}, Dict0),
    finalise(Dict0, Event).

parse_line(Line, In, Out) :-
    sub_string(Line, 0, 1, _, ":"),
    !,
    sub_string(Line, 2, _, 0, Text),
    Out = In.put(comment, Text).
parse_line(Line, In, Out) :-
    sub_string(Line, B, _, _, ": "),
    !,
    sub_string(Line, 0, B, _, Field),
    FieldStart is B + 2,
    sub_string(Line, FieldStart, _, 0, Value),
    atom_string(K, Field),
    add_field(K, Value, In, Out).

add_field(data, V, In, Out) :-
    !,
    (   get_dict(data, In, Old)
    ->  string_concat(Old, "\n", Tmp),
        string_concat(Tmp, V, New)
    ;   New = V
    ),
    Out = In.put(data, New).
add_field(K, V, In, Out) :-
    Out = In.put(K, V).

finalise(Dict, Event) :-
    dict_pairs(Dict, _, Pairs),
    dict_pairs(Event, event, Pairs).
