:- module(test_multipart,
          [ test_multipart/0,
	    data_to_mf/3,
	    dump_mf/1,
	    mf_to_data/3
          ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(library(plunit)).
:- use_module(library(memfile)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/mimepack)).
:- use_module(library(http/http_header)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).

test_multipart :-
        run_tests([ multipart
                  ]).

data_to_mf(Data, MF, Boundary) :-
	new_memory_file(MF),
	setup_call_cleanup(
	    open_memory_file(MF, write, Out, [encoding(octet)]),
	    mime_pack(Data, Out, Boundary),
	    close(Out)).

dump_mf(MF) :-
	setup_call_cleanup(
	    open_memory_file(MF, read, In, [encoding(octet)]),
	    copy_stream_data(In, current_output),
	    close(In)).

mf_to_data(MF, Boundary, Data) :-
	mf_to_data(MF, Boundary, Data, []).

mf_to_data(MF, Boundary, Data, Options) :-
	setup_call_cleanup(
	    open_memory_file(MF, read, In, [encoding(octet)]),
	    process_multifile(In, Boundary, Data, Options),
	    close(In)).

process_multifile(In, Boundary, Data, Options) :-
	(   option(in_buffer_size(BSize), Options)
	->  set_stream(In, buffer_size(BSize))
	;   true
	),
	setup_call_cleanup(
	    multipart_open(In, Stream, [boundary(Boundary)]),
	    process_parts(Stream, Data, Options),
	    close(Stream)).

process_parts(Stream, Parts, Options) :-
	(   option(data_buffer_size(BSize), Options)
	->  set_stream(Stream, buffer_size(BSize))
	;   true
	),
	process_parts(Stream, Parts).

process_parts(Stream, [part(Header, String)|More]) :-
	http_read_header(Stream, HTTPHeader),
	part_header(HTTPHeader, Header),
	read_string(Stream, _, String),
	debug(multipart(content), 'Got ~q~n', [String]),
	(   multipart_open_next(Stream)
	->  process_parts(Stream, More)
	;   More = []
	).

part_header([content_disposition(disposition(Type, Args))], Args) :-
	assertion(Type == 'form-data').

long_string(N, String) :-
	numlist(1,N,List),
	term_string(List, String).

:- begin_tests(multipart).

test(one, Data == [part([name=n], "v")]) :-
	data_to_mf([n=v], MF, B),
	mf_to_data(MF, B, Data).
test(two, Data == [part([name=n], "v"), part([name=x], "y")]) :-
	data_to_mf([n=v,x=y], MF, B),
	mf_to_data(MF, B, Data).
test(long, Data == [part([name=n], String), part([name=x], "y")]) :-
	long_string(10000, String),
	data_to_mf([n=String,x=y], MF, B),
	mf_to_data(MF, B, Data).
test(multi_part_buffer_size) :-
	Data = [n="v",x="y"],
	forall(between(1, 200, BS),
	       loop(Data, [in_buffer_size(BS)])).
test(multi_part_buffer_size_long) :-
	long_string(100, String),
	Data = [n=String,x="y"],
	forall(between(1, 200, BS),
	       loop(Data, [in_buffer_size(BS)])).
test(data_buffer_size) :-
	Data = [n="v",x="y"],
	forall(between(1, 200, BS),
	       loop(Data, [data_buffer_size(BS)])).
test(data_buffer_size_long) :-
	long_string(100, String),
	Data = [n=String,x="y"],
	forall(between(1, 200, BS),
	       loop(Data, [data_buffer_size(BS)])).

loop(Data, Options) :-
	data_to_mf(Data, MF, B),
	mf_to_data(MF, B, DataOut, Options),
	maplist(ok, Data, DataOut).

ok(Name=Value, part([name=Name], Value)).

:- end_tests(multipart).
