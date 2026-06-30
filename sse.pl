/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(sse,
          [ sse_open/0,
            sse_open/1,                 % +Options
            sse_send/1,                 % +Event
            sse_send/2,                 % +Stream, +Event
            sse_comment/1,              % +Comment
            sse_comment/2               % +Stream, +Comment
          ]).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(http/http_cors)).

:- predicate_options(sse_open/1, 1,
                     [ headers(list),
                       retry(number)
                     ]).

/** <module> Server-Sent Events (SSE)

This library provides a small server-side   helper  for the [Server-Sent
Events](https://html.spec.whatwg.org/multipage/server-sent-events.html)
wire format. SSE is a one-way (server to client) push channel that lives
inside a normal HTTP response   with  `Content-Type: text/event-stream`.
Each event is a sequence of `Field:   Value` lines terminated by a blank
line.

Typical use, with the handler running on   a separate thread so that the
worker pool is not blocked:

```
:- use_module(library(http/sse)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(events), events,
                [ spawn([]), time_limit(infinite) ]).

events(_Request) :-
    sse_open,
    between(1, infinite, Min),
        sse_send(_{event: minute, data: Min}),
        sleep(60),
        fail.
```

The underlying machinery is provided  by   the  CGI output stream, which
recognises `Content-Type: text/event-stream` and switches   into  a mode
where every flushed write is sent unbuffered  to the client. The present
library hides the wire  format,  multi-line   `data`  handling  and  the
response headers needed to defeat HTTP  intermediaries that buffer small
responses.

If the client disconnects, the next write   raises an I/O exception that
typically terminates the event-producing loop.

A single SSE response is single-writer;  multiple threads writing to the
same response will interleave bytes and corrupt   the stream. Fan out to
multiple clients by giving each connection its own writer.

Browsers loading  the  page  from   a  different  origin  than  the  SSE
endpoint  need  Cross-Origin  Resource  Sharing  (CORS).  sse_open/0,1
calls cors_enable/0, so setting the `http:cors` setting (see
library(http/http_cors)) enables it.

@see library(http/websocket) for bi-directional messaging.
@see library(http/http_cors) for enabling CORS.
*/

%!  sse_open is det.
%!  sse_open(+Options) is det.
%
%   Emit the HTTP response headers that turn the current response into a
%   Server-Sent Events stream and write  the   blank  line that ends the
%   headers. After this  call  the  CGI   stream  is  in  `event_stream`
%   transfer mode and each sse_send/1 or   sse_send/2  is flushed to the
%   client. The mandatory headers emitted are:
%
%     - `Content-Type: text/event-stream`
%     - `Cache-Control: no-cache`
%     - `X-Accel-Buffering: no` (disables nginx response buffering)
%
%   cors_enable/0  is  called  after  the  mandatory  headers,  so  the
%   `http:cors` setting controls whether an `Access-Control-Allow-Origin`
%   header is emitted. Browser-based `EventSource` clients on a different
%   origin need this; non-browser clients do not.
%
%   Options:
%
%     - headers(+List)
%       Additional response headers to emit, given as a list of
%       `Name-Value` pairs.  Both Name and Value are written with
%       format/3 using `~w`.
%     - retry(+Seconds)
%       After the headers, emit an initial `retry: Millis\n\n` field,
%       which sets the client's reconnect delay in milliseconds,
%       computed from Seconds.

sse_open :-
    sse_open([]).

sse_open(Options) :-
    current_output(Out),
    format(Out, 'Content-Type: text/event-stream\r\n', []),
    format(Out, 'Cache-Control: no-cache\r\n', []),
    format(Out, 'X-Accel-Buffering: no\r\n', []),
    format(Out, 'Connection: close\r\n', []),
    cors_enable,
    (   option(headers(Extras), Options)
    ->  must_be(list, Extras),
        forall(member(Name-Value, Extras),
               format(Out, '~w: ~w\r\n', [Name, Value]))
    ;   true
    ),
    format(Out, '\r\n', []),
    (   option(retry(Retry), Options)
    ->  (   Retry < 0
        ->  domain_error(retry, Retry)
        ;   Millis is round(Retry*1000)
        ),
        format(Out, 'retry: ~d~n~n', [Millis]),
        flush_output(Out)
    ;   true
    ).

%!  sse_send(+Event) is det.
%!  sse_send(+Stream, +Event) is det.
%
%   Emit one or more SSE events and flush the stream.  Event is one of:
%
%     - An atom or string. Shorthand for emitting a single event with
%       just a `data:` field, splitting the text on line breaks.
%     - A dict with any of the optional keys `event`, `data`, `id`,
%       `retry` and `comment`. Unknown keys raise a domain_error/2.
%         - `event` (atom or string) becomes one `event:` line.
%         - `id` (atom, string or integer) becomes one `id:` line.
%         - `retry` (non-negative integer) becomes one `retry:` line.
%         - `comment` (atom, string or list) becomes one or more `:`
%           lines (an SSE comment) before the rest of the event.
%         - `data` may be an atom, string, integer or list. Atoms and
%           strings are split on `\n` (with an optional `\r`) so that
%           multi-line text becomes multiple `data:` lines. A list is
%           taken to be a list of lines (which should not themselves
%           contain newlines).
%     - A list of any combination of the above. The whole list is
%       flushed once at the end.
%
%   The empty event (a dict containing none   of the recognised keys) is
%   not allowed and raises a domain_error/2.

sse_send(Event) :-
    current_output(Out),
    sse_send(Out, Event).

sse_send(Stream, Events) :-
    is_list(Events),
    !,
    forall(member(E, Events), write_event(Stream, E)),
    flush_output(Stream).
sse_send(Stream, Event) :-
    write_event(Stream, Event),
    flush_output(Stream).

write_event(Stream, Text) :-
    (   atom(Text)
    ;   string(Text)
    ),
    !,
    emit_data(Stream, Text),
    nl(Stream).
write_event(Stream, Dict) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    validate_event_pairs(Pairs),
    emit_in_order(Stream, [comment, event, id, retry, data], Pairs),
    nl(Stream).
write_event(_, Event) :-
    domain_error(sse_event, Event).

emit_in_order(_, [], _).
emit_in_order(Stream, [K|Ks], Pairs) :-
    (   memberchk(K-V, Pairs)
    ->  emit_field(Stream, K, V)
    ;   true
    ),
    emit_in_order(Stream, Ks, Pairs).

validate_event_pairs([]) :-
    !,
    domain_error(sse_event, []).
validate_event_pairs(Pairs) :-
    forall(member(K-_, Pairs), valid_key(K)).

valid_key(K) :-
    known_key(K),
    !.
valid_key(K) :-
    domain_error(sse_event_key, K).

known_key(event).
known_key(data).
known_key(id).
known_key(retry).
known_key(comment).

emit_field(Stream, comment, V) :-
    !,
    emit_comment(Stream, V).
emit_field(Stream, event, V) :-
    !,
    format(Stream, 'event: ~w~n', [V]).
emit_field(Stream, id, V) :-
    !,
    format(Stream, 'id: ~w~n', [V]).
emit_field(Stream, retry, V) :-
    !,
    (   V < 0
    ->  domain_error(retry, V)
    ;   true
    ),
    Millis is round(V*1000),
    format(Stream, 'retry: ~d~n', [Millis]).
emit_field(Stream, data, V) :-
    !,
    emit_data(Stream, V).

emit_data(Stream, V) :-
    to_lines(V, Lines0),
    (   Lines0 == []
    ->  Lines = [""]
    ;   Lines = Lines0
    ),
    forall(member(L, Lines), emit_field_line(Stream, "data", L)).

emit_comment(Stream, V) :-
    to_lines(V, Lines0),
    (   Lines0 == []
    ->  Lines = [""]
    ;   Lines = Lines0
    ),
    forall(member(L, Lines), emit_field_line(Stream, "", L)).

emit_field_line(Stream, Name, "") :-
    !,
    format(Stream, '~w:~n', [Name]).
emit_field_line(Stream, Name, Line) :-
    format(Stream, '~w: ~w~n', [Name, Line]).

%!  to_lines(+Input, -Lines) is det.
%
%   Normalise Input to a list of lines. Lists are taken as-is. Atoms and
%   strings are split on `\n`; a trailing   `\r`  on a piece is stripped
%   (so `\r\n` line endings work). A trailing empty line, as produced by
%   a final `\n`, is stripped.

to_lines(L, L) :-
    is_list(L),
    !.
to_lines(Text, Lines) :-
    format(string(S), '~w', [Text]),
    split_string(S, "\n", "\r", All),
    strip_trailing_empty(All, Lines).

strip_trailing_empty(Lines0, Lines) :-
    append(Lines, [""], Lines0),
    !.
strip_trailing_empty(Lines, Lines).

%!  sse_comment(+Comment) is det.
%!  sse_comment(+Stream, +Comment) is det.
%
%   Emit Comment as one or more SSE   comment lines (`:` lines) followed
%   by a blank line and  flush  the   stream.  Comments  are  ignored by
%   clients and are mainly useful  as   a  keep-alive  heartbeat through
%   intermediaries that close idle  connections.   Comment  is  an atom,
%   string or list of lines; multi-line text is split as for `data`.

sse_comment(Comment) :-
    current_output(Out),
    sse_comment(Out, Comment).

sse_comment(Stream, Comment) :-
    emit_comment(Stream, Comment),
    nl(Stream),
    flush_output(Stream).
