/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2024 SWI-Prolog Solutions b.v.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_server_health, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_json)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).

:- http_handler(root(health), server_health, [id(server_health), priority(-10)]).

/** <module> HTTP Server health statistics

This module defines an HTTP handler for ``/health``. The handler returns
a JSON document  with  elementary  health   statistics  on  the  running
instance. The location can be changed  using http_handler/3. Keys may be
added using additional clauses for health/2 or hidden using hide/1.

This library exports no predicates. It only  defines an HTTP handler and
defines two multifile predicates (health/2 and   hide/1)  to control the
information presented.
*/

%!  server_health(+Request)
%
%   HTTP handler that replies with  the   overall  health of the server.
%   Returns a JSON object from all solutions of health/2.

server_health(_Request) :-
    get_server_health(Health),
    reply_json(Health).

get_server_health(Health) :-
    findall(Key-Value, health(Key, Value), Pairs),
    dict_pairs(Health, health, Pairs).

%!  health(-Key, -Value) is nondet.
%
%   Multifile extensible. True when  Key/Value  can   be  reported  as a
%   health statistics. Keys may  be  added   by  adding  clauses to this
%   multifile predicate. Keys may be   filtered using hide/1. Predefined
%   Key values are:
%
%     - up
%       Defined to be `true`.
%     - epoch
%       Starting time of the server in seconds after Jan 1, 1970 UTC.
%     - requests
%       Number of HTTP requests processed.
%     - bytes_sent
%       Number of bytes send in reply to HTTP requests.
%     - open_files
%       Number of open file streams.  This includes physical files as
%       well as sockets (except for Windows).
%     - loadavg
%       An array holding the load average over the last [1,5,15]
%       minutes.  This key is only supported on Linux machines.  It
%       is based on ``/proc/loadavg``
%     - heap
%       When compiled with TCMalloc, this provides two properties:
%       - inuse: Bytes
%         Total amount of in-use memory in bytes
%       - size: Bytes
%         Same as `inuse`, but including the TCMalloc overhead and
%         (thus) memory that has been freed and is not (yet) reused.
%
%   @arg Key is the name of the JSON key.  Must be an atom
%   @arg Value is the Prolog representation for a JSON (dict) value.

:- multifile health/2.

health(up, true).
health(epoch, Epoch) :-
    http_server_property(_, start_time(Epoch)).
health(requests, RequestCount) :-
    cgi_statistics(requests(RequestCount)).
health(bytes_sent, BytesSent) :-
    cgi_statistics(bytes_sent(BytesSent)).
health(open_files, Streams) :-
    aggregate_all(count, N, stream_property(_, file_no(N)), Streams).
health(loadavg, LoadAVG) :-
    access_file('/proc/loadavg', exist),
    catch(setup_call_cleanup(
              open('/proc/loadavg', read, In),
              read_string(In, _, String),
              close(In)),
	      _, fail),
    split_string(String, " ", " ", [One,Five,Fifteen|_]),
    maplist(number_string, LoadAVG, [One,Five,Fifteen]).
:- if(current_predicate(malloc_property/1)).
health(heap, json{inuse:InUse, size:Size}) :-
    malloc_property('generic.current_allocated_bytes'(InUse)),
    malloc_property('generic.heap_size'(Size)).
:- endif.

%!  hide(?Key) is nondet.
%
%   Multifile hook. If true for  a   specific  Key, hide this statistics
%   from the output. This may be used to hide keys that are considered a
%   security risk.

:- multifile hide/1.
