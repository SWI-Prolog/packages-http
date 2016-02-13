/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_error,
	  [
	  ]).
:- use_module(library(prolog_stack)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).

/** <module> Decorate uncaught HTTP exceptions with stack-trace

This module decorates uncaught exceptions of the   user code with a full
stack-trace and sends error reports to the Prolog console. The behaviour
can be controlled by

  - nodebug(http(error))
    After disabling the http(error) debug channal, errors are only sent
    to the client.  See nodebug/1 and debug/1.
  - set_setting(http:client_backtrace, false)
    Stop sending stack traces to the client. Note that sending the stack
    trace to the client simplifies debugging, it also provides clues to
    hackers on how to compromise your site. The more information you
    give them, the easier it is to break into your server!  See
    set_setting/2 and set_setting_default/2.
*/

:- setting(http:client_backtrace, boolean, true,
	   'Make backtrace visible to the client').


		 /*******************************
		 *     LOG ERRORS TO STDERR	*
		 *******************************/

:- debug(http(error)).

:- listen(http(Message),
	  http_listen(Message)).

:- dynamic
	saved_request/2.

http_listen(_) :-
	\+ debugging(http(error)), !.
http_listen(request_start(Id, Request)) :- !,
	asserta(saved_request(Id, Request)).
http_listen(request_finished(Id, Code, Status, _CPU, _Bytes)) :-
	retract(saved_request(Id, Request)), !,
	Code >= 400,
	memberchk(path(Path), Request),
	memberchk(method(Method), Request),
	upcase_atom(Method, UMethod),
	reply_status(Status, Reply),
	debug(http(error),
	      '~w ~w: [~w] ~w', [UMethod, Path, Code, Reply]).

reply_status(Status, Reply) :-
	map_exception(Status, Reply), !.
reply_status(Status, Message) :-
	message_to_string(Status, Message).

map_exception(http_reply(bytes(ContentType,Bytes),_), bytes(ContentType,L)) :-
        string_length(Bytes, L).	% also does lists
map_exception(http_reply(Reply), Reply).
map_exception(http_reply(Reply, _), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
	      error(404, Location)).


		 /*******************************
		 *     DECORATE STACK TRACES	*
		 *******************************/

:- dynamic prolog_stack:stack_guard/1.
:- multifile prolog_stack:stack_guard/1.

prolog_stack:stack_guard(httpd_wrapper:wrapper/5).
prolog_stack:stack_guard(httpd_wrapper:handler_with_output_to/5).

