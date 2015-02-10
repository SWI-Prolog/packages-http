/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2015, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_exception,
	  [ map_exception_to_http_status/4
	  ]).

/** <module> Internal module of the HTTP server

@see	http_header.pl, http_wrapper.pl
*/

%%	map_exception_to_http_status(+Exception, -Reply, -HdrExtra, -Context)
%
%	Map certain defined  exceptions  to   special  reply  codes. The
%	http(not_modified)   provides   backward     compatibility    to
%	http_reply(not_modified).

map_exception_to_http_status(http(not_modified),
	      not_modified,
	      [connection('Keep-Alive')],
              []) :- !.
map_exception_to_http_status(http_reply(Reply),
	      Reply,
	      [connection(Close)],
              []) :- !,
	(   keep_alive(Reply)
	->  Close = 'Keep-Alive'
	;   Close = close
	).
map_exception_to_http_status(http_reply(Reply, HdrExtra0),
	      Reply,
	      HdrExtra,
              Context) :- !,
        map_exception_to_http_status(http_reply(Reply, HdrExtra0, []),
                                     Reply,
                                     HdrExtra,
                                     Context).

map_exception_to_http_status(http_reply(Reply, HdrExtra0, Context),
	      Reply,
	      HdrExtra,
              Context):- !,
	(   memberchk(close(_), HdrExtra0)
	->  HdrExtra = HdrExtra0
	;   HdrExtra = [close(Close)|HdrExtra0],
	    (   keep_alive(Reply)
	    ->  Close = 'Keep-Alive'
	    ;   Close = close
	    )
	).
map_exception_to_http_status(error(existence_error(http_location, Location), _),
	      not_found(Location),
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(error(permission_error(_, http_location, Location), _),
	      forbidden(Location),
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(error(threads_in_pool(_Pool), _),
	      busy,
	      [connection(close)],
              []) :- !.
map_exception_to_http_status(E,
	      resource_error(E),
	      [connection(close)],
              []) :-
	resource_error(E), !.
map_exception_to_http_status(E,
	      bad_request(E2),
	      [connection(close)],
              []) :-
	bad_request_error(E), !,
	discard_stack_trace(E, E2).
map_exception_to_http_status(E,
	      server_error(E),
	      [connection(close)],
              []).

resource_error(error(resource_error(_), _)).

bad_request_error(error(domain_error(http_request, _), _)).
bad_request_error(error(existence_error(http_parameter, _), _)).
bad_request_error(error(type_error(_, _), context(_, http_parameter(Field)))) :-
	atom(Field).
bad_request_error(error(syntax_error(http_request_line(_)), _)).
bad_request_error(error(syntax_error(http_request(_)), _)).

discard_stack_trace(error(Formal, context(_,Msg)),
		    error(Formal, context(_,Msg))).


%%	keep_alive(+Reply) is semidet.
%
%	If true for Reply, the default is to keep the connection open.

keep_alive(not_modified).
keep_alive(file(_Type, _File)).
keep_alive(tmp_file(_Type, _File)).
keep_alive(stream(_In, _Len)).
keep_alive(cgi_stream(_In, _Len)).
keep_alive(switching_protocols(_Goal, _)).


		 /*******************************
		 *	    IDE SUPPORT		*
		 *******************************/

% See library('trace/exceptions')

:- multifile
	prolog:general_exception/2.

prolog:general_exception(http_reply(_), http_reply(_)).
prolog:general_exception(http_reply(_,_), http_reply(_,_)).
