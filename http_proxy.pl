/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Matt Lilley
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, University of Amsterdam
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


:- module(http_proxy, []).
:- use_module(library(http/http_header)).
:- use_module(library(socket)).

/** <module> Use HTTP network proxies

This  module  provides  a  plugin  for   tcp_connect/3  to  realise  TCP
connections through an HTTP proxy that   supports the HTTP 1.1 =CONNECT=
method.
*/

		 /*******************************
		 *	      PROXY		*
		 *******************************/

%%	socket:try_proxy(+Proxy, +Address, -Socket, -StreamPair)
%
%	Connection is via an HTTP proxy   for  socket: Use HTTP CONNECT.
%	Note that most proxies will only  support this for connecting on
%	port 443
%
%	@arg Proxy is of the form proxy(Host, Port)
%	@error proxy_error(Message) if the proxy connection is
%	not successful.

socket:try_proxy(proxy(Host, Port), Address, Socket, StreamPair) :- !,
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port, StreamPair),
        catch(negotiate_http_connect(StreamPair, Address),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).

negotiate_http_connect(StreamPair, Address) :-
        format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
        flush_output(StreamPair),
        http_read_reply_header(StreamPair, Header),
        memberchk(status(_, Status, Message), Header),
        (   Status == ok
	->  true
        ;   throw(error(proxy_error(Message), _))
        ).

