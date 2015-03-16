/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008-2015, University of Amsterdam
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


:- module(http_hooks,
          []).

:- use_module(library(http/http_header)).

        	 /*******************************
		 *	      PROXY		*
		 *******************************/

socket:try_proxy(proxy(Host, Port), Address, Socket, StreamPair):-
        % Connection is via an HTTP proxy for socket: Use HTTP CONNECT
        % Note that most proxies will only support this for connecting on port 443
        !,
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port, StreamPair),
        catch(negotiate_http_connect(StreamPair, Address),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).

negotiate_http_connect(StreamPair, Address):-
        format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
        flush_output(StreamPair),
        http_read_reply_header(StreamPair, Header),
        memberchk(status(_, Status, Message), Header),
        ( Status == ok ->
            true
        ; throw(error(proxy_rejection(Message), _))
        ).

%%      network_proxy:find_proxy_for_url(+URL, +Hostname, -ListOfProxies) is det.
%       This hook can be implemented to return a list of proxies to try when connecting
%       to URL. Pre-defined proxy methods are:
%          * direct: connect directly to the resource
%          * proxy(Host, Port): Connect to the resource using an HTTP proxy. If the
%            resource is not an HTTP URL, then try to connect using the CONNECT verb,
%            otherwise, use the GET verb.
%          * socks(Host, Port): Connect to the resource via a SOCKS5 proxy
%       These correspond to the proxy methods defined by PAC. Additional methods can be
%       returned if suitable clauses for http:http_connection_over_proxy/6 or
%       socket:try_proxy/4 are defined.

:-multifile(network_proxy:find_proxy_for_url/3).


%%      socket:try_proxy(+Proxy, +TargetAddress, -Socket, -StreamPair).
%       Attempt a socket-level connection via the given proxy to TargetAddress.

:-multifile(socket:try_proxy/4).


%%      http:http_connection_over_proxy(+Proxy, +URLParts, +Endpoint, -StreamPair, +Options, -NewOptions).
%       Try to connect to the host Endpoint via Proxy for the purposes of retrieving the
%       resource identified by URLParts. Different options can be returned in NewOptions,
%       which may be required if you have defined a non-standard proxy method in
%       network_proxy:find_proxy_for_url/3 (such as one requiring authentication)

:-multifile(http:http_connection_over_proxy/6).