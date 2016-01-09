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


:- module(http_open,
	  [ http_open/3,		% +URL, -Stream, +Options
	    http_set_authorization/2,	% +URL, +Authorization
	    http_close_keep_alive/1	% +Address
	  ]).
:- use_module(library(uri)).
:- use_module(library(readutil)).
:- use_module(library(socket)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(base64)).
:- use_module(library(debug)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/http_header), [http_parse_header/2]).
:- use_module(library(http/http_stream)).

/** <module> HTTP client library

This library defines http_open/3, which opens a  URL as a Prolog stream.
The functionality of the  library  can   be  extended  by  loading three
additional modules that act as plugins:

    * library(http/http_ssl_plugin)
    Loading this library causes http_open/3 to handle HTTPS connections.
    Relevant options for SLL certificate handling are handed to
    ssl_context/3. This plugin is loaded automatically if the scheme
    `https` is requested using a default SSL context. See the plugin for
    additional information regarding security.

    * library(http/http_cookie)
    Loading this library adds tracking cookies to this library. Returned
    cookies are collected in the Prolog database and supplied for
    subsequent requests.

Here is a simple example to fetch a web-page:

  ==
  ?- http_open('http://www.google.com/search?q=prolog', In, []),
     copy_stream_data(In, user_output),
     close(In).
  <!doctype html><head><title>prolog - Google Search</title><script>
  ...
  ==

The example below fetches the modification time of a web-page. Note that
Modified is '' (the empty atom)  if   the  web-server does not provide a
time-stamp for the resource. See also parse_time/2.

  ==
  modified(URL, Stamp) :-
	  http_open(URL, In,
		    [ method(head),
		      header(last_modified, Modified)
		    ]),
	  close(In),
	  Modified \== '',
	  parse_time(Modified, Stamp).
  ==

Then next example uses Google search. It exploits library(uri) to manage
URIs, library(sgml) to load  an  HTML   document  and  library(xpath) to
navigate the parsed HTML. Note that  you   may  need to adjust the xpath
queries if the data returned by Google changes.

  ==
  :- use_module(library(http/http_open)).
  :- use_module(library(xpath)).
  :- use_module(library(sgml)).
  :- use_module(library(uri)).

  google(For, Title, HREF) :-
	  uri_encoded(query_value, For, Encoded),
	  atom_concat('http://www.google.com/search?q=', Encoded, URL),
	  http_open(URL, In, []),
	  call_cleanup(
	      load_html(In, DOM, []),
	      close(In)),
	  xpath(DOM, //h3(@class=r), Result),
	  xpath(Result, //a(@href=HREF0, text), Title),
	  uri_components(HREF0, Components),
	  uri_data(search, Components, Query),
	  uri_query_components(Query, Parts),
	  memberchk(q=HREF, Parts).
  ==

An example query is below:

==
?- google(prolog, Title, HREF).
Title = 'SWI-Prolog',
HREF = 'http://www.swi-prolog.org/' ;
Title = 'Prolog - Wikipedia',
HREF = 'https://nl.wikipedia.org/wiki/Prolog' ;
Title = 'Prolog - Wikipedia, the free encyclopedia',
HREF = 'https://en.wikipedia.org/wiki/Prolog' ;
Title = 'Pro-Log is logistiek dienstverlener m.b.t. vervoer over water.',
HREF = 'http://www.pro-log.nl/' ;
Title = 'Learn Prolog Now!',
HREF = 'http://www.learnprolognow.org/' ;
Title = 'Free Online Version - Learn Prolog
...
==

@see load_html/3 and xpath/3 can be used to parse and navigate HTML
     documents.
@see http_get/3 and http_post/4 provide an alternative interface that
     convert the reply depending on the =|Content-Type|= header.
*/

:- multifile
	http:encoding_filter/3,		  % +Encoding, +In0,  -In
	http:current_transfer_encoding/1, % ?Encoding
	http:http_protocol_hook/5,	  % +Protocol, +Parts, +StreamPair,
					  % -NewStreamPair, +Options
	http:open_options/2,		  % +Parts, -Options
	http:write_cookies/3,		  % +Out, +Parts, +Options
	http:update_cookies/3,		  % +CookieLine, +Parts, +Options
	http:authenticate_client/2,	  % +URL, +Action
	http:http_connection_over_proxy/6.

:- meta_predicate
	http_open(+,-,:).

:- predicate_options(http_open/3, 3,
		     [ authorization(compound),
		       final_url(-atom),
		       header(+atom, -atom),
		       headers(-list),
		       connection(+atom),
		       method(oneof([delete,get,put,head,post])),
		       size(-integer),
		       status_code(-integer),
		       output(-stream),
		       timeout(number),
		       proxy(atom, integer),
		       proxy_authorization(compound),
		       bypass_proxy(boolean),
		       request_header(any),
		       user_agent(atom),
		       version(-compound),
	% The option below applies if library(http/http_header) is loaded
		       post(any),
	% The options below apply if library(http/http_ssl_plugin)) is loaded
		       pem_password_hook(callable),
		       cacert_file(atom),
		       cert_verify_hook(callable)
		     ]).

%%	user_agent(-Agent) is det.
%
%	Default value for =|User-Agent|=,  can   be  overruled using the
%	option user_agent(Agent) of http_open/3.

user_agent('SWI-Prolog').

%%	http_open(+URL, -Stream, +Options) is det.
%
%	Open the data at the HTTP  server   as  a  Prolog stream. URL is
%	either an atom  specifying  a  URL   or  a  list  representing a
%	broken-down  URL  as  specified  below.   After  this  predicate
%	succeeds the data can be read from Stream. After completion this
%	stream must be  closed  using   the  built-in  Prolog  predicate
%	close/1. Options provides additional options:
%
%	  * authorization(+Term)
%	  Send authorization.  Currently only supports basic(User,Password).
%	  See also http_set_authorization/2.
%
%	  * connection(+Connection)
%	  Specify the =Connection= header.  Default is =close=.  The
%	  alternative is =|Keep-alive|=.  This maintains a pool of
%	  available connections as determined by keep_connection/1.
%	  The library(http/websockets) uses =|Keep-alive, Upgrade|=.
%	  Keep-alive connections can be closed explicitly using
%	  http_close_keep_alive/1. Keep-alive connections may
%	  significantly improve repetitive requests on the same server,
%	  especially if the IP route is long, HTTPS is used or the
%	  connection uses a proxy.
%
%	  * final_url(-FinalURL)
%	  Unify FinalURL with the final   destination. This differs from
%	  the  original  URL  if  the  returned  head  of  the  original
%	  indicates an HTTP redirect (codes 301,  302 or 303). Without a
%	  redirect, FinalURL is the same as URL if  URL is an atom, or a
%	  URL constructed from the parts.
%
%	  * header(Name, -AtomValue)
%	  If provided, AtomValue is  unified  with   the  value  of  the
%	  indicated  field  in  the  reply    header.  Name  is  matched
%	  case-insensitive and the underscore  (_)   matches  the hyphen
%	  (-). Multiple of these options  may   be  provided  to extract
%	  multiple  header  fields.  If  the  header  is  not  available
%	  AtomValue is unified to the empty atom ('').
%
%	  * headers(-List)
%	  If provided, List is unified with  a list of Name(Value) pairs
%	  corresponding to fields in the reply   header.  Name and Value
%	  follow the same conventions  used   by  the header(Name,Value)
%	  option.
%
%	  * method(+Method)
%	  One of =get= (default), =head=, =delete=, =post= or =put=.
%	  The  =head= message can be
%	  used in combination with  the   header(Name,  Value) option to
%	  access information on the resource   without actually fetching
%	  the resource itself.  The  returned   stream  must  be  closed
%	  immediately.
%
%	  If post(Data) is provided, the default is =post=.
%
%	  * size(-Size)
%	  Size is unified with the   integer value of =|Content-Length|=
%	  in the reply header.
%
%	  * version(-Version)
%	  Version is a _pair_ `Major-Minor`, where `Major` and `Minor`
%	  are integers representing the HTTP version in the reply header.
%
%	  * range(+Range)
%	  Ask for partial content. Range   is  a term _|Unit(From,To)|_,
%	  where `From` is an integer and `To`   is  either an integer or
%	  the atom `end`. HTTP 1.1 only   supports Unit = `bytes`. E.g.,
%	  to   ask   for    bytes    1000-1999,     use    the    option
%	  range(bytes(1000,1999))
%
%	  * redirect(+Boolean)
%	  If `false` (default `true`), do _not_ automatically redirect
%	  if a 3XX code is received.  Must be combined with
%	  status_code(Code) and one of the header options to read the
%	  redirect reply. In particular, without status_code(Code) a
%	  redirect is mapped to an exception.
%
%	  * status_code(-Code)
%	  If this option is  present  and   Code  unifies  with the HTTP
%	  status code, do *not* translate errors (4xx, 5xx) into an
%	  exception. Instead, http_open/3 behaves as if 200 (success) is
%	  returned, providing the application to read the error document
%	  from the returned stream.
%
%	  * output(-Out)
%	  Unify the output stream with Out and do not close it. This can
%	  be used to upgrade a connection.
%
%	  * timeout(+Timeout)
%	  If provided, set a timeout on   the stream using set_stream/2.
%	  With this option if no new data arrives within Timeout seconds
%	  the stream raises an exception.  Default   is  to wait forever
%	  (=infinite=).
%
%	  * post(+Data)
%	  Issue a =POST= request on the HTTP server.  Data is
%	  handed to http_post_data/3.
%
%	  * proxy(+Host:Port)
%	  Use an HTTP proxy to connect to the outside world.  See also
%	  socket:proxy_for_url/3.  This option overrules the proxy
%	  specification defined by socket:proxy_for_url/3.
%
%	  * proxy(+Host, +Port)
%	  Synonym for proxy(+Host:Port).  Deprecated.
%
%	  * proxy_authorization(+Authorization)
%	  Send authorization to the proxy.  Otherwise   the  same as the
%	  =authorization= option.
%
%	  * bypass_proxy(+Boolean)
%	  If =true=, bypass proxy hooks.  Default is =false=.
%
%	  * request_header(Name = Value)
%	  Additional  name-value  parts  are  added   in  the  order  of
%	  appearance to the HTTP request   header.  No interpretation is
%	  done.
%
%	  * max_redirect(+Max)
%	  Sets the maximum length of a redirection chain.  This is needed
%	  for some IRIs that redirect indefinitely to other IRIs without
%	  looping (e.g., redirecting to IRIs with a random element in them).
%	  Max must be either a non-negative integer or the atom `infinite`.
%	  The default value is `10`.
%
%	  * user_agent(+Agent)
%	  Defines the value of the  =|User-Agent|=   field  of  the HTTP
%	  header. Default is =SWI-Prolog=.
%
%	The hook http:open_options/2 can  be   used  to  provide default
%	options   based   on   the   broken-down     URL.   The   option
%	status_code(-Code)  is  particularly  useful   to  query  *REST*
%	interfaces that commonly return status   codes  other than `200`
%	that need to be be processed by the client code.
%
%	@param URL is either an atom or string (url) or a list of _parts_.
%		If this list is provided, it may contain the fields
%		=scheme=, =user=, =password=, =host=, =port=, =path= and
%		=search= (where the argument of the latter is a list of
%		Name(Value) or Name=Value).  Only =host= is mandatory.
%		The following example below opens the URL
%		=|http://www.example.com/my/path?q=Hello%20World&lang=en|=.
%		Note that values must *not* be quoted because the
%		library inserts the required quotes.
%
%		==
%		http_open([ host('www.example.com'),
%			    path('/my/path'),
%			    search([ q='Hello world',
%			             lang=en
%				   ])
%			  ])
%		==
%
%	@error existence_error(url, Id)
%	@see ssl_context/3 for SSL related options if
%	library(http/http_ssl_plugin) is loaded.

:- multifile
	socket:proxy_for_url/3.		  % +URL, +Host, -ProxyList

http_open(URL, Stream, QOptions) :-
	meta_options(is_meta, QOptions, Options),
	(   atomic(URL)
	->  parse_url_ex(URL, Parts)
        ;   Parts = URL
	),
	autoload_https(Parts),
	add_authorization(Parts, Options, Options1),
	findall(HostOptions,
		http:open_options(Parts, HostOptions),
		AllHostOptions),
	foldl(merge_options_rev, AllHostOptions, Options1, Options2),
        (   option(bypass_proxy(true), Options)
	->  try_http_proxy(direct, Parts, Stream, Options2)
        ;   term_variables(Options2, Vars2),
	    findall(Result-Vars2,
		    try_a_proxy(Parts, Result, Options2),
		    ResultList),
	    last(ResultList, Status-Vars2)
	->  (	Status = true(_Proxy, Stream)
	    ->	true
	    ;	throw(error(proxy_error(tried(ResultList)), _))
	    )
        ;   try_http_proxy(direct, Parts, Stream, Options2)
        ).

try_a_proxy(Parts, Result, Options) :-
	parts_uri(Parts, AtomicURL),
	option(host(Host), Parts),
	(   (   option(proxy(ProxyHost:ProxyPort), Options)
	    ;	is_list(Options),
		memberchk(proxy(ProxyHost,ProxyPort), Options)
	    )
	->  Proxy = proxy(ProxyHost, ProxyPort)
	;   socket:proxy_for_url(AtomicURL, Host, Proxy)
	),
	debug(http(proxy),
	      'http_open: Connecting via ~w to ~w', [Proxy, AtomicURL]),
	(   catch(try_http_proxy(Proxy, Parts, Stream, Options), E, true)
	->  (   var(E)
	    ->	!, Result = true(Proxy, Stream)
	    ;	Result = error(Proxy, E)
	    )
	;   Result = false(Proxy)
	),
        debug(http(proxy), 'http_open: ~w: ~p', [Proxy, Result]).

try_http_proxy(Method, Parts, Stream, Options0) :-
        option(host(Host), Parts),
	(   Method == direct
	->  parts_request_uri(Parts, RequestURI)
	;   parts_uri(Parts, RequestURI)
	),
	select_option(visited(Visited0), Options0, OptionsV, []),
	Options = [visited([Parts|Visited0])|OptionsV],
        parts_scheme(Parts, Scheme),
        default_port(Scheme, DefPort),
        url_part(port(Port), Parts, DefPort),
	host_and_port(Host, DefPort, Port, HostPort),
	(   option(connection(Connection), Options0),
	    keep_alive(Connection),
	    get_from_pool(Host:Port, StreamPair),
	    debug(http(connection), 'Trying Keep-alive to ~p using ~p',
		  [ Host:Port, StreamPair ]),
	    catch(send_rec_header(StreamPair, Stream, HostPort,
				  RequestURI, Parts, Options),
		  error(E,_),
		  keep_alive_error(E))
	->  true
	;   http:http_connection_over_proxy(Method, Parts, Host:Port,
					    SocketStreamPair, Options, Options1),
	    (   http:http_protocol_hook(Scheme, Parts,
					SocketStreamPair,
					StreamPair, Options)
	    ->  true
	    ;   StreamPair = SocketStreamPair
	    ),
	    send_rec_header(StreamPair, Stream, HostPort,
			    RequestURI, Parts, Options1)
	),
        return_final_url(Options).

http:http_connection_over_proxy(direct, _, Host:Port,
				StreamPair, Options, Options) :- !,
        open_socket(Host:Port, StreamPair, Options).
http:http_connection_over_proxy(proxy(ProxyHost, ProxyPort), Parts, _,
				StreamPair, Options, Options) :-
        \+ memberchk(scheme(https), Parts), !,
        % We do not want any /more/ proxy after this
        open_socket(ProxyHost:ProxyPort, StreamPair,
		    [bypass_proxy(true)|Options]).
http:http_connection_over_proxy(socks(SocksHost, SocksPort), _Parts, Host:Port,
				StreamPair, Options, Options) :- !,
         tcp_connect(SocksHost:SocksPort, StreamPair, [bypass_proxy(true)]),
         catch(negotiate_socks_connection(Host:Port, StreamPair),
               Error,
               ( close(StreamPair, [force(true)]),
                 throw(Error)
               )).


merge_options_rev(Old, New, Merged) :-
	merge_options(New, Old, Merged).

is_meta(pem_password_hook).		% SSL plugin callbacks
is_meta(cert_verify_hook).


http:http_protocol_hook(http, _, StreamPair, StreamPair, _).

default_port(https, 443) :- !.
default_port(wss,   443) :- !.
default_port(_,	    80).

host_and_port(Host, DefPort, DefPort, Host) :- !.
host_and_port(Host, _,       Port,    Host:Port).

%%	autoload_https(+Parts) is det.
%
%	If the requested scheme is https, load the HTTPS plugin.

autoload_https(Parts) :-
	memberchk(scheme(https), Parts),
	\+ clause(http:http_protocol_hook(https, _, StreamPair, StreamPair, _),_),
	exists_source(library(http/http_ssl_plugin)), !,
	use_module(library(http/http_ssl_plugin)).
autoload_https(_).


%%	send_rec_header(+StreamPair, -Stream,
%%			+Host, +RequestURI, +Parts, +Options) is det.
%
%	Send header to Out and process reply.  If there is an error or
%	failure, close In and Out and return the error or failure.

send_rec_header(StreamPair, Stream, Host, RequestURI, Parts, Options) :-
	(   catch(guarded_send_rec_header(StreamPair, Stream,
					  Host, RequestURI, Parts, Options),
		  E, true)
	->  (   var(E)
	    ->	(   option(output(StreamPair), Options)
		->  true
		;   true
		)
            ;   close(StreamPair, [force(true)]),
		throw(E)
	    )
	;   close(StreamPair, [force(true)]),
	    fail
	).

guarded_send_rec_header(StreamPair, Stream, Host, RequestURI, Parts, Options) :-
	user_agent(Agent, Options),
	method(Options, MNAME),
	http_version(Version),
	option(connection(Connection), Options, close),
	format(StreamPair,
	       '~w ~w HTTP/~w\r\n\c
	       Host: ~w\r\n\c
	       User-Agent: ~w\r\n\c
	       Connection: ~w\r\n',
	       [MNAME, RequestURI, Version, Host, Agent, Connection]),
	parts_uri(Parts, URI),
	x_headers(Options, URI, StreamPair),
	write_cookies(StreamPair, Parts, Options),
        (   option(post(PostData), Options)
        ->  http_header:http_post_data(PostData, StreamPair, [])
        ;   format(StreamPair, '\r\n', [])
        ),
	flush_output(StreamPair),
					% read the reply header
	read_header(StreamPair, ReplyVersion, Code, Comment, Lines),
	update_cookies(Lines, Parts, Options),
	do_open(ReplyVersion, Code, Comment, Lines, Options, Parts, Host,
		StreamPair, Stream).


%%	http_version(-Version:atom) is det.
%
%	HTTP version we publish. We  can  only   use  1.1  if we support
%	chunked encoding, which means http_chunked.pl must be loaded.

http_version('1.1') :-
	http:current_transfer_encoding(chunked), !.
http_version('1.0').

method(Options, MNAME) :-
	option(post(_), Options), !,
	option(method(M), Options, post),
	(   map_method(M, MNAME0)
	->  MNAME = MNAME0
	;   domain_error(method, M)
	).
method(Options, MNAME) :-
	option(method(M), Options, get),
	(   map_method(M, MNAME0)
	->  MNAME = MNAME0
	;   map_method(_, M)
	->  MNAME = M
	;   domain_error(method, M)
	).

map_method(delete, 'DELETE').
map_method(get,	   'GET').
map_method(head,   'HEAD').
map_method(post,   'POST').
map_method(put,	   'PUT').

%%	x_headers(+Options, +Out) is det.
%
%	Emit extra headers from   request_header(Name=Value)  options in
%	Options.
%
%	@tbd Use user/password fields

x_headers([], _, _).
x_headers([H|T], URI, Out) :- !,
	x_header(H, URI, Out),
	x_headers(T, URI, Out).

x_header(request_header(Name=Value), _, Out) :- !,
	format(Out, '~w: ~w\r\n', [Name, Value]).
x_header(proxy_authorization(ProxyAuthorization), URI, Out) :- !,
	auth_header(ProxyAuthorization, URI, 'Proxy-Authorization', Out).
x_header(authorization(Authorization), URI, Out) :- !,
	auth_header(Authorization, URI, 'Authorization', Out).
x_header(range(Spec), _, Out) :- !,
        Spec =.. [Unit, From, To],
        (   To == end
        ->  ToT = ''
        ;   must_be(integer, To),
            ToT = To
        ),
        format(Out, 'Range: ~w=~d-~w\r\n', [Unit, From, ToT]).
x_header(_, _, _).

auth_header(basic(User, Password), _, Header, Out) :- !,
	format(codes(Codes), '~w:~w', [User, Password]),
	phrase(base64(Codes), Base64Codes),
	format(Out, '~w: Basic ~s\r\n', [Header, Base64Codes]).
auth_header(Auth, URL, _, Out) :-
	http:authenticate_client(URL, send_auth_header(Auth, Out)), !.
auth_header(Auth, _, _, _) :-
	domain_error(authorization, Auth).

user_agent(Agent, Options) :-
	(   option(user_agent(Agent), Options)
	->  true
	;   user_agent(Agent)
	).

%%	do_open(+HTTPVersion, +HTTPStatusCode, +HTTPStatusComment, +Header,
%%		+Options, +Parts, +Host, +In, -FinalIn) is det.
%
%	Handle the HTTP status. If 200, we   are ok. If a redirect, redo
%	the open, returning a new stream. Else issue an error.
%
%	@error	existence_error(url, URL)

					% Redirections
do_open(_, Code, _, Lines, Options0, Parts, _, In, Stream) :-
	redirect_code(Code),
	option(redirect(true), Options0, true),
	location(Lines, RequestURI), !,
	debug(http(redirect), 'http_open: redirecting to ~w', [RequestURI]),
	close(In),
	parts_uri(Parts, Base),
	uri_resolve(RequestURI, Base, Redirected),
	parse_url_ex(Redirected, RedirectedParts),
	(   redirect_limit_exceeded(Options0, Max)
	->  format(atom(Comment), 'max_redirect (~w) limit exceeded', [Max]),
	    throw(error(permission_error(redirect, http, Redirected),
			context(_, Comment)))
	;   redirect_loop(RedirectedParts, Options0)
	->  throw(error(permission_error(redirect, http, Redirected),
			context(_, 'Redirection loop')))
	;   true
	),
	redirect_options(Options0, Options),
	http_open(RedirectedParts, Stream, Options).
					% Need authentication
do_open(_Version, Code, _Comment, Lines, Options0, Parts, _Host, In0, Stream) :-
	authenticate_code(Code),
	parts_uri(Parts, URI),
	parse_headers(Lines, Headers),
	http:authenticate_client(
		 URI,
		 auth_reponse(Headers, Options0, Options)), !,
	close(In0),
	http_open(Parts, Stream, Options).
					% Accepted codes
do_open(Version, Code, _, Lines, Options, Parts, Host, In0, In) :-
	(   option(status_code(Code), Options),
	    Lines \== []
	->  true
	;   Code == 200
	), !,
	parts_uri(Parts, URI),
	parse_headers(Lines, Headers),
	return_version(Options, Version),
	return_size(Options, Headers),
	return_fields(Options, Headers),
	return_headers(Options, Headers),
	consider_keep_alive(Lines, Parts, Host, In0, In1, Options),
	transfer_encoding_filter(Lines, In1, In),
					% properly re-initialise the stream
	set_stream(In, file_name(URI)),
	set_stream(In, record_position(true)).
do_open(_, _, _, [], Options, _, _, _, _) :-
	option(connection(Connection), Options),
	keep_alive(Connection), !,
	throw(error(keep_alive(closed),_)).
					% report anything else as error
do_open(_Version, Code, Comment, _,  _, Parts, _, _, _) :-
	parts_uri(Parts, URI),
	(   map_error_code(Code, Error)
	->  Formal =.. [Error, url, URI]
	;   Formal = existence_error(url, URI)
	),
	throw(error(Formal, context(_, status(Code, Comment)))).


%%	redirect_limit_exceeded(+Options:list(compound), -Max:nonneg) is
%	semidet.
%
%	True if we have exceeded the maximum redirection length (default 10).

redirect_limit_exceeded(Options, Max) :-
	option(visited(Visited), Options, []),
	length(Visited, N),
	option(max_redirect(Max), Options, 10),
	(Max == infinite -> fail ; N > Max).


%%	redirect_loop(+Parts, +Options) is semidet.
%
%	True if we are in  a  redirection   loop.  Note  that some sites
%	redirect once to the same place using  cookies or similar, so we
%	allow for two tries. In fact,   we  should probably test whether
%	authorization or cookie headers have changed.

redirect_loop(Parts, Options) :-
	option(visited(Visited), Options, []),
	include(==(Parts), Visited, Same),
	length(Same, Count),
	Count > 2.


%%	redirect_options(+Options0, -Options) is det.
%
%	A redirect from a POST should do a GET on the returned URI. This
%	means we must remove  the   method(post)  and post(Data) options
%	from the original option-list.

redirect_options(Options0, Options) :-
	(   select_option(post(_), Options0, Options1)
	->  true
	;   Options1 = Options0
	),
	(   select_option(method(Method), Options1, Options),
	    \+ redirect_method(Method)
	->  true
	;   Options = Options1
	).

redirect_method(delete).
redirect_method(get).
redirect_method(head).


%%	map_error_code(+HTTPCode, -PrologError) is semidet.
%
%	Map HTTP error codes to Prolog errors.
%
%	@tbd	Many more maps. Unfortunately many have no sensible Prolog
%		counterpart.

map_error_code(401, permission_error).
map_error_code(403, permission_error).
map_error_code(404, existence_error).
map_error_code(405, permission_error).
map_error_code(407, permission_error).
map_error_code(410, existence_error).

redirect_code(301).			% moved permanently
redirect_code(302).			% moved temporary
redirect_code(303).			% see also

authenticate_code(401).

%%	open_socket(+Address, -StreamPair, +Options) is det.
%
%	Create and connect a client socket to Address.  Options
%
%	    * timeout(+Timeout)
%	    Sets timeout on the stream, *after* connecting the
%	    socket.
%
%	@tbd	Make timeout also work on tcp_connect/4.
%	@tbd	This is the same as do_connect/4 in http_client.pl

open_socket(Address, StreamPair, Options) :-
	debug(http(open), 'http_open: Connecting to ~p ...', [Address]),
	tcp_connect(Address, StreamPair, Options),
        stream_pair(StreamPair, In, Out),
	debug(http(open), '\tok ~p ---> ~p', [In, Out]),
	set_stream(In, record_position(false)),
	(   option(timeout(Timeout), Options)
	->  set_stream(In, timeout(Timeout))
	;   true
	).


return_version(Options, Major-Minor) :-
	option(version(Major-Minor), Options, _).

return_size(Options, Headers) :-
	(   memberchk(content_length(Size), Headers)
	->  option(size(Size), Options, _)
	;   true
	).

return_fields([], _).
return_fields([header(Name, Value)|T], Headers) :- !,
	(   Term =.. [Name,Value],
	    memberchk(Term, Headers)
	->  true
	;   Value = ''
	),
	return_fields(T, Headers).
return_fields([_|T], Lines) :-
	return_fields(T, Lines).

return_headers(Options, Headers) :-
	option(headers(Headers), Options, _).

%%	parse_headers(+Lines, -Headers:list(compound)) is det.
%
%	Parse the header lines for   the  headers(-List) option. Invalid
%	header   lines   are   skipped,   printing   a   warning   using
%	pring_message/2.

parse_headers([], []) :- !.
parse_headers([Line|Lines], Headers) :-
	catch(http_parse_header(Line, [Header]), Error, true),
	(   var(Error)
	->  Headers = [Header|More]
	;   print_message(warning, Error),
	    Headers = More
	),
	parse_headers(Lines, More).


%%	return_final_url(+Options) is semidet.
%
%	If Options contains final_url(URL), unify URL with the final
%	URL after redirections.

return_final_url(Options) :-
	option(final_url(URL), Options),
	var(URL), !,
	option(visited(Parts), Options),
	parts_uri(Parts, URL).
return_final_url(_).


%%	transfer_encoding_filter(+Lines, +In0, -In) is det.
%
%	Install  filters  depending  on  the  encoding.   If  In0  is  a
%	stream-pair, we close the output side.

transfer_encoding_filter(Lines, In0, In) :-
	transfer_encoding(Lines, Encoding), !,
	stream_pair(In0, In1, Out),
	(   nonvar(Out)
	->  close(Out)
	;   true
	),
	(   http:encoding_filter(Encoding, In1, In)
	->  true
	;   domain_error(http_encoding, Encoding)
	).
transfer_encoding_filter(_, In, In).


%%	transfer_encoding(+Lines, -Encoding) is semidet.
%
%	True if Encoding is the value of the =|Transfer-encoding|=
%	header.

transfer_encoding(Lines, Encoding) :-
	member(Line, Lines),
	phrase(transfer_encoding(Encoding0), Line), !,
	debug(http(transfer_encoding), 'Transfer-encoding: ~w', [Encoding0]),
	Encoding = Encoding0.

transfer_encoding(Encoding) -->
	field('transfer-encoding'),
	rest(Encoding).

%%	read_header(+In:stream, -Version, -Code:int, -Comment:atom, -Lines:list) is det.
%
%	Read the HTTP reply-header. If the replied header is invalid, it
%	simulates a 500 error with the comment =|Invalid reply header|=.
%
%	@param Version	HTTP reply version as Major-Minor pair
%	@param Code	Numeric HTTP reply-code
%	@param Comment	Comment of reply-code as atom
%	@param Lines	Remaining header lines as code-lists.

read_header(In, Major-Minor, Code, Comment, Lines) :-
	read_line_to_codes(In, Line),
	Line \== end_of_file,
	phrase(first_line(Major-Minor, Code, Comment), Line),
	debug(http(open), 'HTTP/~d.~d ~w ~w', [Major, Minor, Code, Comment]),
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, Lines), !,
	(   debugging(http(open))
	->  forall(member(HL, Lines),
		   debug(http(open), '~s', [HL]))
	;   true
	).
read_header(_, 1-1, 500, 'Invalid reply header', []).

rest_header([], _, []) :- !.		% blank line: end of header
rest_header(L0, In, [L0|L]) :-
	read_line_to_codes(In, L1),
	rest_header(L1, In, L).

%%	content_length(+Header, -Length:int) is semidet.
%
%	Find the Content-Length in an HTTP reply-header.

content_length(Lines, Length) :-
	member(Line, Lines),
	phrase(content_length(Length0), Line), !,
	Length = Length0.

location(Lines, RequestURI) :-
	member(Line, Lines),
	phrase(atom_field(location, RequestURI), Line), !.

connection(Lines, Connection) :-
	member(Line, Lines),
	phrase(atom_field(connection, Connection0), Line), !,
	Connection = Connection0.

first_line(Major-Minor, Code, Comment) -->
	"HTTP/", integer(Major), ".", integer(Minor),
	skip_blanks,
	integer(Code),
	skip_blanks,
	rest(Comment).

atom_field(Name, Value) -->
	field(Name),
	rest(Value).

content_length(Len) -->
	field('content-length'),
	integer(Len).

field(Name) -->
	{ atom_codes(Name, Codes) },
	field_codes(Codes).

field_codes([]) -->
	":",
	skip_blanks.
field_codes([H|T]) -->
	[C],
	{ match_header_char(H, C)
	},
	field_codes(T).

match_header_char(C, C) :- !.
match_header_char(C, U) :-
	code_type(C, to_lower(U)), !.
match_header_char(0'_, 0'-).


skip_blanks -->
	[C],
	{ code_type(C, white)
	}, !,
	skip_blanks.
skip_blanks -->
	[].

%%	integer(-Int)//
%
%	Read 1 or more digits and return as integer.

integer(Code) -->
	digit(D0),
	digits(D),
	{ number_codes(Code, [D0|D])
	}.

digit(C) -->
	[C],
	{ code_type(C, digit)
	}.

digits([D0|D]) -->
	digit(D0), !,
	digits(D).
digits([]) -->
	[].

%%	rest(-Atom:atom)//
%
%	Get rest of input as an atom.

rest(A,L,[]) :-
	atom_codes(A, L).


		 /*******************************
		 *   AUTHORIZATION MANAGEMENT	*
		 *******************************/

%%	http_set_authorization(+URL, +Authorization) is det.
%
%	Set user/password to supply with URLs   that have URL as prefix.
%	If  Authorization  is  the   atom    =|-|=,   possibly   defined
%	authorization is cleared.  For example:
%
%	==
%	?- http_set_authorization('http://www.example.com/private/',
%				  basic('John', 'Secret'))
%	==
%
%	@tbd	Move to a separate module, so http_get/3, etc. can use this
%		too.

:- dynamic
	stored_authorization/2,
	cached_authorization/2.

http_set_authorization(URL, Authorization) :-
	must_be(atom, URL),
	retractall(stored_authorization(URL, _)),
	(   Authorization = (-)
	->  true
	;   check_authorization(Authorization),
	    assert(stored_authorization(URL, Authorization))
	),
	retractall(cached_authorization(_,_)).

check_authorization(Var) :-
	var(Var), !,
	instantiation_error(Var).
check_authorization(basic(User, Password)) :-
	must_be(atom, User),
	must_be(atom, Password).

%%	authorization(+URL, -Authorization) is semidet.
%
%	True if Authorization must be supplied for URL.
%
%	@tbd	Cleanup cache if it gets too big.

authorization(_, _) :-
	\+ stored_authorization(_, _), !,
	fail.
authorization(URL, Authorization) :-
	cached_authorization(URL, Authorization), !,
	Authorization \== (-).
authorization(URL, Authorization) :-
	(   stored_authorization(Prefix, Authorization),
	    sub_atom(URL, 0, _, _, Prefix)
	->  assert(cached_authorization(URL, Authorization))
	;   assert(cached_authorization(URL, -)),
	    fail
	).

add_authorization(_, Options, Options) :-
	option(authorization(_), Options), !.
add_authorization(Parts, Options0, Options) :-
	url_part(user(User), Parts),
	url_part(password(Passwd), Parts),
	Options = [authorization(basic(User,Passwd))|Options0].
add_authorization(Parts, Options0, Options) :-
	stored_authorization(_, _) ->	% quick test to avoid work
	parts_uri(Parts, URL),
	authorization(URL, Auth), !,
	Options = [authorization(Auth)|Options0].
add_authorization(_, Options, Options).


%%	parse_url_ex(+URL, -Parts)
%
%	Parts:  Schema,  Host,  Port,    User:Password,  RequestURI  (no
%	fragment).

parse_url_ex(URL, [uri(URL)|Parts]) :-
	uri_components(URL, Components),
	phrase(components(Components), Parts),
	(   option(host(_), Parts)
	->  true
	;   domain_error(url, URL)
	).

components(Components) -->
	uri_scheme(Components),
	uri_authority(Components),
	uri_request_uri(Components).

uri_scheme(Components) -->
	{ uri_data(scheme, Components, Scheme), nonvar(Scheme) }, !,
	[ scheme(Scheme)
	].
uri_scheme(_) --> [].

uri_authority(Components) -->
	{ uri_data(authority, Components, Auth), nonvar(Auth), !,
	  uri_authority_components(Auth, Data)
	},
	[ authority(Auth) ],
	auth_field(user, Data),
	auth_field(password, Data),
	auth_field(host, Data),
	auth_field(port, Data).
uri_authority(_) --> [].

auth_field(Field, Data) -->
	{ uri_authority_data(Field, Data, EncValue), nonvar(EncValue), !,
	  (   atom(EncValue)
	  ->  uri_encoded(query_value, Value, EncValue)
	  ;   Value = EncValue
	  ),
	  Part =.. [Field,Value]
	},
	[ Part ].
auth_field(_, _) --> [].

uri_request_uri(Components) -->
	{ uri_data(path, Components, Path0),
	  uri_data(search, Components, Search),
	  (   Path0 == ''
	  ->  Path = (/)
	  ;   Path = Path0
	  ),
	  uri_data(path, Components2, Path),
	  uri_data(search, Components2, Search),
	  uri_components(RequestURI, Components2)
	},
	[ request_uri(RequestURI)
	].

%%	parts_scheme(+Parts, -Scheme) is det.
%%	parts_uri(+Parts, -URI) is det.
%%	parts_request_uri(+Parts, -RequestURI) is det.
%%	parts_search(+Parts, -Search) is det.
%%	parts_authority(+Parts, -Authority) is semidet.

parts_scheme(Parts, Scheme) :-
	url_part(scheme(Scheme), Parts), !.
parts_scheme(Parts, Scheme) :-		% compatibility with library(url)
	url_part(protocol(Scheme), Parts), !.
parts_scheme(_, http).

parts_authority(Parts, Auth) :-
	url_part(authority(Auth), Parts), !.
parts_authority(Parts, Auth) :-
	url_part(host(Host), Parts, _),
	url_part(port(Port), Parts, _),
	url_part(user(User), Parts, _),
	url_part(password(Password), Parts, _),
	uri_authority_components(Auth,
				 uri_authority(User, Password, Host, Port)).

parts_request_uri(Parts, RequestURI) :-
	option(request_uri(RequestURI), Parts), !.
parts_request_uri(Parts, RequestURI) :-
	url_part(path(Path), Parts, /),
	ignore(parts_search(Parts, Search)),
	uri_data(path, Data, Path),
	uri_data(search, Data, Search),
	uri_components(RequestURI, Data).

parts_search(Parts, Search) :-
	option(query_string(Search), Parts), !.
parts_search(Parts, Search) :-
	option(search(Fields), Parts), !,
	uri_query_components(Search, Fields).


parts_uri(Parts, URI) :-
	option(uri(URI), Parts), !.
parts_uri(Parts, URI) :-
	parts_scheme(Parts, Scheme),
	ignore(parts_authority(Parts, Auth)),
	parts_request_uri(Parts, RequestURI),
	uri_components(RequestURI, Data),
	uri_data(scheme, Data, Scheme),
	uri_data(authority, Data, Auth),
	uri_components(URI, Data).

parts_port(Parts, Port) :-
	parts_scheme(Parts, Scheme),
        default_port(Scheme, DefPort),
        url_part(port(Port), Parts, DefPort).

url_part(Part, Parts) :-
	Part =.. [Name,Value],
	Gen =.. [Name,RawValue],
	option(Gen, Parts), !,
	Value = RawValue.

url_part(Part, Parts, Default) :-
	Part =.. [Name,Value],
	Gen =.. [Name,RawValue],
	(   option(Gen, Parts)
	->  Value = RawValue
	;   Value = Default
	).


		 /*******************************
		 *	      COOKIES		*
		 *******************************/

write_cookies(Out, Parts, Options) :-
	http:write_cookies(Out, Parts, Options), !.
write_cookies(_, _, _).

update_cookies(_, _, _) :-
	predicate_property(http:update_cookies(_,_,_), number_of_clauses(0)), !.
update_cookies(Lines, Parts, Options) :-
	(   member(Line, Lines),
	    phrase(atom_field('set_cookie', CookieData), Line),
	    http:update_cookies(CookieData, Parts, Options),
	    fail
	;   true
	).


		 /*******************************
		 *	     OPEN ANY		*
		 *******************************/

:- multifile iostream:open_hook/6.

%%	iostream:open_hook(+Spec, +Mode, -Stream, -Close,
%%			   +Options0, -Options) is semidet.
%
%	Hook implementation that makes  open_any/5   support  =http= and
%	=https= URLs for `Mode == read`.

iostream:open_hook(URL, read, Stream, Close, Options0, Options) :-
	(atom(URL) -> true ; string(URL)),
	uri_is_global(URL),
	uri_components(URL, Components),
        uri_data(scheme, Components, Scheme),
	http_scheme(Scheme), !,
	Options = Options0,
	Close = close(Stream),
	http_open(URL, Stream, Options0).

http_scheme(http).
http_scheme(https).


		 /*******************************
		 *	    KEEP-ALIVE		*
		 *******************************/

%%	consider_keep_alive(+HeaderLines, +Parts, +Host,
%%			    +Stream0, -Stream,
%%			    +Options) is det.

consider_keep_alive(Lines, Parts, Host, StreamPair, In, Options) :-
	option(connection(Asked), Options),
	keep_alive(Asked),
	connection(Lines, Given),
	keep_alive(Given),
	content_length(Lines, Bytes), !,
	stream_pair(StreamPair, In0, _),
	connection_address(Host, Parts, HostPort),
	debug(http(connection),
	      'Keep-alive to ~w (~D bytes)', [HostPort, Bytes]),
	stream_range_open(In0, In,
			  [ size(Bytes),
			    onclose(keep_alive(StreamPair, HostPort))
			  ]).
consider_keep_alive(_, _, _, Stream, Stream, _).

connection_address(Host, _, Host) :-
	Host = _:_, !.
connection_address(Host, Parts, Host:Port) :-
	parts_port(Parts, Port).

keep_alive(keep_alive) :- !.
keep_alive(Connection) :-
	downcase_atom(Connection, 'keep-alive').

:- public keep_alive/4.

keep_alive(StreamPair, Host, In, Left) :-
	read_incomplete(In, Left),
	add_to_pool(Host, StreamPair), !.
keep_alive(StreamPair, _, _, _) :-
	close(StreamPair, [force(true)]).

%%	read_incomplete(+In, +Left) is semidet.
%
%	If we have not all input from  a Keep-alive connection, read the
%	remainder if it is short. Else, we fail and close the stream.

read_incomplete(_, 0) :- !.
read_incomplete(In, Left) :-
	Left < 100, !,
	catch(setup_call_cleanup(
		  open_null_stream(Null),
		  copy_stream_data(In, Null, Left),
		  close(Null)),
	      _,
	      fail).

:- dynamic
	connection_pool/4,		% Hash, Address, Stream, Time
	connection_gc_time/1.

add_to_pool(Address, StreamPair) :-
	keep_connection(Address),
	get_time(Now),
	term_hash(Address, Hash),
	assertz(connection_pool(Hash, Address, StreamPair, Now)).

get_from_pool(Address, StreamPair) :-
	term_hash(Address, Hash),
	retract(connection_pool(Hash, Address, StreamPair, _)).

%%	keep_connection(+Address) is semidet.
%
%	Succeeds if we want to keep   the  connection open. We currently
%	keep a maximum of 10 connections  waiting   and  a  maximum of 2
%	waiting for the same address. Connections   older than 2 seconds
%	are closed.

keep_connection(Address) :-
	close_old_connections(2),
	predicate_property(connection_pool(_,_,_,_), number_of_clauses(C)),
	C =< 10,
	term_hash(Address, Hash),
	aggregate_all(count, connection_pool(Hash, Address, _, _), Count),
	Count =< 2.

close_old_connections(Timeout) :-
	get_time(Now),
	Before is Now - Timeout,
	(   connection_gc_time(GC),
	    GC > Before
	->  true
	;   (   retractall(connection_gc_time(_)),
	        asserta(connection_gc_time(Now)),
		connection_pool(Hash, Address, StreamPair, Added),
	        Added < Before,
		retract(connection_pool(Hash, Address, StreamPair, Added)),
		debug(http(connection),
		      'Closing inactive keep-alive to ~p', [Address]),
		close(StreamPair, [force(true)]),
		fail
	    ;   true
	    )
	).


%%	http_close_keep_alive(+Address) is det.
%
%	Close all keep-alive connections matching Address. Address is of
%	the  form  Host:Port.  In  particular,  http_close_keep_alive(_)
%	closes all currently known keep-alive connections.

http_close_keep_alive(Address) :-
	forall(get_from_pool(Address, StreamPair),
	       close(StreamPair, [force(true)])).

%%	keep_alive_error(+Error)
%
%	Deal with an error from reusing  a keep-alive connection. If the
%	error is due to an I/O error   or end-of-file, fail to backtrack
%	over get_from_pool/2. Otherwise it is a   real error and we thus
%	re-raise it.

keep_alive_error(keep_alive(closed)) :- !,
	debug(http(connection), 'Keep-alive connection was closed', []),
	fail.
keep_alive_error(io_error(_,_)) :- !,
	debug(http(connection), 'IO error on Keep-alive connection', []),
	fail.
keep_alive_error(Error) :-
	throw(Error).


		 /*******************************
		 *     HOOK DOCUMENTATION	*
		 *******************************/

%%	http:open_options(+Parts, -Options) is nondet.
%
%	This hook is used by the HTTP   client library to define default
%	options based on the the broken-down request-URL.  The following
%	example redirects all trafic, except for localhost over a proxy:
%
%	    ==
%	    :- multifile
%		http:open_options/2.
%
%	    http:open_options(Parts, Options) :-
%		option(host(Host), Parts),
%		Host \== localhost,
%		Options = [proxy('proxy.local', 3128)].
%	    ==
%
%	This hook may return multiple   solutions.  The returned options
%	are  combined  using  merge_options/3  where  earlier  solutions
%	overrule later solutions.

%%	http:write_cookies(+Out, +Parts, +Options) is semidet.
%
%	Emit a =|Cookie:|= header for the  current connection. Out is an
%	open stream to the HTTP server, Parts is the broken-down request
%	(see uri_components/2) and Options is the list of options passed
%	to http_open.  The predicate is called as if using ignore/1.
%
%	@see complements http:update_cookies/3.
%	@see library(http/http_cookies) implements cookie handling on
%	top of these hooks.

%%	http:update_cookies(+CookieData, +Parts, +Options) is semidet.
%
%	Update the cookie database.  CookieData  is   the  value  of the
%	=|Set-Cookie|= field, Parts is  the   broken-down  request  (see
%	uri_components/2) and Options is the list   of options passed to
%	http_open.
%
%	@see complements http:write_cookies
%	@see library(http/http_cookies) implements cookie handling on
%	top of these hooks.
