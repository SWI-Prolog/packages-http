/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam,
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


:- module(http_client,
	  [ http_get/3,			% +URL, -Reply, +Options
	    http_delete/3,		% +URL, -Reply, +Options
	    http_post/4,		% +URL, +In, -Reply, +Options
	    http_put/4,			% +URL, +In, -Reply, +Options
	    http_read_data/3,		% +Header, -Data, :Options
	    http_disconnect/1		% +What
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(http_header).
:- use_module(http_stream).
:- use_module(library(memfile)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(option)).

:- meta_predicate
	http_read_data(+, -, :).

:- multifile
	http_convert_data/4,		% http_read_data plugin-hook
	http:post_data_hook/3.

:- predicate_options(http_get/3, 3,
		     [ pass_to(http_open/3, 3),
		       pass_to(http_read_data/3, 3)
		     ]).
:- predicate_options(http_delete/3, 3, [pass_to(http_get/3, 3)]).
:- predicate_options(http_post/4, 4, [pass_to(http_get/3, 3)]).
:- predicate_options(http_put/4, 4, [pass_to(http_post/4, 4)]).
:- predicate_options(http_read_data/3, 3,
		     [ to(any),
		       content_type(any),
		       form_data(oneof([form,mime])),
		       input_encoding(encoding),
		       on_filename(callable)
		     ]).


/** <module> HTTP client library

This library provides  the  four  basic   HTTP  client  actions:  =GET=,
=DELETE=, =POST= and =PUT=. In   addition, it provides http_read_data/3,
which is used by library(http/http_parameters) to  decode =POST= data in
server applications.

This library is based on http_open/3,  which   opens  a  URL as a Prolog
stream.
*/

		 /*******************************
		 *	        GET		*
		 *******************************/

%%	http_get(+URL, -Data, +Options) is det.
%
%	Get data from a URL server and   convert it to a suitable Prolog
%	representation based on the =|Content-type|= header and plugins.
%	This predicate is the common implementation   of the HTTP client
%	operations.  The  predicates  http_delete/3,    http_post/4  and
%	http_put/4   call   this   predicate     with   an   appropriate
%	method(+Method) option and ---for  http_post/4 and http_put/4---
%	a post(+Data) option.
%
%	Options are passed to http_open/3   and  http_read_data/3. Other
%	options:
%
%	  - reply_header(-Fields)
%	  Synonym for headers(Fields) from http_open/3.  Provided for
%	  backward compatibility.  Note that http_version(Major-Minor)
%	  is missing in the new version.

http_get(URL, Data, Options) :-
	headers_option(Options, Options1, Headers),
	option(reply_header(Headers), Options, _),
	http_open(URL, In, Options1),
	delete(Headers, transfer_encoding(_), Headers1),
	call_cleanup(
	    http_read_data(In, Headers1, Data, Options),
	    close(In)).

headers_option(Options, Options1, Headers) :-
	option(headers(Headers), Options), !,
	Options1 = Options.
headers_option(Options, [headers(Headers)|Options], Headers).


%%	http_delete(+URL, -Data, +Options) is det.
%
%	Execute a DELETE method on the server.
%
%	@tbd Properly map the 201, 202 and 204 replies.

http_delete(URL, Data, Options) :-
	http_get(URL, Data, [method(delete)|Options]).


%%	http_read_data(+Request, -Data, +Options) is det.
%
%	Read data from an HTTP connection.   Options must contain a term
%	input(In) that provides the input stream   from the HTTP server.
%	Fields is the parsed http reply-header. Options is one of:
%
%	  * to(Format)
%	    Convert data into Format.  Values are:
%	    - stream(+WriteStream))
%	      Append the content of the message to Stream
%	    - atom
%	      Return the reply as an atom
%	    - string
%	      Return the reply as a string
%	    - codes
%	      Return the reply as a list of codes
%	  * form_data(AsForm)
%	  * input_encoding(+Encoding)
%	  * on_filename(:CallBack)
%	    These options are implemented by the plugin
%	    library(http/http_multipart_plugin) and apply to processing
%	    =|multipart/form-data|= content.
%	  * content_type(+Type)
%	    Overrule the content-type that is part of Request as a
%	    work-around for wrongly configured servers.
%
%	Without plugins, this predicate handles
%
%	  * 'application/x-www-form-urlencoded'
%	  Converts form-data into a list of `Name=Value` terms.
%	  * 'application/x-prolog'
%	  Converts data into a Prolog term.
%
%	@param Request is a parsed HTTP request as returned by
%	http_read_request/2 or available from the HTTP server's
%	request dispatcher.

http_read_data(Fields, Data, QOptions) :-
	meta_options(is_meta, QOptions, Options),
	memberchk(input(In), Fields),
	(   http_read_data(In, Fields, Data, Options)
	->  true
	;   throw(error(failed(http_read_data), _))
	).

is_meta(on_filename).

http_read_data(In, Fields, Data, Options) :-	% Transfer-encoding: chunked
	select(transfer_encoding(chunked), Fields, RestFields), !,
	setup_call_cleanup(
	    http_chunked_open(In, DataStream, []),
	    http_read_data(DataStream, RestFields, Data, Options),
	    close(DataStream)).
http_read_data(In, Fields, Data, Options) :-
	option(to(X), Options), !,
	(   X = stream(Stream)
	->  (   memberchk(content_length(Bytes), Fields)
	    ->  copy_stream_data(In, Stream, Bytes)
	    ;   copy_stream_data(In, Stream)
	    )
	;   must_be(oneof([atom,string,codes]), X),
	    setup_call_cleanup(
		new_memory_file(MemFile),
		( setup_call_cleanup(
		      open_memory_file(MemFile, write, Stream,
				       [encoding(octet)]),
		      (   memberchk(content_length(Bytes), Fields)
		      ->  copy_stream_data(In, Stream, Bytes)
		      ;   copy_stream_data(In, Stream)
		      ),
		      close(Stream)),
		  encoding(Fields, Encoding),
		  memory_file_to(X, MemFile, Encoding, Data0)
		),
		free_memory_file(MemFile)),
	    Data = Data0
	).
http_read_data(In, Fields, Data, _) :-
	option(content_type(ContentType), Fields),
	is_content_type(ContentType, 'application/x-www-form-urlencoded'), !,
	http_read_data(In, Fields, Codes, [to(string)]),
	uri_query_components(Codes, Data).
http_read_data(In, Fields, Data, Options) :-			% call hook
	(   select_option(content_type(Type), Options, Options1)
	->  delete(Fields, content_type(_), Fields1),
	    http_convert_data(In, [content_type(Type)|Fields1], Data, Options1)
	;   http_convert_data(In, Fields, Data, Options)
	), !.
http_read_data(In, Fields, Data, Options) :-
	http_read_data(In, Fields, Data, [to(atom)|Options]).

memory_file_to(atom, MemFile, Encoding, Data) :-
	memory_file_to_atom(MemFile, Data, Encoding).
memory_file_to(string, MemFile, Encoding, Data) :-
	memory_file_to_string(MemFile, Data, Encoding).
memory_file_to(codes, MemFile, Encoding, Data) :-
	memory_file_to_codes(MemFile, Data, Encoding).


encoding(Fields, utf8) :-
	memberchk(content_type(Type), Fields),
	(   sub_atom(Type, _, _, _, 'UTF-8')
	->  true
	;   sub_atom(Type, _, _, _, 'utf-8')
	), !.
encoding(_, octet).

is_content_type(ContentType, Check) :-
	sub_atom(ContentType, 0, Len, After, Check),
	(   After == 0
	->  true
	;   sub_atom(ContentType, Len, 1, _, ';')
	).

%%	http_convert_data(+In, +Fields, -Data, +Options) is semidet.
%
%	Multi-file hook to convert  a  HTTP   payload  according  to the
%	_Content-type_ header. The  default   implementation  deals with
%	application/x-prolog.    The    HTTP      framework     provides
%	implementations  for  JSON  (library(http/http_json)),  HTML/XML
%	(library(http/http_sgml_plugin))

http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	is_content_type(Type, 'application/x-prolog'), !,
	(   memberchk(content_length(Bytes), Fields)
	->  setup_call_cleanup(
		( stream_range_open(In, Range, [size(Bytes)]),
		  set_stream(Range, encoding(utf8))
		),
		read_term(Range, Data, Options),
		close(Range))
	;   set_stream(In, encoding(utf8)),
	    read_term(In, Data, Options)
	).

		 /*******************************
		 *	       POST		*
		 *******************************/

%%	http_put(+URL, +In, -Out, +Options)
%
%	Issue an HTTP PUT request.

http_put(URL, In, Out, Options) :-
	http_post(URL, In, Out, [method(put)|Options]).


%%	http_post(+URL, +Data, -Reply, +Options) is det.
%
%	Issue   an   HTTP   POST   request.   Data   is   posted   using
%	http_post_data/3. The HTTP server reply   is  returned in Reply,
%	using the same rules as http_get/3.

http_post(URL, Data, Reply, Options) :-
	http_get(URL, Reply,
		 [ post(Data)
		 | Options
		 ]).

%%	http_disconnect(+Connections) is det.
%
%	Close down some connections. Currently Connections must have the
%	value =all=, closing all connections.
%
%	@deprecated	New code should use http_close_keep_alive/1 from
%			library(http/http_open).

http_disconnect(all) :-
	http_close_keep_alive(_).

%%	http:post_data_hook(+Term, +Out, +Options) is semidet.
%
%	Hook to extend the datatypes supported  by the post(Data) option
%	of   http_open/3.   The   default     implementation    supports
%	prolog(Term), sending a Prolog term as =|application/x-prolog|=.

http:post_data_hook(prolog(Term), Out, HdrExtra) :-
	setup_call_cleanup(
	    ( new_memory_file(MemFile),
	      open_memory_file(MemFile, write, Handle)
	    ),
	    ( format(Handle,
		     'Content-type: application/x-prolog; charset=UTF-8~n~n',
		     []),
	      write_term(Handle, Term,
			 [ quoted(true),
			   ignore_ops(true),
			   fullstop(true),
			   nl(true)
			 ])
	    ),
	    close(Handle)),
	setup_call_cleanup(
	    open_memory_file(MemFile, read, RdHandle,
			     [ free_on_close(true)
			     ]),
	    http_post_data(cgi_stream(RdHandle), Out, HdrExtra),
	    close(RdHandle)).
