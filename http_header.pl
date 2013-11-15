/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
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

:- module(http_header,
	  [ http_read_request/2,	% +Stream, -Request
	    http_read_reply_header/2,	% +Stream, -Reply
	    http_reply/2,		% +What, +Stream
	    http_reply/3,		% +What, +Stream, +HdrExtra
	    http_reply/4,		% +What, +Stream, +HdrExtra, -Code
            http_reply/5,		% +What, +Stream, +HdrExtra, +Context, -Code
	    http_reply_header/3,	% +Stream, +What, +HdrExtra
	    http_status_reply/4,	% +Status, +Out, +HdrExtra, -Code
            http_status_reply/5,	% +Status, +Out, +HdrExtra, +Context, -Code

	    http_timestamp/2,		% +Time, -HTTP string

	    http_post_data/3,		% +Stream, +Data, +HdrExtra

	    http_read_header/2,		% +Fd, -Header
	    http_parse_header/2,	% +Codes, -Header
	    http_parse_header_value/3,	% +Header, +HeaderValue, -MediaTypes
	    http_join_headers/3,	% +Default, +InHdr, -OutHdr
	    http_update_encoding/3,	% +HeaderIn, -Encoding, -HeaderOut
	    http_update_connection/4,	% +HeaderIn, +Request, -Connection, -HeaderOut
	    http_update_transfer/4	% +HeaderIn, +Request, -Transfer, -HeaderOut
	  ]).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(uri)).
:- use_module(library(memfile)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(socket)).
:- use_module(library(dcg/basics)).
:- use_module(html_write).
:- use_module(http_exception).
:- use_module(mimetype).
:- use_module(mimepack).


% see http_update_transfer/4.

:- setting(http:chunked_transfer, oneof([never,on_request,if_possible]),
	   on_request, 'When to use Transfer-Encoding: Chunked').


/** <module> Handling HTTP headers

The library library(http/http_header) provides   primitives  for parsing
and composing HTTP headers. Its functionality  is normally hidden by the
other parts of the HTTP server and client libraries.
*/


		 /*******************************
		 *	    READ REQUEST	*
		 *******************************/

%%	http_read_request(+FdIn:stream, -Request) is det.
%
%	Read an HTTP request-header from FdIn and return the broken-down
%	request fields as +Name(+Value) pairs  in   a  list.  Request is
%	unified to =end_of_file= if FdIn is at the end of input.

http_read_request(In, Request) :-
	catch(read_line_to_codes(In, Codes), E, true),
	(   var(E)
	->  (   Codes == end_of_file
	    ->  debug(http(header), 'end-of-file', []),
		Request = end_of_file
	    ;   debug(http(header), 'First line: ~s', [Codes]),
		Request =  [input(In)|Request1],
		phrase(request(In, Request1), Codes),
		(   Request1 = [unknown(Text)|_]
		->  atom_codes(S, Text),
		    syntax_error(http_request(S))
		;   true
		)
	    )
	;   message_to_string(E, Msg),
	    debug(http(request), 'Exception reading 1st line: ~s', [Msg]),
	    Request = end_of_file
	).


%%	http_read_reply_header(+FdIn, -Reply)
%
%	Read the HTTP reply header. Throws   an exception if the current
%	input does not contain a valid reply header.

http_read_reply_header(In, [input(In)|Reply]) :-
	read_line_to_codes(In, Codes),
	(   Codes == end_of_file
	->  debug(http(header), 'end-of-file', []),
	    throw(error(syntax(http_reply_header, end_of_file), _))
	;   debug(http(header), 'First line: ~s~n', [Codes]),
	    (   phrase(reply(In, Reply), Codes)
	    ->  true
	    ;   atom_codes(Header, Codes),
		syntax_error(http_reply_header(Header))
	    )
	).


		 /*******************************
		 *	  FORMULATE REPLY	*
		 *******************************/

%%	http_reply(+Data, +Out:stream) is det.
%%	http_reply(+Data, +Out:stream, +HdrExtra) is det.
%%	http_reply(+Data, +Out:stream, +HdrExtra, -Code) is det.
%%	http_reply(+Data, +Out:stream, +HdrExtra, +Context, -Code) is det.
%
%	Compose  a  complete  HTTP  reply  from   the  term  Data  using
%	additional headers from  HdrExtra  to   the  output  stream Out.
%	ExtraHeader is a list of Field(Value). Data is one of:
%
%		* html(HTML)
%		HTML tokens as produced by html//1 from html_write.pl
%
%		* file(+MimeType, +FileName)
%		Reply content of FileName using MimeType
%
%		* file(+MimeType, +FileName, +Range)
%		Reply partial content of FileName with given MimeType
%
%		* tmp_file(+MimeType, +FileName)
%		Same as =file=, but do not include modification time
%
%		* stream(+In, +Len)
%		Reply content of stream.
%
%		* cgi_stream(+In, +Len)
%		Reply content of stream, which should start with an
%		HTTP header, followed by a blank line.  This is the
%		typical output from a CGI script.
%
%		* Status
%		HTTP status report and defined by http_status_reply/4.
%
%	@param HdrExtra provides additional reply-header fields, encoded
%	       as Name(Value). It can also contain a field
%	       content_length(-Len) to _retrieve_ the
%	       value of the Content-length header that is replied.
%	@param Code is the numeric HTTP status code sent
%
%	@tbd	Complete documentation

http_reply(What, Out) :-
	http_reply(What, Out, [connection(close)], _).

http_reply(Data, Out, HdrExtra) :-
	http_reply(Data, Out, HdrExtra, _Code).

http_reply(Status, Out, HdrExtra, Code) :-
	http_status_reply(Status, Out, HdrExtra, [], Code).

http_reply(Data, Out, HdrExtra, _Context, Code) :-
	byte_count(Out, C0),
	catch(http_reply_data(Data, Out, HdrExtra, Code), E, true), !,
	(   var(E)
	->  true
	;   E = error(io_error(write, _), _)
	->  byte_count(Out, C1),
	    Sent is C1 - C0,
	    throw(error(http_write_short(Data, Sent), _))
	;   E = error(timeout_error(write, _), _)
	->  throw(E)
	;   map_exception_to_http_status(E, Status, NewHdr, NewContext),
	    http_status_reply(Status, Out, NewHdr, NewContext, Code)
	).
http_reply(Status, Out, HdrExtra, Context, Code) :-
	http_status_reply(Status, Out, HdrExtra, Context, Code).



%%	http_reply_data(+Data, +Out, +HdrExtra, -Code) is semidet.
%
%	Fails if Data is not a defined   reply-data format, but a status
%	term. See http_reply/3 and http_status_reply/4.
%
%	@error Various I/O errors.

http_reply_data(html(HTML), Out, HdrExtra, Code) :- !,
	phrase(reply_header(html(HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply_data(file(Type, File), Out, HdrExtra, Code) :- !,
	phrase(reply_header(file(Type, File), HdrExtra, Code), Header),
	reply_file(Out, File, Header).
http_reply_data(file(Type, File, Range), Out, HdrExtra, Code) :- !,
	phrase(reply_header(file(Type, File, Range), HdrExtra, Code), Header),
	reply_file_range(Out, File, Header, Range).
http_reply_data(tmp_file(Type, File), Out, HdrExtra, Code) :- !,
	phrase(reply_header(tmp_file(Type, File), HdrExtra, Code), Header),
	reply_file(Out, File, Header).
http_reply_data(stream(In, Len), Out, HdrExtra, Code) :- !,
	phrase(reply_header(cgi_data(Len), HdrExtra, Code), Header),
	copy_stream(Out, In, Header, 0, end).
http_reply_data(cgi_stream(In, Len), Out, HdrExtra, Code) :- !,
	http_read_header(In, CgiHeader),
	seek(In, 0, current, Pos),
	Size is Len - Pos,
	http_join_headers(HdrExtra, CgiHeader, Hdr2),
	phrase(reply_header(cgi_data(Size), Hdr2, Code), Header),
	copy_stream(Out, In, Header, 0, end).

reply_file(Out, File, Header) :-
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    copy_stream(Out, In, Header, 0, end),
	    close(In)).

reply_file_range(Out, File, Header, bytes(From, To)) :- !,
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    copy_stream(Out, In, Header, From, To),
	    close(In)).

copy_stream(Out, In, Header, From, To) :-
	(   From == 0
	->  true
	;   seek(In, From, bof, _)
	),
	peek_byte(In, _),
	format(Out, '~s', [Header]),
	(   To == end
	->  copy_stream_data(In, Out)
	;   Len is To - From,
	    copy_stream_data(In, Out, Len)
	),
	flush_output(Out).


%%	http_status_reply(+Status, +Out, +HdrExtra, -Code) is det.
%%	http_status_reply(+Status, +Out, +HdrExtra, +Context, -Code) is det.
%
%	Emit HTML non-200 status reports. Such  requests are always sent
%	as UTF-8 documents.

http_status_reply(Status, Out, HdrExtra, Code) :-
        http_status_reply(Status, Out, HdrExtra, [], Code).

http_status_reply(Status, Out, HdrExtra, Context, Code) :-
	setup_call_cleanup(
	    set_stream(Out, encoding(utf8)),
	    status_reply(Status, Out, HdrExtra, Context, Code),
	    set_stream(Out, encoding(octet))), !.


status_reply(no_content, Out, HdrExtra, _Context, Code) :- !,
	phrase(reply_header(status(no_content), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	flush_output(Out).
status_reply(switching_protocols(_,_), Out, HdrExtra, _Context, Code) :- !,
	phrase(reply_header(status(switching_protocols), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	flush_output(Out).
status_reply(created(Location), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('201 Created')
		    ],
		    [ h1('Created'),
		      p(['The document was created ',
			 a(href(Location), ' Here')
			]),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(created(Location, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(moved(To), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('301 Moved Permanently')
		    ],
		    [ h1('Moved Permanently'),
		      p(['The document has moved ',
			 a(href(To), ' Here')
			]),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(moved(To, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(moved_temporary(To), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('302 Moved Temporary')
		    ],
		    [ h1('Moved Temporary'),
		      p(['The document is currently ',
			 a(href(To), ' Here')
			]),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(moved_temporary(To, HTML),
			    HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(see_other(To),Out,HdrExtra, _Context, Code) :- !,
       phrase(page([ title('303 See Other')
                    ],
                    [ h1('See Other'),
                      p(['See other document ',
                         a(href(To), ' Here')
                        ]),
                      \address
                    ]),
               HTML),
        phrase(reply_header(see_other(To, HTML), HdrExtra, Code), Header),
        format(Out, '~s', [Header]),
        print_html(Out, HTML).
status_reply(bad_request(ErrorTerm), Out, HdrExtra, _Context, Code) :- !,
	'$messages':translate_message(ErrorTerm, Lines, []),
	phrase(page([ title('400 Bad Request')
		    ],
		    [ h1('Bad Request'),
		      p(\html_message_lines(Lines)),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(bad_request, HTML),
			    HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(not_found(URL), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('404 Not Found')
		    ],
		    [ h1('Not Found'),
		      p(['The requested URL ', tt(URL),
			 ' was not found on this server'
			]),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(not_found, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(forbidden(URL), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('403 Forbidden')
		    ],
		    [ h1('Forbidden'),
		      p(['You don\'t have permission to access ', URL,
			 ' on this server'
			]),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(forbidden, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(authorise(basic, ''), Out, HdrExtra, Context, Code) :- !,
        status_reply(authorise(basic), Out, HdrExtra, Context, Code).
status_reply(authorise(basic, Realm), Out, HdrExtra, Context, Code) :- !,
        status_reply(authorise(basic(Realm)), Out, HdrExtra, Context, Code).
status_reply(authorise(Method), Out, HdrExtra, Context, Code) :- !,
        status_page_hook(401, Context, HTML),
	phrase(reply_header(authorise(Method, HTML),
			    HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(not_modified, Out, HdrExtra, _Context, Code) :- !,
	phrase(reply_header(status(not_modified), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	flush_output(Out).
status_reply(server_error(ErrorTerm), Out, HdrExtra, _Context, Code) :-
	'$messages':translate_message(ErrorTerm, Lines, []),
	phrase(page([ title('500 Internal server error')
		    ],
		    [ h1('Internal server error'),
		      p(\html_message_lines(Lines)),
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(server_error, HTML),
			    HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(not_acceptable(WhyHTML), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('406 Not Acceptable')
		    ],
		    [ h1('Not Acceptable'),
		      WhyHTML,
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(not_acceptable, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(unavailable(WhyHTML), Out, HdrExtra, _Context, Code) :- !,
	phrase(page([ title('503 Service Unavailable')
		    ],
		    [ h1('Service Unavailable'),
		      WhyHTML,
		      \address
		    ]),
	       HTML),
	phrase(reply_header(status(service_unavailable, HTML), HdrExtra, Code), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
status_reply(resource_error(ErrorTerm), Out, HdrExtra, Context, Code) :- !,
	'$messages':translate_message(ErrorTerm, Lines, []),
	status_reply(unavailable(p(\html_message_lines(Lines))),
		     Out, HdrExtra, Context, Code).
status_reply(busy, Out, HdrExtra, Context, Code) :- !,
	HTML = p(['The server is temporarily out of resources, ',
		  'please try again later']),
	http_status_reply(unavailable(HTML), Out, HdrExtra, Context, Code).

:-multifile(http:status_page/3).

status_page_hook(Status, Context, HTML):-
        http:status_page(Status, Context, HTML), !.

status_page_hook(401, _, HTML):-
        phrase(page([ title('401 Authorization Required')
		    ],
		    [ h1('Authorization Required'),
		      p(['This server could not verify that you ',
			 'are authorized to access the document ',
			 'requested.  Either you supplied the wrong ',
			 'credentials (e.g., bad password), or your ',
			 'browser doesn\'t understand how to supply ',
			 'the credentials required.'
			]),
		      \address
		    ]),
	       HTML).


html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	html([br([])]),
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([Fmt-Args|T]) --> !,
	{ format(string(S), Fmt, Args)
	},
	html([S]),
	html_message_lines(T).
html_message_lines([Fmt|T]) --> !,
	{ format(string(S), Fmt, [])
	},
	html([S]),
	html_message_lines(T).

%%	http_join_headers(+Default, +Header, -Out)
%
%	Append headers from Default to Header if they are not
%	already part of it.

http_join_headers([], H, H).
http_join_headers([H|T], Hdr0, Hdr) :-
	functor(H, N, A),
	functor(H2, N, A),
	member(H2, Hdr0), !,
	http_join_headers(T, Hdr0, Hdr).
http_join_headers([H|T], Hdr0, [H|Hdr]) :-
	http_join_headers(T, Hdr0, Hdr).


%%	http_update_encoding(+HeaderIn, -Encoding, -HeaderOut)
%
%	Allow for rewrite of the  header,   adjusting  the  encoding. We
%	distinguish three options. If  the   user  announces  `text', we
%	always use UTF-8 encoding. If   the user announces charset=utf-8
%	we  use  UTF-8  and  otherwise  we  use  octet  (raw)  encoding.
%	Alternatively we could dynamically choose for ASCII, ISO-Latin-1
%	or UTF-8.

http_update_encoding(Header0, utf8, [content_type(Type)|Header]) :-
	select(content_type(Type0), Header0, Header),
	sub_atom(Type0, 0, _, _, 'text/'), !,
	(   sub_atom(Type0, S, _, _, ';')
	->  sub_atom(Type0, 0, S, _, B)
	;   B = Type0
	),
	atom_concat(B, '; charset=UTF-8', Type).
http_update_encoding(Header, Encoding, Header) :-
	memberchk(content_type(Type), Header),
	(   (   sub_atom(Type, _, _, _, 'UTF-8')
	    ;   sub_atom(Type, _, _, _, 'utf-8')
	    )
	->  Encoding = utf8
	;   mime_type_encoding(Type, Encoding)
	).
http_update_encoding(Header, octet, Header).

%%	mime_type_encoding(+MimeType, -Encoding) is semidet.
%
%	Encoding is the (default) character encoding for MimeType.

mime_type_encoding('application/json', utf8).
mime_type_encoding('application/jsonrequest', utf8).


%%	http_update_connection(+CGIHeader, +Request, -Connection, -Header)
%
%	Merge keep-alive information from  Request   and  CGIHeader into
%	Header.

http_update_connection(CgiHeader, Request, Connect, [connection(Connect)|Rest]) :-
	select(connection(CgiConn), CgiHeader, Rest), !,
	connection(Request, ReqConnection),
	join_connection(ReqConnection, CgiConn, Connect).
http_update_connection(CgiHeader, Request, Connect, [connection(Connect)|CgiHeader]) :-
	connection(Request, Connect).

join_connection(Keep1, Keep2, Connection) :-
	(   downcase_atom(Keep1, 'keep-alive'),
	    downcase_atom(Keep2, 'keep-alive')
	->  Connection = 'Keep-Alive'
	;   Connection = close
	).


%%	connection(+Header, -Connection)
%
%	Extract the desired connection from a header.

connection(Header, Close) :-
	(   memberchk(connection(Connection), Header)
	->  Close = Connection
	;   memberchk(http_version(1-X), Header),
	    X >= 1
	->  Close = 'Keep-Alive'
	;   Close = close
	).


%%	http_update_transfer(+Request, +CGIHeader, -Transfer, -Header)
%
%	Decide on the transfer encoding  from   the  Request and the CGI
%	header.    The    behaviour    depends      on    the    setting
%	http:chunked_transfer. If =never=, even   explitic  requests are
%	ignored. If =on_request=, chunked encoding  is used if requested
%	through  the  CGI  header  and  allowed    by   the  client.  If
%	=if_possible=, chunked encoding is  used   whenever  the  client
%	allows for it, which is  interpreted   as  the client supporting
%	HTTP 1.1 or higher.
%
%	Chunked encoding is more space efficient   and allows the client
%	to start processing partial results. The drawback is that errors
%	lead to incomplete pages instead of  a nicely formatted complete
%	page.

http_update_transfer(Request, CgiHeader, Transfer, Header) :-
	setting(http:chunked_transfer, When),
	http_update_transfer(When, Request, CgiHeader, Transfer, Header).

http_update_transfer(never, _, CgiHeader, none, Header) :- !,
	delete(CgiHeader, transfer_encoding(_), Header).
http_update_transfer(_, _, CgiHeader, none, Header) :-
	memberchk(location(_), CgiHeader), !,
	delete(CgiHeader, transfer_encoding(_), Header).
http_update_transfer(_, Request, CgiHeader, Transfer, Header) :-
	select(transfer_encoding(CgiTransfer), CgiHeader, Rest), !,
	transfer(Request, ReqConnection),
	join_transfer(ReqConnection, CgiTransfer, Transfer),
	(   Transfer == none
	->  Header = Rest
	;   Header = [transfer_encoding(Transfer)|Rest]
	).
http_update_transfer(if_possible, Request, CgiHeader, Transfer, Header) :-
	transfer(Request, Transfer),
	Transfer \== none, !,
	Header = [transfer_encoding(Transfer)|CgiHeader].
http_update_transfer(_, _, CgiHeader, none, CgiHeader).

join_transfer(chunked, chunked, chunked) :- !.
join_transfer(_, _, none).


%%	transfer(+Header, -Connection)
%
%	Extract the desired connection from a header.

transfer(Header, Transfer) :-
	(   memberchk(transfer_encoding(Transfer0), Header)
	->  Transfer = Transfer0
	;   memberchk(http_version(1-X), Header),
	    X >= 1
	->  Transfer = chunked
	;   Transfer = none
	).


%%	content_length_in_encoding(+Encoding, +In, -Bytes)
%
%	Determine hom many bytes are required to represent the data from
%	stream In using the given encoding.  Fails if the data cannot be
%	represented with the given encoding.

content_length_in_encoding(Enc, Stream, Bytes) :-
	stream_property(Stream, position(Here)),
	setup_call_cleanup(
	    open_null_stream(Out),
	    ( set_stream(Out, encoding(Enc)),
	      catch(copy_stream_data(Stream, Out), _, fail),
	      flush_output(Out),
	      byte_count(Out, Bytes)
	    ),
	    ( close(Out, [force(true)]),
	      set_stream_position(Stream, Here)
	    )).


		 /*******************************
		 *	    POST SUPPORT	*
		 *******************************/

%%	http_post_data(+Data, +Out:stream, +HdrExtra) is det.
%
%	Send data on behalf on an HTTP   POST request. This predicate is
%	normally called by http_post/4 from   http_client.pl to send the
%	POST data to the server.  Data is one of:
%
%	  * html(+Tokens)
%	  Result of html//1 from html_write.pl
%
%	  * xml(+Term)
%	  Post the result of xml_write/3 using the Mime-type
%	  =|text/xml|=
%
%	  * xml(+Type, +Term)
%	  Post the result of xml_write/3 using the given Mime-type
%	  and an empty option list to xml_write/3.
%
%	  * xml(+Type, +Term, +Options)
%	  Post the result of xml_write/3 using the given Mime-type
%	  and option list for xml_write/3.
%
%	  * file(+File)
%	  Send contents of a file. Mime-type is determined by
%	  file_mime_type/2.
%
%	  * file(+Type, +File)
%	  Send file with content of indicated mime-type.
%
%	  * memory_file(+Type, +Handle)
%	  Similar to file(+Type, +File), but using a memory file
%	  instead of a real file.  See new_memory_file/1.
%
%	  * codes(+Codes)
%	  As codes(text/plain, Codes).
%
%	  * codes(+Type, +Codes)
%	  Send Codes using the indicated MIME-type.
%
%	  * atom(+Atom)
%	  As atom(text/plain, Atom).
%
%	  * atom(+Type, +Atom)
%	  Send Atom using the indicated MIME-type.
%
%	  * cgi_stream(+Stream, +Len) Read the input from Stream which,
%	  like CGI data starts with a partial HTTP header. The fields of
%	  this header are merged with the provided HdrExtra fields. The
%	  first Len characters of Stream are used.
%
%	  * form(+ListOfParameter)
%	  Send data of the MIME type application/x-www-form-urlencoded as
%	  produced by browsers issuing a POST request from an HTML form.
%	  ListOfParameter is a list of Name=Value or Name(Value).
%
%	  * form_data(+ListOfData)
%	  Send data of the MIME type multipart/form-data.  ListOfData is the same
%	  as for the List alternative described below.
%
%	  * List
%	  If the argument is a plain list, it is sent using the MIME type
%	  multipart/mixed and packed using mime_pack/3. See mime_pack/3
%	  for details on the argument format.

:- multifile
	http_client:post_data_hook/3.

http_post_data(Data, Out, HdrExtra) :-
	http_client:post_data_hook(Data, Out, HdrExtra), !.
http_post_data(html(HTML), Out, HdrExtra) :- !,
	phrase(post_header(html(HTML), HdrExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_post_data(xml(XML), Out, HdrExtra) :- !,
	http_post_data(xml(text/xml, XML, []), Out, HdrExtra).
http_post_data(xml(Type, XML), Out, HdrExtra) :- !,
	http_post_data(xml(Type, XML, []), Out, HdrExtra).
http_post_data(xml(Type, XML, Options), Out, HdrExtra) :- !,
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    (   setup_call_cleanup(
		    open_memory_file(MemFile, write, MemOut),
		    xml_write(MemOut, XML, Options),
		    close(MemOut)),
		http_post_data(memory_file(Type, MemFile), Out, HdrExtra)
	    ),
	    free_memory_file(MemFile)).
http_post_data(file(File), Out, HdrExtra) :- !,
	(   file_mime_type(File, Type)
	->  true
	;   Type = text/plain
	),
	http_post_data(file(Type, File), Out, HdrExtra).
http_post_data(file(Type, File), Out, HdrExtra) :- !,
	phrase(post_header(file(Type, File), HdrExtra), Header),
	format(Out, '~s', [Header]),
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    copy_stream_data(In, Out),
	    close(In)).
http_post_data(memory_file(Type, Handle), Out, HdrExtra) :- !,
	phrase(post_header(memory_file(Type, Handle), HdrExtra), Header),
	format(Out, '~s', [Header]),
	setup_call_cleanup(
	    open_memory_file(Handle, read, In, [encoding(octet)]),
	    copy_stream_data(In, Out),
	    close(In)).
http_post_data(codes(Codes), Out, HdrExtra) :- !,
	http_post_data(codes(text/plain, Codes), Out, HdrExtra).
http_post_data(codes(Type, Codes), Out, HdrExtra) :- !,
	phrase(post_header(codes(Type, Codes), HdrExtra), Header),
	format(Out, '~s', [Header]),
	setup_call_cleanup(
	    set_stream(Out, encoding(utf8)),
	    format(Out, '~s', [Codes]),
	    set_stream(Out, encoding(octet))).
http_post_data(atom(Atom), Out, HdrExtra) :- !,
	http_post_data(atom(text/plain, Atom), Out, HdrExtra).
http_post_data(atom(Type, Atom), Out, HdrExtra) :- !,
	phrase(post_header(atom(Type, Atom), HdrExtra), Header),
	format(Out, '~s', [Header]),
	setup_call_cleanup(
	    set_stream(Out, encoding(utf8)),
	    write(Out, Atom),
	    set_stream(Out, encoding(octet))).
http_post_data(cgi_stream(In, _Len), Out, HdrExtra) :- !,
	debug(obsolete, 'Obsolete 2nd argument in cgi_stream(In,Len)', []),
	http_post_data(cgi_stream(In), Out, HdrExtra).
http_post_data(cgi_stream(In), Out, HdrExtra) :- !,
	http_read_header(In, Header0),
	http_update_encoding(Header0, Encoding, Header),
	content_length_in_encoding(Encoding, In, Size),
	http_join_headers(HdrExtra, Header, Hdr2),
	phrase(post_header(cgi_data(Size), Hdr2), HeaderText),
	format(Out, '~s', [HeaderText]),
	setup_call_cleanup(
	    set_stream(Out, encoding(Encoding)),
	    copy_stream_data(In, Out),
	    set_stream(Out, encoding(octet))).
http_post_data(form(Fields), Out, HdrExtra) :- !,
	parse_url_search(Codes, Fields),
	length(Codes, Size),
	http_join_headers(HdrExtra,
			  [ content_type('application/x-www-form-urlencoded')
			  ], Header),
	phrase(post_header(cgi_data(Size), Header), HeaderChars),
	format(Out, '~s', [HeaderChars]),
	format(Out, '~s', [Codes]).
http_post_data(form_data(Data), Out, HdrExtra) :- !,
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    ( setup_call_cleanup(
		  open_memory_file(MemFile, write, MimeOut),
		  mime_pack(Data, MimeOut, Boundary),
		  close(MimeOut)),
	      size_memory_file(MemFile, Size, octet),
	      format(string(ContentType),
		     'multipart/form-data; boundary=~w', [Boundary]),
	      http_join_headers(HdrExtra,
				[ mime_version('1.0'),
				  content_type(ContentType)
				], Header),
	      phrase(post_header(cgi_data(Size), Header), HeaderChars),
	      format(Out, '~s', [HeaderChars]),
	      setup_call_cleanup(
		  open_memory_file(MemFile, read, In, [encoding(octet)]),
		  copy_stream_data(In, Out),
		  close(In))
	    ),
	    free_memory_file(MemFile)).
http_post_data(List, Out, HdrExtra) :-		% multipart-mixed
	is_list(List), !,
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    ( setup_call_cleanup(
		  open_memory_file(MemFile, write, MimeOut),
		  mime_pack(List, MimeOut, Boundary),
		  close(MimeOut)),
	      size_memory_file(MemFile, Size, octet),
	      format(string(ContentType),
		     'multipart/mixed; boundary=~w', [Boundary]),
	      http_join_headers(HdrExtra,
				[ mime_version('1.0'),
				  content_type(ContentType)
				], Header),
	      phrase(post_header(cgi_data(Size), Header), HeaderChars),
	      format(Out, '~s', [HeaderChars]),
	      setup_call_cleanup(
		  open_memory_file(MemFile, read, In, [encoding(octet)]),
		  copy_stream_data(In, Out),
		  close(In))
	    ),
	    free_memory_file(MemFile)).

%%	post_header(+Data, +HeaderExtra)//
%
%	Generate the POST header, emitting HeaderExtra, followed by the
%	HTTP Content-length and Content-type fields.

post_header(html(Tokens), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(html(Tokens), Len),
	content_type(text/html),
	"\r\n".
post_header(file(Type, File), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(file(File), Len),
	content_type(Type),
	"\r\n".
post_header(memory_file(Type, File), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(memory_file(File), Len),
	content_type(Type),
	"\r\n".
post_header(cgi_data(Size), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(Size, Len),
	"\r\n".
post_header(codes(Type, Codes), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(codes(Codes, utf8), Len),
	content_type(Type, utf8),
	"\r\n".
post_header(atom(Type, Atom), HdrExtra) -->
	header_fields(HdrExtra, Len),
	content_length(atom(Atom, utf8), Len),
	content_type(Type, utf8),
	"\r\n".


		 /*******************************
		 *       OUTPUT HEADER DCG	*
		 *******************************/

%%	http_reply_header(+Out:stream, +What, +HdrExtra) is det.
%
%	Create a reply header  using  reply_header//3   and  send  it to
%	Stream.

http_reply_header(Out, What, HdrExtra) :-
	phrase(reply_header(What, HdrExtra, _Code), String), !,
	format(Out, '~s', [String]).

%%	reply_header(+Data, +HdrExtra, -Code)// is det.
%
%	Grammar that realises the HTTP handler for sending Data. Data is
%	a  real  data  object  as  described   with  http_reply/2  or  a
%	not-200-ok HTTP status reply. The   following status replies are
%	defined.
%
%	  * moved(+URL, +HTMLTokens)
%	  * created(+URL, +HTMLTokens)
%	  * moved_temporary(+URL, +HTMLTokens)
%	  * see_other(+URL, +HTMLTokens)
%	  * status(+Status)
%	  * status(+Status, +HTMLTokens)
%	  * authorise(+Method, +Realm, +Tokens)
%	  * authorise(+Method, +Tokens)
%
%	@see http_status_reply/4 formulates the not-200-ok HTTP replies.

reply_header(string(String), HdrExtra, Code) -->
	reply_header(string(text/plain, String), HdrExtra, Code).
reply_header(string(Type, String), HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	header_fields(HdrExtra, CLen),
	content_length(codes(String, utf8), CLen),
	content_type(Type, utf8),
	"\r\n".
reply_header(html(Tokens), HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html),
	"\r\n".
reply_header(file(Type, File), HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	modified(file(File)),
	header_fields(HdrExtra, CLen),
	content_length(file(File), CLen),
	content_type(Type),
	"\r\n".
reply_header(file(Type, File, Range), HdrExtra, Code) -->
	vstatus(partial_content, Code),
	date(now),
	modified(file(File)),
	header_fields(HdrExtra, CLen),
	content_length(file(File, Range), CLen),
	content_type(Type),
	"\r\n".
reply_header(tmp_file(Type, File), HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	header_fields(HdrExtra, CLen),
	content_length(file(File), CLen),
	content_type(Type),
	"\r\n".
reply_header(cgi_data(Size), HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	header_fields(HdrExtra, CLen),
	content_length(Size, CLen),
	"\r\n".
reply_header(chunked_data, HdrExtra, Code) -->
	vstatus(ok, Code),
	date(now),
	header_fields(HdrExtra, _),
	(   {memberchk(transfer_encoding(_), HdrExtra)}
	->  ""
	;   transfer_encoding(chunked)
	),
	"\r\n".
reply_header(moved(To, Tokens), HdrExtra, Code) -->
	vstatus(moved, Code),
	date(now),
	header_field('Location', To),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".
reply_header(created(Location, Tokens), HdrExtra, Code) -->
	vstatus(created, Code),
	date(now),
	header_field('Location', Location),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".
reply_header(moved_temporary(To, Tokens), HdrExtra, Code) -->
	vstatus(moved_temporary, Code),
	date(now),
	header_field('Location', To),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".
reply_header(see_other(To,Tokens),HdrExtra, Code) -->
	vstatus(see_other, Code),
	date(now),
	header_field('Location',To),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".
reply_header(status(Status), HdrExtra, Code) --> % Empty messages: 1xx, 204 and 304
	vstatus(Status, Code),
	header_fields(HdrExtra, Clen),
	{ Clen = 0 },
	"\r\n".
reply_header(status(Status, Tokens), HdrExtra, Code) -->
	vstatus(Status, Code),
	date(now),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".
reply_header(authorise(Method, Tokens), HdrExtra, Code) -->
	vstatus(authorise, Code),
	date(now),
	authenticate(Method),
	header_fields(HdrExtra, CLen),
	content_length(html(Tokens), CLen),
	content_type(text/html, utf8),
	"\r\n".

vstatus(Status, Code) -->
	"HTTP/1.1 ",
	status_number(Status, Code),
	" ",
	status_comment(Status),
	"\r\n".

%%	status_number(?Status, ?Code)// is semidet.
%
%	Parse/generate the HTTP status  numbers  and   map  them  to the
%	proper name.
%
%	@see See the source code for supported status names and codes.

status_number(Status, Code) -->
	{ var(Status) }, !,
	integer(Code),
	{ status_number(Status, Code) }, !.
status_number(Status, Code) -->
	{ status_number(Status, Code) },
	integer(Code).

status_number(continue,		   100).
status_number(switching_protocols, 101).
status_number(ok,		   200).
status_number(created,		   201).
status_number(accepted,		   202).
status_number(no_content,	   204).
status_number(partial_content,	   206).
status_number(moved,		   301).
status_number(moved_temporary,	   302).
status_number(see_other,	   303).
status_number(not_modified,	   304).
status_number(bad_request,	   400).
status_number(authorise,	   401).
status_number(forbidden,	   403).
status_number(not_found,	   404).
status_number(not_acceptable,      406).
status_number(server_error,	   500).
status_number(service_unavailable, 503).


%%	status_comment(+Code:atom)// is det.
%
%	Emit standard HTTP human-readable comment on the reply-status.

status_comment(continue) -->
	"Continue".
status_comment(switching_protocols) -->
	"Switching Protocols".
status_comment(ok) -->
	"OK".
status_comment(created) -->
	"Created".
status_comment(accepted) -->
	"Accepted".
status_comment(no_content) -->
	"No Content".
status_comment(created) -->
	"Created".
status_comment(partial_content) -->
	"Partial content".
status_comment(moved) -->
	"Moved Permanently".
status_comment(moved_temporary) -->
	"Moved Temporary".
status_comment(see_other) -->
	"See Other".
status_comment(not_modified) -->
	"Not Modified".
status_comment(bad_request) -->
	"Bad Request".
status_comment(not_found) -->
	"Not Found".
status_comment(forbidden) -->
	"Forbidden".
status_comment(authorise) -->
	"Authorization Required".
status_comment(server_error) -->
	"Internal Server Error".
status_comment(service_unavailable) -->
	"Service Unavailable".
status_comment(not_acceptable) -->
	"Not Acceptable".

authenticate(negotiate(Data)) -->
	"WWW-Authenticate: Negotiate ",
        {base64(Data, DataBase64)},
        DataBase64, "\r\n".
authenticate(negotiate) -->
	"WWW-Authenticate: Negotiate\r\n".

authenticate(basic) --> !,
	"WWW-Authenticate: Basic\r\n".
authenticate(basic(Realm)) -->
        "WWW-Authenticate: Basic Realm=\"", atom(Realm), "\"\r\n".

authenticate(digest) --> !,
	"WWW-Authenticate: Digest\r\n".
authenticate(digest(Realm)) -->
        "WWW-Authenticate: Digest Realm=\"", atom(Realm), "\"\r\n".


date(Time) -->
	"Date: ",
	(   { Time == now }
	->  now
	;   rfc_date(Time)
	),
	"\r\n".

modified(file(File)) --> !,
	{ time_file(File, Time)
	},
	modified(Time).
modified(Time) -->
	"Last-modified: ",
	(   { Time == now }
	->  now
	;   rfc_date(Time)
	),
	"\r\n".


%%	content_length(+Object, ?Len)// is det.
%
%	Emit the content-length field and (optionally) the content-range
%	field.
%
%	@param Len Number of bytes specified

content_length(file(File, bytes(From, To)), Len) --> !,
	{ size_file(File, Size),
	  (   To == end
	  ->  Len is Size - From,
	      RangeEnd is Size - 1
	  ;   Len is To+1 - From,	% To is index of last byte
	      RangeEnd = To
	  )
	},
	content_range(bytes, From, RangeEnd, Size),
	content_length(Len, Len).
content_length(Reply, Len) -->
	{ length_of(Reply, Len)
	},
	"Content-Length: ", integer(Len),
	"\r\n".


length_of(_, Len) :-
	nonvar(Len), !.
length_of(codes(String, Encoding), Len) :- !,
	setup_call_cleanup(
	    open_null_stream(Out),
	    ( set_stream(Out, encoding(Encoding)),
	      format(Out, '~s', [String]),
	      byte_count(Out, Len)
	    ),
	    close(Out)).
length_of(atom(Atom, Encoding), Len) :- !,
	setup_call_cleanup(
	    open_null_stream(Out),
	    ( set_stream(Out, encoding(Encoding)),
	      format(Out, '~a', [Atom]),
	      byte_count(Out, Len)
	    ),
	    close(Out)).
length_of(file(File), Len) :- !,
	size_file(File, Len).
length_of(memory_file(Handle), Len) :- !,
	size_memory_file(Handle, Len, octet).
length_of(html(Tokens), Len) :- !,
	html_print_length(Tokens, Len).
length_of(Len, Len).


%%	content_range(+Unit:atom, +From:int, +RangeEnd:int, +Size:int)// is det
%
%	Emit the =|Content-Range|= header  for   partial  content  (206)
%	replies.

content_range(Unit, From, RangeEnd, Size) -->
	"Content-Range: ", atom(Unit), " ",
	integer(From), "-", integer(RangeEnd), "/", integer(Size),
	"\r\n".

transfer_encoding(Encoding) -->
	"Transfer-Encoding: ", atom(Encoding), "\r\n".

content_type(Type) -->
	content_type(Type, _).

content_type(Type, Charset) -->
	ctype(Type),
	charset(Charset),
	"\r\n".

ctype(Main/Sub) --> !,
	"Content-Type: ",
	atom(Main),
	"/",
	atom(Sub).
ctype(Type) --> !,
	"Content-Type: ",
	atom(Type).

charset(Var) -->
	{ var(Var) }, !.
charset(utf8) --> !,
	"; charset=UTF-8".
charset(CharSet) -->
	"; charset=",
	atom(CharSet).

%%	header_field(-Name, -Value)// is det.
%%	header_field(+Name, +Value) is det.
%
%	Process an HTTP request property. Request properties appear as a
%	single line in an HTTP header.

header_field(Name, Value) -->
	{ var(Name) }, !,		% parsing
	field_name(Name),
	":",
	whites,
	read_field_value(ValueChars),
	blanks_to_nl, !,
	{   field_to_prolog(Name, ValueChars, Value)
	->  true
	;   atom_codes(Value, ValueChars),
	    domain_error(Name, Value)
	}.
header_field(Name, Value) -->
	field_name(Name),
	": ",
	field_value(Value),
	"\r\n".

%%	read_field_value(-Codes)//
%
%	Read a field eagerly upto the next whitespace

read_field_value([H|T]) -->
	[H],
	{ \+ code_type(H, space) }, !,
	read_field_value(T).
read_field_value([]) -->
	"".
read_field_value([H|T]) -->
	[H],
	read_field_value(T).


%%	http_parse_header_value(+Field, +Value, -Prolog) is semidet.
%
%	Translate Value in a meaningful Prolog   term. Field denotes the
%	HTTP request field for which we   do  the translation. Supported
%	fields are:
%
%	  * content_length
%	  Converted into an integer
%	  * cookie
%	  Converted into a list with Name=Value by cookies//1.
%	  * set_cookie
%	  Converted into a term set_cookie(Name, Value, Options).
%	  Options is a list consisting of Name=Value or a single
%	  atom (e.g., =secure=)
%	  * host
%	  Converted to HostName:Port if applicable.
%	  * range
%	  Converted into bytes(From, To), where From is an integer
%	  and To is either an integer or the atom =end=.
%	  * accept
%	  Parsed to a list of media descriptions.  Each media is a term
%	  media(Type, TypeParams, Quality, AcceptExts). The list is
%	  sorted according to preference.

http_parse_header_value(Field, Value, Prolog) :-
	valid_field(Field),
	to_codes(Value, Codes),
	field_to_prolog(Field, Codes, Prolog).

valid_field(content_length).
valid_field(cookie).
valid_field(set_cookie).
valid_field(host).
valid_field(range).
valid_field(accept).

to_codes(In, Codes) :-
	(   is_list(In)
	->  Codes = In
	;   atom_codes(In, Codes)
	).


field_to_prolog(content_length, ValueChars, ContentLength) :- !,
	number_codes(ContentLength, ValueChars).
field_to_prolog(cookie, ValueChars, Cookies) :- !,
	debug(cookie, 'Cookie: ~s', [ValueChars]),
	phrase(cookies(Cookies), ValueChars).
field_to_prolog(set_cookie, ValueChars, SetCookie) :- !,
	debug(cookie, 'SetCookie: ~s', [ValueChars]),
	phrase(set_cookie(SetCookie), ValueChars).
field_to_prolog(host, ValueChars, Host) :- !,
	(   append(HostChars, [0':|PortChars], ValueChars), % 0'
	    catch(number_codes(Port, PortChars), _, fail)
	->  atom_codes(HostName, HostChars),
	    Host = HostName:Port
	;   atom_codes(Host, ValueChars)
	).
field_to_prolog(range, ValueChars, Range) :-
	phrase(range(Range), ValueChars), !.
field_to_prolog(accept, ValueChars, Media) :-
	parse_accept(ValueChars, Media), !.
field_to_prolog(_, ValueChars, Atom) :-
	atom_codes(Atom, ValueChars).

field_value(set_cookie(Name, Value, Options)) --> !,
	atom(Name), "=", atom(Value),
	set_cookie_options(Options).
field_value(Atomic) -->
	atom(Atomic).

set_cookie_options([]) -->
	[].
set_cookie_options([secure=true|T]) --> !,
	" ; secure",
	set_cookie_options(T).
set_cookie_options([Name=Value|T]) -->
	" ; ", field_name(Name), "=",
	atom(Value),
	set_cookie_options(T).


		 /*******************************
		 *	  ACCEPT HEADERS	*
		 *******************************/

:- dynamic accept_cache/2.
:- volatile accept_cache/2.

parse_accept(Codes, Media) :-
	atom_codes(Atom, Codes),
	(   accept_cache(Atom, Media0)
	->  Media = Media0
	;   phrase(accept(Media0), Codes),
	    keysort(Media0, Media1),
	    pairs_values(Media1, Media2),
	    assertz(accept_cache(Atom, Media2)),
	    Media = Media2
	).

%%	accept(-Media)// is semidet.
%
%	Parse an HTTP Accept: header

accept([H|T]) -->
	blanks,
	media_range(H),
	blanks,
	(   ","
	->  accept(T)
	;   {T=[]}
	).

media_range(s(SortQuality,Spec)-media(Type, TypeParams, Quality, AcceptExts)) -->
	media_type(Type),
	blanks,
	(   ";"
	->  blanks,
	    parameters_and_quality(TypeParams, Quality, AcceptExts)
	;   { TypeParams = [],
	      Quality = 1.0,
	      AcceptExts = []
	    }
	),
	{ SortQuality is float(-Quality),
	  rank_specialised(Type, TypeParams, Spec)
	}.


%%	rank_specialised(+Type, +TypeParam, -Key) is det.
%
%	Although the specification linked  above   is  unclear, it seems
%	that  more  specialised  types  must   be  preferred  over  less
%	specialized ones.
%
%	@tbd	Is there an official specification of this?

rank_specialised(Type/SubType, TypeParams, v(VT, VS, SortVP)) :-
	var_or_given(Type, VT),
	var_or_given(SubType, VS),
	length(TypeParams, VP),
	SortVP is -VP.

var_or_given(V, Val) :-
	(   var(V)
	->  Val = 0
	;   Val = -1
	).

media_type(Type/SubType) -->
	type(Type), "/", type(SubType).

type(_) -->
	"*", !.
type(Type) -->
	token(Type).

parameters_and_quality(Params, Quality, AcceptExts) -->
	token(Name),
	blanks, "=", blanks,
	(   { Name == q }
	->  float(Quality), blanks,
	    accept_extensions(AcceptExts),
	    { Params = [] }
	;   { Params = [Name=Value|T] },
	    parameter_value(Value),
	    blanks,
	    (	";"
	    ->	blanks,
		parameters_and_quality(T, Quality, AcceptExts)
	    ;	{ T = [],
		  Quality = 1.0,
		  AcceptExts = []
		}
	    )
	).

accept_extensions([H|T]) -->
	";", !,
	blanks, token(Name), blanks,
	(   "="
	->  blanks,
	    (	token(Value)
	    ->	[]
	    ;	quoted_string(Value)
	    ),
	    { H = (Name=Value) }
	;   { H = Name }
	),
	blanks,
	accept_extensions(T).
accept_extensions([]) -->
	[].

parameter_value(Value) -->
	(   token(Value)
	->  []
	;   quoted_string(Value)
	).


%%	token(-Name)// is semidet.
%
%	Process an HTTP header token from the input.

token(Name) -->
	token_char(C1),
	token_chars(Cs),
	{ atom_codes(Name, [C1|Cs]) }.

token_chars([H|T]) -->
	token_char(H), !,
	token_chars(T).
token_chars([]) --> [].

token_char(C) -->
	[C],
	{   \+ ctl(C),
	    \+ separator_code(C)
	}, !.

ctl(C) :- between(0,31,C), !.
ctl(127).

separator_code(0'().
separator_code(0')).
separator_code(0'<).
separator_code(0'>).
separator_code(0'@).
separator_code(0',).
separator_code(0';).
separator_code(0':).
separator_code(0'\\).
separator_code(0'").
separator_code(0'/).
separator_code(0'[).
separator_code(0']).
separator_code(0'?).
separator_code(0'=).
separator_code(0'{).
separator_code(0'}).
separator_code(32).
separator_code(0'\t).


%%	quoted_string(-Text)// is semidet.
%
%	True if input starts with a quoted string representing Text.

quoted_string(Text) -->
	"\"",
	quoted_text(Codes),
	{ atom_codes(Text, Codes) }.

quoted_text([]) -->
	"\"", !.
quoted_text([H|T]) -->
	"\\", !, [H],
	quoted_text(T).
quoted_text([H|T]) -->
	[H], !,
	quoted_text(T).


%%	header_fields(+Fields, ?ContentLength)// is det.
%
%	Process a sequence of  [Name(Value),   ...]  attributes  for the
%	header. A term content_length(Len) is   special. If instantiated
%	it emits the header. If not   it just unifies ContentLength with
%	the argument of the content_length(Len)   term.  This allows for
%	both sending and retrieving the content-length.

header_fields([], _) -->
	[].
header_fields([content_length(CLen)|T], CLen) --> !,
	(   { var(CLen) }
	->  ""
	;   header_field(content_length, CLen)
	),
	header_fields(T, CLen).		% Continue or return first only?
header_fields([H|T], CLen) -->
	{ H =.. [Name, Value] },
	header_field(Name, Value),
	header_fields(T, CLen).


%%	field_name(?PrologName)
%
%	Convert between prolog_name  and  HttpName.   Field  names  are,
%	according to RFC 2616, considered  tokens   and  covered  by the
%	following definition:
%
%	==
%	token          = 1*<any CHAR except CTLs or separators>
%       separators     = "(" | ")" | "<" | ">" | "@"
%                      | "," | ";" | ":" | "\" | <">
%                      | "/" | "[" | "]" | "?" | "="
%                      | "{" | "}" | SP | HT
%	==

field_name(Name) -->
	{ var(Name) }, !,
	rd_field_chars(Chars),
	{ atom_codes(Name, Chars) }.
field_name(mime_version) --> !,
	"MIME-Version".
field_name(Name) -->
	{ atom_codes(Name, Chars) },
	wr_field_chars(Chars).

rd_field_chars_no_fold([C|T]) -->
	[C],
	{ rd_field_char(C, _) }, !,
	rd_field_chars_no_fold(T).
rd_field_chars_no_fold([]) -->
	[].

rd_field_chars([C0|T]) -->
	[C],
	{ rd_field_char(C, C0) }, !,
	rd_field_chars(T).
rd_field_chars([]) -->
	[].

%%	separators(-CharCodes) is det.
%
%	CharCodes is a list of separators according to RFC2616

separators("()<>@,;:\\\"/[]?={} \t").

term_expansion(rd_field_char('expand me',_), Clauses) :-

	Clauses = [ rd_field_char(0'-, 0'_)
		  | Cls
		  ],
	separators(SepString),
	string_codes(SepString, Seps),
	findall(rd_field_char(In, Out),
		(   between(32, 127, In),
		    \+ memberchk(In, Seps),
		    In \== 0'-,		% 0'
		    code_type(Out, to_lower(In))),
		Cls).

rd_field_char('expand me', _).			% avoid recursion

wr_field_chars([C|T]) --> !,
	{ code_type(C, to_lower(U)) },
	[U],
	wr_field_chars2(T).
wr_field_chars([]) -->
	[].

wr_field_chars2([]) --> [].
wr_field_chars2([C|T]) -->		% 0'
	(   { C == 0'_ }
	->  "-",
	    wr_field_chars(T)
	;   [C],
	    wr_field_chars2(T)
	).

%%	now//
%
%	Current time using rfc_date//1.

now -->
	{ get_time(Time)
	},
	rfc_date(Time).

%%	rfc_date(+Time)// is det.
%
%	Write time according to RFC1123 specification as required by the
%	RFC2616 HTTP protocol specs.

rfc_date(Time, String, Tail) :-
	stamp_date_time(Time, Date, 'UTC'),
	format_time(codes(String, Tail),
		    '%a, %d %b %Y %T GMT',
		    Date, posix).

%%	http_timestamp(+Time:timestamp, -Text:atom) is det.
%
%	Generate a description of a Time in HTTP format (RFC1123)

http_timestamp(Time, Atom) :-
	stamp_date_time(Time, Date, 'UTC'),
	format_time(atom(Atom),
		    '%a, %d %b %Y %T GMT',
		    Date, posix).


		 /*******************************
		 *	   REQUEST DCG		*
		 *******************************/

request(Fd, [method(Method),request_uri(ReqURI)|Header]) -->
	method(Method),
	blanks,
	nonblanks(Query),
	{ atom_codes(ReqURI, Query),
	  request_uri_parts(ReqURI, Header, Rest)
	},
	request_header(Fd, Rest), !.
request(Fd, [unknown(What)|Header]) -->
	string(What),
	eos, !,
	{   http_read_header(Fd, Header)
        ->  true
	;   Header = []
	}.

method(get)     --> "GET", !.
method(put)     --> "PUT", !.
method(head)    --> "HEAD", !.
method(post)    --> "POST", !.
method(delete)  --> "DELETE", !.
method(options) --> "OPTIONS", !.
method(trace)   --> "TRACE", !.

%%	request_uri_parts(+RequestURI, -Parts, ?Tail) is det.
%
%	Process the request-uri, producing the following parts:
%
%	  * path(-Path)
%	  Decode path information (always present)
%	  * search(-QueryParams)
%	  Present if there is a ?name=value&... part of the request uri.
%	  QueryParams is a Name=Value list.
%	  * fragment(-Fragment)
%	  Present if there is a #Fragment.

request_uri_parts(ReqURI, [path(Path)|Parts], Rest) :-
	uri_components(ReqURI, Components),
	uri_data(path, Components, PathText),
	uri_encoded(path, Path, PathText),
	phrase(uri_parts(Components), Parts, Rest).

uri_parts(Components) -->
	uri_search(Components),
	uri_fragment(Components).

uri_search(Components) -->
	{ uri_data(search, Components, Search),
	  nonvar(Search), !,
	  uri_query_components(Search, Query)
	},
	[ search(Query) ].
uri_search(_) --> [].

uri_fragment(Components) -->
	{ uri_data(fragment, Components, String),
	  nonvar(String), !,
	  uri_encoded(fragment, Fragment, String)
	},
	[ fragment(Fragment) ].
uri_fragment(_) --> [].

%%	request_header(+In:stream, -Header:list) is det.
%
%	Read the remainder (after the request-uri)   of  the HTTP header
%	and return it as a Name(Value) list.

request_header(_, []) -->		% Old-style non-version header
	blanks,
	eos, !.
request_header(Fd, [http_version(Version)|Header]) -->
	http_version(Version),
	blanks,
	eos, !,
	{   Version = 1-_
	->  http_read_header(Fd, Header)
	;   Header = []
	}.

http_version(Version) -->
	blanks,
	"HTTP/",
	http_version_number(Version).

http_version_number(Major-Minor) -->
	integer(Major),
	".",
	integer(Minor).


		 /*******************************
		 *	      COOKIES		*
		 *******************************/

%%	cookies(-List)// is semidet.
%
%	Translate a cookie description into a list Name=Value.

cookies([Name=Value|T]) -->
	blanks,
	cookie(Name, Value), !,
	blanks,
	(   ";"
	->  cookies(T)
	;   { T = [] }
	).
cookies(List) -->
	string(Skipped),
	";", !,
	{ print_message(warning, http(skipped_cookie(Skipped))) },
	cookies(List).
cookies([]) -->
	blanks.

cookie(Name, Value) -->
	cookie_name(Name),
	blanks, "=", blanks,
	cookie_value(Value).

cookie_name(Name) -->
	{ var(Name) }, !,
	rd_field_chars_no_fold(Chars),
	{ atom_codes(Name, Chars) }.

cookie_value(Value) -->
	quoted_string(Value), !.
cookie_value(Value) -->
	chars_to_semicolon_or_blank(Chars),
	{ atom_codes(Value, Chars)
	}.

chars_to_semicolon_or_blank([H|T]) -->
	[H],
	{ H \== 32, H \== 0'; }, !,
	chars_to_semicolon_or_blank(T).
chars_to_semicolon_or_blank([]) -->
	[].

set_cookie(set_cookie(Name, Value, Options)) -->
	ws,
	cookie(Name, Value),
	cookie_options(Options).

cookie_options([H|T]) -->
	ws,
	";",
	ws,
	cookie_option(H), !,
	cookie_options(T).
cookie_options([]) -->
	ws.

ws --> " ", !, ws.
ws --> [].


%%	cookie_option(-Option)// is semidet.
%
%	True if input represents a valid  Cookie option. Officially, all
%	cookie  options  use  the  syntax   <name>=<value>,  except  for
%	=secure=.  M$  decided  to  extend  this  to  include  at  least
%	=httponly= (only the Gods know what it means).
%
%	@param	Option	Term of the form Name=Value
%	@bug	Incorrectly accepts options without = for M$ compatibility.

cookie_option(Name=Value) -->
	rd_field_chars(NameChars), ws,
	{ atom_codes(Name, NameChars) },
	(   "="
	->  ws,
	    chars_to_semicolon(ValueChars),
	    { atom_codes(Value, ValueChars)
	    }
	;   { Value = true }
	).

chars_to_semicolon([H|T]) -->
	[H],
	{ H \== 32, H \== 0'; }, !,
	chars_to_semicolon(T).
chars_to_semicolon([]), ";" -->
	ws, ";", !.
chars_to_semicolon([H|T]) -->
	[H],
	chars_to_semicolon(T).
chars_to_semicolon([]) -->
	[].

%%	range(-Range)// is semidet.
%
%	Process the range header value. Range is currently defined as:
%
%	    * bytes(From, To)
%	    Where From is an integer and To is either an integer or
%	    the atom =end=.

range(bytes(From, To)) -->
	"bytes", whites, "=", whites, integer(From), "-",
	(   integer(To)
	->  ""
	;   { To = end }
	).


		 /*******************************
		 *	     REPLY DCG		*
		 *******************************/

%%	reply(+In, -Reply:list)// is semidet.
%
%	Process the first line of an HTTP   reply.  After that, read the
%	remainder  of  the  header  and    parse  it.  After  successful
%	completion, Reply contains the following fields, followed by the
%	fields produced by http_read_header/2.
%
%	    * http_version(Major-Minor)
%	    * status(StatusCode, Comment)
%
%	StatusCode is one of the values provided by status_number//2.

reply(Fd, [http_version(HttpVersion), status(Status, Comment)|Header]) -->
	http_version(HttpVersion),
	blanks,
	(   status_number(Status, _Code)
	->  []
	;   integer(Status)
	),
	blanks,
	string(CommentCodes),
	blanks_to_nl, !,
	blanks,
	{ atom_codes(Comment, CommentCodes),
	  http_read_header(Fd, Header)
	}.


		 /*******************************
		 *	      READ HEADER	*
		 *******************************/

%%	http_read_header(+Fd, -Header) is det.
%
%	Read Name: Value lines from FD until an empty line is encountered.
%	Field-name are converted to Prolog conventions (all lower, _ instead
%	of -): Content-Type: text/html --> content_type(text/html)

http_read_header(Fd, Header) :-
	read_header_data(Fd, Text),
	http_parse_header(Text, Header).

read_header_data(Fd, Header) :-
	read_line_to_codes(Fd, Header, Tail),
	read_header_data(Header, Fd, Tail),
	debug(http(header), 'Header = ~n~s~n', [Header]).

read_header_data([0'\r,0'\n], _, _) :- !.
read_header_data([0'\n], _, _) :- !.
read_header_data([], _, _) :- !.
read_header_data(_, Fd, Tail) :-
	read_line_to_codes(Fd, Tail, NewTail),
	read_header_data(Tail, Fd, NewTail).

%%	http_parse_header(+Text:codes, -Header:list) is det.
%
%	Header is a list of Name(Value)-terms representing the structure
%	of the HTTP header in Text.
%
%	@error domain_error(http_request_line, Line)

http_parse_header(Text, Header) :-
	phrase(header(Header), Text),
	debug(http(header), 'Fields: ~w~n', [Header]).

header(List) -->
	header_field(Name, Value), !,
	{ mkfield(Name, Value, List, Tail)
	},
	blanks,
	header(Tail).
header([]) -->
	blanks,
	eos, !.
header(_) -->
	string(S), blanks_to_nl, !,
	{ atom_codes(Line, S),
	  syntax_error(http_request_line(Line))
	}.

%%	address//
%
%	Emit the HTML for the server address on behalve of error and
%	status messages (non-200 replies).  Default is
%
%	    ==
%	    SWI-Prolog httpd at <hostname>
%	    ==
%
%	The address can be modified by   providing  a definition for the
%	multifile predicate http:http_address//0.

:- multifile
	http:http_address//0.

address -->
	http:http_address, !.
address -->
	{ gethostname(Host) },
	html(address([ a(href('http://www.swi-prolog.org'), 'SWI-Prolog'),
		       ' httpd at ', Host
		     ])).

mkfield(host, Host:Port, [host(Host),port(Port)|Tail], Tail) :- !.
mkfield(Name, Value, [Att|Tail], Tail) :-
	Att =.. [Name, Value].

%%	http:http_address// is det.
%
%	HTML-rule that emits the location of  the HTTP server. This hook
%	is called from address//0 to customise   the server address. The
%	server address is emitted on non-200-ok replies.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(error(http_write_short(Data, Sent), _)) -->
	[ '~p: remote hangup after ~D bytes'-[Data, Sent] ].
prolog:message(http(skipped_cookie(S))) -->
	[ 'Skipped illegal cookie: ~s'-[S] ].
