/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, University of Amsterdam
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

:- module(http_multipart_plugin,
	  [
	  ]).
:- use_module(http_stream).
:- use_module(http_header).
:- use_module(library(debug)).
:- use_module(library(option)).

/** <module> Multipart form-data plugin

This plugin for library(http_client)   automatically translates messages
with content-type =|multipart/form-data|= into a list   of  Name = Value
pairs, greatly simplifying the processing of   forms  with this type.

After loading this plugin, multipart form-data   can be accessed through
http_parameters/3 from library(http/http_parameters) or http_read_data/3
from library(http/http_client).
*/

:- multifile
	http_client:http_convert_data/4,
	http_parameters:form_data_content_type/1.

%%	http_client:http_convert_data(+In, +Fields, -Data, +Options) is semidet.
%
%	Convert =|multipart/form-data|= messages for http_read_data/3.
%	This plugin adds the folling options to http_read_data/3:
%
%	  * form_data(+AsForm)
%	  If the content-type is =|multipart/form-data|=, return the
%	  form-data either in one of the following formats:
%
%	    - AsForm = form
%	    A list of Name=Value, where Value is an atom.
%	    - AsForm = mime
%	    A list of mime(Properties, Value, []).  This is a backward
%	    compatibility mode, emulating library(http/http_mime_plugin).
%	    Note that if the disposition contains a =filename=
%	    property, the data is read as binary unless there is a
%	    charset parameter in the Content-Type stating otherwise,
%	    while the old library would use UTF-8 for text files.
%
%	  * input_encoding(+Encoding)
%	  Encoding to be used for parts that have no =filename=
%	  disposition and no Content-Type with a charset indication.
%	  This is typically the case for input widgets and browsers
%	  encode this using the encoding of the page. As the SWI-Prolog
%	  http library emits pages in UTF-8, the default is =utf8=.
%
%	  * on_filename(:CallBack)
%	  If a part with a =filename= disposition is found and this
%	  option is given, call CallBack as below.  `Stream` is the
%	  multipart input stream, which has octet (raw) encoding.
%	  `Value` is returned as result.  Note that the callback
%	  may wish to save the result into a file and return e.g.,
%	  file(Path) to indicate where the file was saved.
%
%	      call(:CallBack, +Stream, -Value, +Options).
%
%	  The Options list contains information from the part header.
%	  It always contains name(Name) and filename(FileName).  It
%	  may contain a term media(Type/SubType, Params) if the part
%	  contains a Content-Type header.

http_client:http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	multipart_type(Type, Boundary), !,
	setup_call_cleanup(
	    multipart_open(In, Stream, [boundary(Boundary)]),
	    process_parts(Stream, Data, Options),
	    close(Stream)).

%%	multipart_type(+Type, -Boundary) is semidet.
%
%	True   if   Type   is   of   the   form   =|multipart/form-data;
%	boundary="..."|=  and  Boundary  is  a   string  describing  the
%	boundary.

multipart_type(Type, Boundary) :-
	http_parse_header_value(content_type, Type,
				media(multipart/'form-data', Params)),
	memberchk(boundary=Boundary, Params).


process_parts(Stream, [Part|More], Options) :-
	http_read_header(Stream, HTTPHeader),
	part_header(HTTPHeader, Params, Name, Encoding),
	part_value(Stream, Name, Params, Encoding, Part, Options),
	debug(multipart(content), 'Got ~q~n', [Part]),
	(   multipart_open_next(Stream)
	->  process_parts(Stream, More, Options)
	;   More = []
	).

set_encoding(text, Stream, _) :- !,
	(   set_stream(Stream, encoding(bom))
	->  stream_property(Stream, encoding(Enc)),
	    debug(multipart(bom), 'BOM: ~q', [Enc])
	;   set_stream(Stream, encoding(iso_latin_1)) % RFC2616, sec. 3.7.1
	).
set_encoding(input, Stream, Options) :- !,
	option(input_encoding(Enc), Options, utf8),
	set_stream(Stream, encoding(Enc)).
set_encoding(Enc, Stream, _) :-
	set_stream(Stream, encoding(Enc)).


%%	part_header(+PartHeader, -Params, -Name, -Encoding) is det.
%
%	Extract the form-field Name, the   content Encoding and possible
%	other properties of the form-field.  Extra properties are:
%
%	  - filename(Name)
%	  - media(Type/SubType, MediaParams)

part_header(PartHeader, Extra, Name, Encoding) :-
	memberchk(content_disposition(disposition('form-data', DProps)),
		  PartHeader),
	memberchk(name=Name, DProps),
	(   filename(DProps, Extra, Extra1)
	->  part_encoding(PartHeader, Extra1, Encoding)
	;   Encoding = input,
	    Extra = []
	).

filename(DProps, Extra, Tail) :-
	memberchk(filename=FileName, DProps), !,
	Extra = [filename(FileName)|Tail].

part_encoding(PartHeader, Extra, Encoding) :-
	memberchk(content_type(TypeA), PartHeader),
	http_parse_header_value(content_type, TypeA, MediaType), !,
	Extra = [MediaType],
	media_type_encoding(MediaType, Encoding).

media_type_encoding(media(_Type, Params), Encoding) :-
	memberchk(charset=CharSet, Params),
	charset_encoding(CharSet, Encoding).
media_type_encoding(media(Type/SubType, _Params), Encoding) :-
	media_encoding(Type, SubType, Encoding).

charset_encoding(CharSet, utf8) :-
	sub_atom_icasechk(CharSet, _, 'utf-8'), !.
charset_encoding(_, octet).

media_encoding(text, _, text) :- !.
media_encoding(_,    _, octet).


%%	part_value(+Stream, +Name, +Params, +Encoding, -Part, +Options)

part_value(Stream, Name, Params, Encoding, Part, Options) :-
	option(form_data(mime), Options), !,
	set_encoding(Encoding, Stream, Options),
	Part = mime([disposition('form-data'),name(Name)|Properties], Atom, []),
	mime_properties(Params, Properties),
	read_string(Stream, _, String),
	atom_string(Atom, String).
part_value(Stream, Name, Params, _, Name=Value, Options) :-
	memberchk(filename(_), Params),
	option(on_filename(Goal), Options), !,
	call(Goal, Stream, Value, [name(Name)|Params]).
part_value(Stream, Name, _, Encoding, Name=Value, Options) :-
	set_encoding(Encoding, Stream, Options),
	read_string(Stream, _, String),
	atom_string(Value, String).

mime_properties([], []).
mime_properties([media(Type/SubType, Params)|T0],
		[type(ContentType)|T]) :- !,
	atomic_list_concat([Type, SubType], /, ContentType),
	(   memberchk(charset(CharSet), Params)
	->  T = [character_set(CharSet)|T1]
	;   T = T1
	),
	mime_properties(T0, T1).
mime_properties([H|T0], [H|T]) :-
	mime_properties(T0, T).


http_parameters:form_data_content_type(ContentType) :-
	sub_atom(ContentType, 0, _, _, 'multipart/form-data').
