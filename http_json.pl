/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
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

:- if(current_predicate(is_dict/1)).
:- module(http_json,
	  [ reply_json/1,		% +JSON
	    reply_json/2,		% +JSON, Options
	    http_read_json/2,		% +Request, -JSON
	    http_read_json/3,		% +Request, -JSON, +Options
	    http_read_json_dict/2,	% +Request, -Dict
	    http_read_json_dict/3	% +Request, -Dict, +Options
	  ]).
:- else.
:- module(http_json,
	  [ reply_json/1,		% +JSON
	    reply_json/2,		% +JSON, Options
	    http_read_json/2,		% +Request, -JSON
	    http_read_json/3		% +Request, -JSON, +Options
	  ]).
:- endif.
:- use_module(http_client).
:- use_module(http_header).
:- use_module(http_stream).
:- use_module(json).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(memfile)).

:- multifile
	http_client:http_convert_data/4,
	http_client:post_data_hook/3,
	json_type/1.

:- predicate_options(http_read_json/3, 3,
		     [ content_type(any),
		       false(ground),
		       null(ground),
		       true(ground),
		       value_string_as(oneof([atom, string])),
		       json_object(oneof([term,dict]))
		     ]).
:- predicate_options(reply_json/2, 2,
		     [ content_type(any),
		       status(integer),
		       json_object(oneof([term,dict])),
		       pass_to(json:json_write/3, 3)
		     ]).


/** <module> HTTP JSON Plugin module

This  module  inserts  the  JSON  parser  for  documents  of  MIME  type
=|application/jsonrequest|= and =|application/json|=   requested through
the http_client.pl library.

Typically JSON is used by Prolog HTTP  servers. This module supports two
JSON  representations:  the  classical  representation    and   the  new
representation supported by  the  SWI-Prolog   version  7  extended data
types. Below is a skeleton for  handling   a  JSON request, answering in
JSON using the classical interface.

  ==
  handle(Request) :-
	http_read_json(Request, JSONIn),
	json_to_prolog(JSONIn, PrologIn),
	<compute>(PrologIn, PrologOut),		% application body
	prolog_to_json(PrologOut, JSONOut),
	reply_json(JSONOut).
  ==

When using dicts, the conversion step is   generally  not needed and the
code becomes:

  ==
  handle(Request) :-
	http_read_json_dict(Request, DictIn),
	<compute>(DictIn, DictOut),
	reply_json(DictOut).
  ==

This module also integrates JSON support   into the http client provided
by http_client.pl. Posting a JSON query   and  processing the JSON reply
(or any other reply understood  by   http_read_data/3)  is  as simple as
below, where Term is a JSON term as described in json.pl and reply is of
the same format if the server replies with JSON.

  ==
	...,
	http_post(URL, json(Term), Reply, [])
  ==

@see	JSON Requests are discussed in http://json.org/JSONRequest.html
@see	json.pl describes how JSON objects are represented in Prolog terms.
@see	json_convert.pl converts between more natural Prolog terms and json
terms.
*/

%%	http_client:http_convert_data(+In, +Fields, -Data, +Options)
%
%	Hook implementation that supports  reading   JSON  documents. It
%	processes the following option:
%
%	  * json_object(+As)
%	  Where As is one of =term= or =dict=.  If the value is =dict=,
%	  json_read_dict/3 is used.

http_client:http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	is_json_type(Type), !,
	(   memberchk(content_length(Bytes), Fields)
	->  setup_call_cleanup(
		( stream_range_open(In, Range, [size(Bytes)]),
		  set_stream(Range, encoding(utf8))
		),
		json_read_to(Range, Data, Options),
		close(Range))
	;   set_stream(In, encoding(utf8)),
	    json_read_to(In, Data, Options)
	).


is_json_type(Type) :-
	json_type(Type), !.
is_json_type(ContentType) :-
	json_type(Type),
	sub_atom(ContentType, 0, _, _, Type), !,
	strip_utf8(ContentType, Plain),
	json_type(Plain).

:- if(current_predicate(is_dict/1)).
json_read_to(In, Data, Options) :-
	memberchk(json_object(dict), Options), !,
	json_read_dict(In, Data, Options).
:- endif.
json_read_to(In, Data, Options) :-
	json_read(In, Data, Options).

%%	strip_utf8(+ContentTypeIn, -ContentType) is det.
%
%	Strip an optional  =|;  charset=UTF-8|=.   JSON  data  is always
%	UTF-8, but some clients seem to insist in sending this.

strip_utf8(ContentType, Plain) :-
	sub_atom(ContentType, B, _, A, ;),
	sub_atom(ContentType, _, A, 0, Ext),
	normalize_space(atom(Charset), Ext),
	downcase_atom(Charset, 'charset=utf-8'), !,
	sub_atom(ContentType, 0, B, _, CT),
	normalize_space(atom(Plain), CT).
strip_utf8(ContentType, ContentType).


%%	json_type(?MIMEType:atom) is semidet.
%
%	True if MIMEType is a JSON  mimetype. http_json:json_type/1 is a
%	multifile  predicate  and  may   be    extended   to  facilitate
%	non-conforming clients.

json_type('application/jsonrequest').
json_type('application/json').


%%	http_client:post_data_hook(+Data, +Out:stream, +HdrExtra) is semidet.
%
%	Hook implementation that allows   http_post_data/3  posting JSON
%	objects using one of the  forms   below.
%
%	  ==
%	  http_post(URL, json(Term), Reply, Options)
%	  http_post(URL, json(Term, Options), Reply, Options)
%	  ==
%
%	If Options are passed, these are handed to json_write/3. In
%	addition, this option is processed:
%
%	  * json_object(As)
%	  If As is =dict=, json_write_dict/3 is used to write the
%	  output.  This is default if json(Dict) is passed.
%
%	@tbd avoid creation of intermediate data using chunked output.

:- if(current_predicate(is_dict/1)).
http_client:post_data_hook(json(Dict), Out, HdrExtra) :-
	is_dict(Dict), !,
	http_client:post_data_hook(json(Dict, [json_object(dict)]),
				   Out, HdrExtra).
:- endif.
http_client:post_data_hook(json(Term), Out, HdrExtra) :-
	http_client:post_data_hook(json(Term, []), Out, HdrExtra).
http_client:post_data_hook(json(Term, Options), Out, HdrExtra) :-
	option(content_type(Type), HdrExtra, 'application/json'),
	setup_call_cleanup(
	    ( new_memory_file(MemFile),
	      open_memory_file(MemFile, write, Handle)
	    ),
	    ( format(Handle, 'Content-type: ~w~n~n', [Type]),
	      json_write_to(Handle, Term, Options)
	    ),
	    close(Handle)),
	setup_call_cleanup(
	    open_memory_file(MemFile, read, RdHandle,
			     [ free_on_close(true)
			     ]),
	    http_post_data(cgi_stream(RdHandle), Out, HdrExtra),
	    close(RdHandle)).

:- if(current_predicate(is_dict/1)).
json_write_to(Out, Term, Options) :-
	memberchk(json_object(dict), Options), !,
	json_write_dict(Out, Term, Options).
:- endif.
json_write_to(Out, Term, Options) :-
	json_write(Out, Term, Options).


%%	http_read_json(+Request, -JSON) is det.
%%	http_read_json(+Request, -JSON, +Options) is det.
%
%	Extract JSON data posted  to  this   HTTP  request.  Options are
%	passed to json_read/3.  In addition, this option is processed:
%
%	  * json_object(+As)
%	  One of =term= (default) to generate a classical Prolog
%	  term or =dict= to exploit the SWI-Prolog version 7 data type
%	  extensions.  See json_read_dict/3.
%
%	@error	domain_error(mimetype, Found) if the mimetype is
%		not known (see json_type/1).
%	@error	domain_error(method, Method) if the request is not
%		a =POST= or =PUT= request.

http_read_json(Request, JSON) :-
	http_read_json(Request, JSON, []).

http_read_json(Request, JSON, Options) :-
	select_option(content_type(Type), Options, Rest), !,
	delete(Request, content_type(_), Request2),
	request_to_json([content_type(Type)|Request2], JSON, Rest).
http_read_json(Request, JSON, Options) :-
	request_to_json(Request, JSON, Options).

request_to_json(Request, JSON, Options) :-
	option(method(Method), Request),
	option(content_type(Type), Request),
	(   data_method(Method)
	->  true
	;   domain_error(method, Method)
	),
	(   is_json_type(Type)
	->  true
	;   domain_error(mimetype, Type)
	),
	http_read_data(Request, JSON, Options).

data_method(post).
data_method(put).

:- if(current_predicate(is_dict/1)).

%%	http_read_json_dict(+Request, -Dict) is det.
%%	http_read_json_dict(+Request, -Dict, +Options) is det.
%
%	Similar to http_read_json/2,3, but by default uses the version 7
%	extended datatypes.

http_read_json_dict(Request, Dict) :-
	http_read_json_dict(Request, Dict, []).

http_read_json_dict(Request, Dict, Options) :-
	merge_options([json_object(dict)], Options, Options1),
	http_read_json(Request, Dict, Options1).

:- endif.

%%	reply_json(+JSONTerm) is det.
%%	reply_json(+JSONTerm, +Options) is det.
%
%	Formulate a JSON  HTTP  reply.   See  json_write/2  for details.
%	The processed options are listed below.  Remaining options are
%	forwarded to json_write/3.
%
%	    * content_type(+Type)
%	    The default =|Content-type|= is =|application/json;
%	    charset=UTF8|=. =|charset=UTF8|= should not be required
%	    because JSON is defined to be UTF-8 encoded, but some
%	    clients insist on it.
%
%	    * status(+Code)
%	    The default status is 200.  REST API functions may use
%	    other values from the 2XX range, such as 201 (created).
%
%	    * json_object(+As)
%	    One of =term= (classical json representation) or =dict=
%	    to use the new dict representation.	 If omitted and Term
%	    is a dict, =dict= is assumed.  SWI-Prolog Version 7.

:- if(current_predicate(is_dict/1)).
reply_json(Dict) :-
	is_dict(Dict), !,
	format('Content-type: application/json; charset=UTF-8~n~n'),
	json_write_dict(current_output, Dict).
:- endif.
reply_json(Term) :-
	format('Content-type: application/json; charset=UTF-8~n~n'),
	json_write(current_output, Term).

:- if(current_predicate(is_dict/1)).
reply_json(Dict, Options) :-
	is_dict(Dict), !,
	merge_options([json_object(dict)], Options, Options1),
	reply_json2(Dict, Options1).
:- endif.
reply_json(Term, Options) :-
	reply_json2(Term, Options).

reply_json2(Term, Options) :-
	select_option(content_type(Type), Options, Rest0, 'application/json'),
	(   select_option(status(Code), Rest0, Rest)
	->  format('Status: ~d~n', [Code])
	;   Rest = Rest0
	),
	format('Content-type: ~w~n~n', [Type]),
	json_write_to(current_output, Term, Rest).
