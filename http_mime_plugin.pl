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

:- module(http_mime_plugin, []).
:- use_module(http_client).
:- use_module(library(memfile)).
:- use_module(library(mime)).
:- use_module(library(option)).

:- initialization
	mime_default_charset(_, 'UTF-8').

/** <module> MIME client plugin

This plugin for library(http_client)   automatically translates messages
with content-type =|multipart/form-data|= into a list   of  Name = Value
pairs, greatly simplifying the processing of   forms  with this type. It
relies  on  library(mime),  which  in   turn    relies   on   a  foreign
implementation of the rfc2045 (mime) specifications.

This library uses mime_default_charset/2 to   set  the default character
set of the MIME library to =|UTF-8|=.   Note that this setting is global
and not thread-safe. This implies  that   this  library cannot be safely
used together with other code that manipulates the default MIME charset.
*/

:- multifile
	http_client:http_convert_data/4,
	http_parameters:form_data_content_type/1.

%%	http_client:http_convert_data(+In, +Fields, -Data, +Options) is semidet.
%
%	Convert =|multipart/form-data|= messages for http_read_data/3.
%	Options:
%
%	  * form_data(AsForm)
%	  If the content-type is =|multipart/form-data|=, return the
%	  form-data either as a list of Name=Value (AsForm = =form=;
%	  default) or as a part-list as defined by mime_parse/2.

http_client:http_convert_data(In, Fields, Data, Options) :-
	memberchk(content_type(Type), Fields),
	(   memberchk(mime_version(MimeVersion), Fields)
	;   sub_atom(Type, 0, _, _, 'multipart/form-data'),
	    MimeVersion = '1.0'
	), !,
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    convert_mime_data(In, Fields, Data,
			      MemFile, Type, MimeVersion, Options),
	    free_memory_file(MemFile)).

convert_mime_data(In, Fields, Data, MemFile, Type, MimeVersion, Options) :-
	option(form_data(AsForm), Options, form),
	setup_call_cleanup(
	    open_memory_file(MemFile, write, Tmp),
	    ( format(Tmp, 'Mime-Version: ~w\r\n', [MimeVersion]),
	      format(Tmp, 'Content-Type: ~w\r\n\r\n', [Type]),
	      http_read_data([input(In)|Fields], _,
			     [ to(stream(Tmp))
			     | Options
			     ])
	    ),
	    close(Tmp)),
	setup_call_cleanup(
	    open_memory_file(MemFile, read, MimeIn),
	    mime_parse(stream(MimeIn), Data0),
	    close(MimeIn)),
	mime_to_form(Data0, AsForm, Data).

mime_to_form(mime(A,'',Parts), AsForm, Form) :-
	memberchk(type('multipart/form-data'), A), !,
	(   AsForm == mime
	->  Form = Parts
	;   mime_form_fields(Parts, Form0)
	->  Form = Form0
	;   Form = Parts
	).
mime_to_form(Mime, _, Mime).

mime_form_fields([], []).
mime_form_fields([mime(A, V, [])|T0], [Name=V|T]) :-
	memberchk(name(Name), A),
	mime_form_fields(T0, T).

http_parameters:form_data_content_type(ContentType) :-
	sub_atom(ContentType, 0, _, _, 'multipart/form-data').
