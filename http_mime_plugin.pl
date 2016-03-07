/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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

@deprecated  POST  messages  of  type  =|multipart/form-data|=  are  now
handled by library(http/http_multipart_plugin). The new  library is much
more lightweight, allows processing large   files  without buffering the
data and does not include GPL foreign components.
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
