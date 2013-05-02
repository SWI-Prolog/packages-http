/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(http_ax,
	  [ http_ax_attributes/2,	% +Spec, -AttributeList
	    ax_form_attributes/2	% +Form, -Values
	  ]).
:- use_module(library(error)).


/** <module> Attribute Exchange library

This library can be used to create   HTTP request parameters and analyse
form-data for _attribute exchange_. Attribute exchange   (AX) is used by
OpenID and OAuth to fetch attributes  for   accounts,  such  as the real
username or e-mail address.
*/

%%	http_ax_attributes(+Spec, -HTTPAttributes) is det.
%
%	True when HTTPAttributes is a list  of Name=Value pairs that can
%	be used with an HTTP request to   query for the attributes Spec.
%	Spec is a list of elements =|Alias(Value[, Options])|=.  Options
%	include:
%
%	  - required
%	  The attribute is required.  This is mutually exclusive
%	  with =if_available=.
%	  - if_available
%	  Only provide the attribute if it is available. This is
%	  mutually exclusive with =required=.  This is the default.
%	  - url(+URL)
%	  Can be used to ovcerrule or extend the ax_alias/2.
%	  - count(+Count)
%	  Maximum number of values to provide
%
%	For example:
%
%	    ==
%	    ?- http_ax_attributes([ nickname(Nick),
%				    email(Email, [required])
%			          ], Params).
%	    Params = [ 'openid.ax.mode'          = fetch_request,
%		       'openid.ax.type.nickname' = 'http://axschema.org/namePerson/friendly',
%		       'openid.ax.type.email'    = 'http://axschema.org/contact/email',
%		       'openid.ax.required'      = email,
%		       'openid.ax.if_available'  = nickname
%		     ].
%	    ==

http_ax_attributes(Spec, [ 'openid.ns.ax'   = 'http://openid.net/srv/ax/1.0',
			   'openid.ax.mode' = fetch_request
			 | AllAttr
			 ]) :-
	maplist(type_alias, Spec, AliasAttrs),
	partition(required, Spec, Required, Optional),
	alias_list(Required, 'openid.ax.required', RequiredAttr),
	alias_list(Optional, 'openid.ax.if_available', IfAvailableAttr),
	count_attr(Spec, CountAttr),
	append([AliasAttrs, RequiredAttr, IfAvailableAttr, CountAttr], AllAttr).

type_alias(Spec, Attr=URL) :-
	functor(Spec, Alias, Arity),
	(   Arity > 1,
	    arg(2, Spec, Options),
	    memberchk(url(URL), Options)
	->  true
	;   ax_alias(Alias, URL)
	->  true
	;   existence_error(ax_alias, Alias)
	),
	atom_concat('openid.ax.type.', Alias, Attr).

required(Spec) :-
	functor(Spec, _, 2),
	arg(2, Spec, Options),
	memberchk(required, Options).

alias_list([], _, []).
alias_list(Specs, A, [A=V]) :-
	maplist(alias_name, Specs, Aliases),
	atomic_list_concat(Aliases, ',', V).

alias_name(Spec, Alias) :-
	functor(Spec, Alias, _).

count_attr([], []).
count_attr([Spec|T0], [A=Count|T]) :-
	functor(Spec, Alias, 2),
	arg(2, Spec, Options),
	memberchk(count(Count), Options), !,
	atomic_list_concat('openid.ax.count.', Alias, A),
	count_attr(T0, T).
count_attr([_|T0], T) :-
	count_attr(T0, T).


%%	ax_alias(?Alias, ?URL) is nondet.
%
%	True when Alias is an alias  name   for  the AX schema URL. This
%	predicate is defined as _multifile_.
%
%	Note  that  Google  federated  login    only  supports  =email=,
%	=country=, =language=, =firstname= and =lastname=.

:- multifile
	ax_alias/2.

ax_alias(nickname,  'http://axschema.org/namePerson/friendly').
ax_alias(email,	    'http://axschema.org/contact/email').
ax_alias(fullname,  'http://axschema.org/namePerson').
ax_alias(dob,	    'http://axschema.org/birthDate').
ax_alias(gender,    'http://axschema.org/person/gender').
ax_alias(postcode,  'http://axschema.org/contact/postalCode/home').
ax_alias(country,   'http://axschema.org/contact/country/home').
ax_alias(language,  'http://axschema.org/pref/language').
ax_alias(timezone,  'http://axschema.org/pref/timezone').
ax_alias(prefix,    'http://axschema.org/namePerson/prefix').
ax_alias(firstname, 'http://axschema.org/namePerson/first').
ax_alias(lastname,  'http://axschema.org/namePerson/last').
ax_alias(suffix,    'http://axschema.org/namePerson/suffix').


		 /*******************************
		 *	      RESPONSE		*
		 *******************************/

%%	ax_form_attributes(+Form, -Values) is det.
%
%	True if Values  is  a  list   Alias(Value)  for  each  exchanged
%	attribute.
%
%	Note that we assume we get the same   alias names as we used for
%	requesting the data. Not sure whether this is true.
%
%	@arg	Form is an HTTP form as returned using the form(Form)
%		option of http_parameters/3.

ax_form_attributes(Form, Values) :-
	(   memberchk('openid.ax.mode'=fetch_response, Form)
	->  Ext = ax
	;   memberchk(ExtNS='http://openid.net/srv/ax/1.0', Form),
	    atomic_list_concat([openid,ns,Ext], '.', ExtNS)
	->  true
	),
	ax_attributes(Form, Ext, Values).
ax_form_attributes(_, []).

ax_attributes([], _, []).
ax_attributes([Name=Value|T0], Ext, AXs) :-
	atomic_list_concat([openid, Ext, value, Alias|_Num], '.', Name), !,
	AX =.. [Alias,Value],
	AXs = [AX|AXT],
	ax_attributes(T0, Ext, AXT).
ax_attributes([_|T0], Ext, AXs) :-
	ax_attributes(T0, Ext, AXs).
