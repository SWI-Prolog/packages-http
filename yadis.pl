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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301 USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(yadis,
	  [ xrds_dom/2,			% +URI, -XRDS_DOM
	    xrds_location/2		% +Xid, -URL
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(sgml)).

/** <module> Yadis discovery

@see http://en.wikipedia.org/wiki/Yadis
*/

:- multifile
	xrds_specified_location/2.

%%	xrds_dom(+Id, -XRDS_DOM) is det.
%
%	True when XRDS_DOM is  a  parsed   XML  document  for  the given
%	resource.

xrds_dom(Xid, XRDS_DOM) :-
	xrds_location(Xid, XRDSLocation),
	xrds_load(XRDSLocation, XRDS_DOM).

%%	xid_normalize(+OpenID, -URL) is det.
%
%	Translate the user-specified  OpenID  agent   into  a  URL. This
%	follows appendix A.1. (Normalization), RFC3986).
%
%	@tbd This does not implement XRI identifiers.

xid_normalize(Xid, URL) :-
	add_component(scheme, Xid, URL0, http),
	add_component(path,   URL0, URL, /).

add_component(Field, URL0, URL, Default) :-
	uri_components(URL0, Comp),
	uri_data(Field, Comp, Value),
	(   var(Value)
	->  (   Field == scheme
	    ->	atomic_list_concat([Default, '://', URL0], URL)
	    ;	Value = Default,
		uri_components(URL, Comp)
	    )
	;   Field == path,
	    Value = ''
	->  uri_data(path, Comp, Default, Comp2),
	    uri_components(URL, Comp2)
	;   URL = URL0
	).


%%	xrds_location(+Id, -XRDSLocation) is semidet.
%
%	Discover the location of the XRDS document from the given Id.

xrds_location(Xid, XRDSLocation) :-
	xid_normalize(Xid, URL),
	(   xrds_specified_location(URL, XRDSLocation)
	->  XRDSLocation \== (-)
	;   catch(xrds_location_direct(URL, XRDSLocation),
		  E, yadis_failed(E))
	->  true
	;   catch(xrds_location_html(URL, XRDSLocation),
		  E, yadis_failed(E))
	).

yadis_failed(E) :-
	(   debugging(yadis)
	->  print_message(warning, E)
	;   true
	),
	fail.

xrds_location_direct(URL, XRDSLocation) :-
	setup_call_cleanup(
	    http_open(URL, In,
		      [ method(head),
			request_header(accept='application/xrds+xml'),
			header(x_xrds_location, Reply),
			cert_verify_hook(ssl_verify)
		      ]),
	    true,
	    close(In)),
	Reply \== '', !,
	XRDSLocation = Reply.

xrds_location_html(URL, XRDSLocation) :-
	setup_call_cleanup(
	    http_open(URL, In,
		      [ cert_verify_hook(ssl_verify)
		      ]),
	    html_head_dom(In, DOM),
	    close(In)),
	xpath(DOM, meta(@'http-equiv'=Equiv, @content), Content),
	downcase_atom(Equiv, 'x-xrds-location'), !,
	XRDSLocation = Content.

%%	xrds_load(+XRDSLocation, -XRDS_DOM) is det.
%
%	Parse the XRDS document at XRDSLocation.

xrds_load(XRDSLocation, XRDS_DOM) :-
	setup_call_cleanup(
	    http_open(XRDSLocation, In,
		      [ request_header(accept='application/xrds+xml'),
			cert_verify_hook(ssl_verify)
		      ]),
	    load_structure(In, XRDS_DOM,
			   [ dialect(xmlns),
			     space(remove)
			   ]),
	    close(In)).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Accept all certificates.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).


%%	html_head_dom(+Stream, -HeadDOM) is semidet.
%
%	Extract the HTML head content from   the  given stream. Does not
%	parse the remainder of the document.

:- thread_local
	html_head_dom/1.

html_head_dom(Stream, HeadDOM) :-
	dtd(html, DTD),
	new_sgml_parser(Parser, [dtd(DTD)]),
	call_cleanup(
	    sgml_parse(Parser,
		       [ source(Stream),
			 syntax_errors(quiet),
			 call(begin, on_begin)
		       ]),
	    free_sgml_parser(Parser)),
	retract(html_head_dom(HeadDOM)).

on_begin(head, Attrs, Parser) :-
	sgml_parse(Parser,
		   [ document(DOM),
		     parse(content)
		   ]),
	asserta(html_head_dom(element(head, Attrs, DOM))).

%%	xrds_specified_location(+URL, -XRDSLocation) is nondet.
%
%	Hook that allows for specifying locations of XRDS documents. For
%	example, Google does not reply to   Yadis discovery messages. We
%	can fake it does using:
%
%	  ==
%	  yadis:xrds_specified_location('http://google.com/',
%					'https://www.google.com/accounts/o8/id').
%	  ==
%
%	If this hook succeeds with XRDSLocation bound to `-` (minus), we
%	assume there is no XRDS document associated to URL.  This can be
%	used to avoid retrieving misleading or broken XRDS documents.
