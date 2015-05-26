/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(jquery, []).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).

:- setting(version, atom, '1.11.3.min',
	   'Version of jquery served by the html resource "jquery"').

/** <module> Provide JQuery

This module provides the HTML  resource   `jquery`.  To  get the default
version of jquery included in a web page,  make sure this file is loaded
and include the following into the HTML generation DCG.

  ==
    html_requires(jquery),
  ==

The file served is determined by the setting `jquery:version` and loaded
from the file search path `js`,  the   default  for which is provided by
library(http/http_server_files).

Note that including jquery into the   HTTP  infrastructure is not ideal.
However, components, such  as  PlDoc  and   Pengines  as  well  as  user
applications require jquery, causing this JavaScript  to be installed in
many places. That is even worse.

# Using your own copy

To use your own copy of jquery, add   your jquery file to a directory in
the   `js`   file   search    path     (see    file_search_path/2    and
absolute_file_name/3) and set `jquery:version` to   the file version you
provided. Alternatively, you  can  define   the  html  resource `jquery`
before loading this file.
*/

register_jquery :-
	setting(version, Version),
	atomic_list_concat(['jquery-', Version, '.js'], JQuery),
	html_resource(jquery,
		      [ virtual(true),
			requires([ js(JQuery)
				 ])
		      ]).

:- if(\+html_current_resource(jquery)).
:- initialization register_jquery.
:- listen(settings(changed(jquery:version, _, _)),
	  register_jquery).
:- endif.

