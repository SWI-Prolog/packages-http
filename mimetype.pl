/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
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

:- module(mimetype,
	  [ file_mime_type/2		% +Path, -Type
	  ]).

/** <module> Determine mime-type for a file

Simple library to guess the mime-type from   the extension of a file. As
various applications need  to  do  this   type  ofinferencing  it  seems
worthwhile to place this functionality in an extensible library.

@tbd	Consider content handling (using the Unix file command)
@tbd	Allow parameters? (e.g. text/html; charset=UTF-8)
*/

%%	file_mime_type(+FileName, -MimeType) is semidet.
%
%	True when MimeType is  the  mime-type   to  be  used for sending
%	FileName. The default rules can be overridden and extended using
%	the hook mime:mime_extension/2.
%
%	@param MimeType is a compound term of the form Type/SubType.

file_mime_type(File, MimeType) :-
	file_name_extension(_, Ext, File),
	(   current_prolog_flag(windows, true)
	->  downcase_atom(Ext, Lower),
	    mime_extension(Lower, MimeType)
	;   mime_extension(Ext, M0)
	->  MimeType = M0
	;   downcase_atom(Ext, Lower),
	    mime_extension(Lower, MimeType)
	).

%%	mime:mime_extension(+Ext, -MimeType)
%
%	Hook that is called by file_mime_type/2 before the default table
%	is examined.

:- multifile
	mime:mime_extension/2.

mime_extension(Ext, MimeType) :-
	(   mime:mime_extension(Ext, Mime)
	->  MimeType = Mime
	;   ext_mimetype(Ext, Mime)
	->  MimeType = Mime
	;   default_mimetype(MimeType)
	).

%%	default_mimetype(-MimeType) is semidet.
%
%	If the mime-type cannot be determined   from the file extension,
%	this predicate is used as fallback.  It takes the value from the
%	Prolog flag =default_mimetype=. To change the default, use e.g.,
%
%	  ==
%	  :- set_prolog_flag(default_mimetype, text/plain).
%	  ==
%
%	The initial default mime-type   is  =|application/unknown|=. Use
%	the value =|-|= to denote there is no default.

:- create_prolog_flag(default_mimetype, application/unknown, []).

default_mimetype(MimeType) :-
	current_prolog_flag(default_mimetype, MimeType),
	MimeType = _/_.


%%	ext_mimetype(+Extension, -MimeType) is semidet.
%
%	Built-in table of file-name extension to mime-type mappings.

					% plain text
ext_mimetype(txt,  text/plain).
					% markup
ext_mimetype(htm,  text/html).
ext_mimetype(html, text/html).
ext_mimetype(xhtml, application/'xhtml+xml').
ext_mimetype(sgml, text/'x-sgml').
ext_mimetype(sgm,  text/'x-sgml').
ext_mimetype(xml,  text/xml).
ext_mimetype(css,  text/css).
ext_mimetype(xsl,  text/xml).		% Unclear what this should be.
					% semantic web stuff
ext_mimetype(rdf,  application/'rdf+xml').
ext_mimetype(rdfs, application/'rdf+xml').
ext_mimetype(owl,  application/'rdf+xml').
					% Prolog source
ext_mimetype(pl,   text/plain).
					% Other languages
ext_mimetype(c,    text/csrc).
ext_mimetype(h,    text/chdr).
ext_mimetype(py,   text/'x-python').
ext_mimetype(java, text/'x-java').
					% Packaged formats
ext_mimetype(gz,   application/'x-gzip').
ext_mimetype(zip,  application/zip).
ext_mimetype(tgz,  application/'x-gtar').
					% Some document formats
ext_mimetype(pdf,  application/pdf).
ext_mimetype(doc,  application/msword).
					% Java classes
ext_mimetype(class, application/'octet-stream').
ext_mimetype(jar,  application/'x-java-archive').
					% JavaScript
ext_mimetype(js,   text/javascript).
					% Visual Basic Script :-(
ext_mimetype(vbs,  text/vbscript).
					% Some image formats
ext_mimetype(jpg,  image/jpeg).
ext_mimetype(jpeg, image/jpeg).
ext_mimetype(gif,  image/gif).
ext_mimetype(png,  image/png).
ext_mimetype(tif,  image/tiff).
ext_mimetype(tiff, image/tiff).
ext_mimetype(xpm,  image/'x-xpixmap').
ext_mimetype(ico,  image/'x-ico').
ext_mimetype(svg,  image/'svg+xml').
					% Google earth
ext_mimetype(kml,  application/'vnd.google-earth.kml+xml').
ext_mimetype(kmz,  application/'vnd.google-earth.kmz').

					% Flash
ext_mimetype(swf,  application/'x-shockwave-flash').
ext_mimetype(flv,  video/'x-flv').
					% MP3
ext_mimetype(mp3,  audio/mpeg).
					% Downloads
ext_mimetype(rpm,  application/'x-rpm').
ext_mimetype(exe,  application/'x-executable').
