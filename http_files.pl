/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(http_files,
	  [ http_reply_from_files/3	% +Dir, +Options, +Request
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- predicate_options(http_reply_from_files/3, 2, [indexes(list(atom))]).

/** <module> Serve plain files from a hierarchy

Although the SWI-Prolog web-server is intended   to serve documents that
needed to be computed dynamically,  serving   plain  files  is sometimes
necessary.   This   small   module   combines   the   functionality   of
http_reply_file/3  and  http_reply_dirindex/3  to  act    as   a  simple
web-server. Such a server  can  be   created  using  the  following code
sample, which starts a server at port   8080  that serves files from the
current directory ('.'). Note that the   handler needs a =prefix= option
to specify it must  handle  all  paths   that  begin  with  the registed
location of the handler.

  ==
  :- use_module(library(http/thread_httpd)).
  :- use_module(library(http/http_dispatch)).

  :- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

  :- initialization
	http_server(http_dispatch, [port(8080)]).
  ==

@see	pwp_handler/2 provides similar facilities, where .pwp files
	can be used to add dynamic behaviour.
*/


%%	http_reply_from_files(+Dir, +Options, +Request)
%
%	HTTP handler that serves files  from   the  directory  Dir. This
%	handler uses http_reply_file/3 to  reply   plain  files.  If the
%	request resolves to a directory, it uses the option =indexes= to
%	locate an index file (see   below) or uses http_reply_dirindex/3
%	to create a listing of the directory.
%
%	Options:
%
%	  * indexes(+List)
%	  List of files tried to find an index for a directory.  The
%	  default is ['index.html'].
%
%	Note that this handler must be tagged as a =prefix= handler (see
%	http_handler/3 and module introduction). This  also implies that
%	it is possible to  override  more   specific  locations  in  the
%	hierarchy using http_handler/3 with a longer path-specifier.
%
%	@param	Dir is either a directory or an path-specification as
%		used by absolute_file_name/3.  This option provides
%		great flexibility in (re-)locating the physical files
%		and allows merging the files of multiple physical
%		locations into one web-hierarchy by using multiple
%		user:file_search_path/2 clauses that define the same
%		alias.
%	@see	The hookable predicate file_mime_type/2 is used to
%		determine the =|Content-type|= from the file name.

http_reply_from_files(Dir, Options, Request) :-
	(   memberchk(path_info(PathInfo), Request)
	->  true
	;   PathInfo = ''
	),
	http_safe_file(PathInfo, []),
	locate_file(Dir, PathInfo, Path, IsFile, Options),
	(   IsFile == true
	->  http_reply_file(Path, [unsafe(true)], Request)
	;   http_reply_dirindex(Path, [unsafe(true)], Request)
	).

locate_file(Dir, PathInfo, Result, IsFile, Options) :-
	absolute_file_name(Dir, DirPath,
			   [ file_type(directory),
			     access(read),
			     solutions(all)
			   ]),
	directory_file_path(DirPath, PathInfo, Path),
	(   exists_file(Path)
	->  IsFile = true,
	    Result = Path
	;   exists_directory(Path),
	    (   option(indexes(Indexes), Options, ['index.html']),
		member(Index, Indexes),
		directory_file_path(Path, Index, IndexFile),
		exists_file(IndexFile)
	    ->  Result = IndexFile,
		IsFile = true
	    ;   Result = Path,
		IsFile = false
	    )
	).


