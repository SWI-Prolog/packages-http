/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2014, VU University Amsterdam
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

:- module(http_dirindex,
          [ http_reply_dirindex/3,      % +PhysicalDir, +Options, +Request
            directory_index//2          % +PhysicalDir, +Options
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_head)).
:- use_module(library(http/mimetype)).
:- use_module(library(apply)).
:- use_module(library(option)).

:- predicate_options(http_reply_dirindex/3, 2,
                     [ title(any),
                       pass_to(http_dispatch:http_safe_file/2, 2)
                     ]).

/** <module> HTTP directory listings

This module provides a simple API to   generate  an index for a physical
directory. The index can be customised   by  overruling the dirindex.css
CSS file and by defining  additional  rules   for  icons  using the hook
http:file_extension_icon/2.

@tbd    Provide more options (sorting, selecting columns, hiding files)
*/

%!  http_reply_dirindex(+DirSpec, +Options, +Request) is det.
%
%   Provide a directory listing for Request, assuming it is an index
%   for the physical directrory Dir. If   the  request-path does not
%   end with /, first return a moved (301 Moved Permanently) reply.
%
%   The  calling  conventions  allows  for    direct   calling  from
%   http_handler/3.

http_reply_dirindex(DirSpec, Options, Request) :-
    http_safe_file(DirSpec, Options),
    absolute_file_name(DirSpec, Dir,
                       [ file_type(directory),
                         access(read)
                       ]),
    memberchk(path(Path), Request),
    (   atom_concat(PlainPath, /, Path),
        merge_options(Options,
                      [ title(['Index of ', PlainPath]) ],
                      Options1)
    ->  dir_index(Dir, Options1)
    ;   atom_concat(Path, /, NewLocation),
        throw(http_reply(moved(NewLocation)))
    ).

dir_index(Dir, Options) :-
    directory_members(Dir, SubDirs, Files),
    option(title(Title), Options, Dir),
    reply_html_page(
        dir_index(Dir, Title),
        title(Title),
        [ h1(Title),
          \dirindex_table(SubDirs, Files, Options)
        ]).

directory_members(Dir, Dirs, Files) :-
    atom_concat(Dir, '/*', Pattern),
    expand_file_name(Pattern, Matches),
    partition(exists_directory, Matches, Dirs, Files).

%!  directory_index(+Dir, +Options)// is det.
%
%   Show index for a directory.  Options processed:
%
%     * order_by(+Field)
%     Sort the files in the directory listing by Field.  Field
%     is one of =name= (default), =size= or =time=.
%     * order(+AscentDescent)
%     Sorting order.  Default is =ascending=.  The altenative is
%     =descending=

directory_index(Dir, Options) -->
    { directory_members(Dir, SubDirs, Files) },
    dirindex_table(SubDirs, Files, Options).

dirindex_table(SubDirs, Files, Options) -->
    { option(order_by(By), Options, name),
      sort_files(By, Files, SortedFiles0),
      asc_desc(SortedFiles0, SortedFiles, Options)
    },
    html_requires(http_dirindex),
    html(table(class(dirindex),
               [ \dirindex_title,
                 \back
               | \dirmembers(SubDirs, SortedFiles)
               ])).

sort_files(name, Files, Files) :- !.
sort_files(Order, Files, Sorted) :-
    map_list_to_pairs(key_file(Order), Files, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

key_file(name, File, Base) :-
    file_base_name(File, Base).
key_file(size, File, Size) :-
    size_file(File, Size).
key_file(time, File, Time) :-
    time_file(File, Time).

asc_desc(Files, Ordered, Options) :-
    (   option(order(ascending), Options, ascending)
    ->  Ordered = Files
    ;   reverse(Files, Ordered)
    ).

dirindex_title -->
    html(tr(class(dirindex_header),
            [ th(class(icon),     ''),
              th(class(name),     'Name'),
              th(class(modified), 'Last modified'),
              th(class(size),     'Size')
            ])).

back -->
    html(tr([ \icon_cell('back.png', '[UP]'),
              \name_cell(.., 'Up'),
              td(class(modified), -),
              td(class(size),     -)
            ])).

dirmembers(Dirs, Files) -->
    dir_rows(Dirs, odd, End),
    file_rows(Files, End, _).

dir_rows([], OE, OE) --> [].
dir_rows([H|T], OE0, OE) -->
    dir_row(H, OE0),
    { oe(OE0, OE1) },
    dir_rows(T, OE1, OE).

file_rows([], OE, OE) --> [].
file_rows([H|T], OE0, OE) -->
    file_row(H, OE0),
    {oe(OE0, OE1)},
    file_rows(T, OE1, OE).

oe(odd, even).
oe(even, odd).

dir_row(Dir, OE) -->
    { file_base_name(Dir, Name)
    },
    html(tr(class(OE),
            [ \icon_cell('folder.png', '[DIR]'),
              \name_cell(Name, Name),
              \modified_cell(Dir),
              td(class(size), -)
            ])).


file_row(File, OE) -->
    { file_base_name(File, Name),
      file_mime_type(File, MimeType),
      mime_type_icon(MimeType, IconName),
      uri_encoded(path, Name, Ref)
    },
    html(tr(class(OE),
            [ \icon_cell(IconName, '[FILE]'),
              \name_cell(Ref, Name),
              \modified_cell(File),
              td(class(size), \size(File))
            ])).

icon_cell(IconName, Alt) -->
    { http_absolute_location(icons(IconName), Icon, [])
    },
    html(td(class(icon), img([src(Icon), alt(Alt)]))).


name_cell(Ref, Name) -->
    html(td(class(name), a(href(Ref), Name))).


modified_cell(Name) -->
    { time_file(Name, Stamp),
      format_time(string(Date), '%+', Stamp)
    },
    html(td(class(modified), Date)).

size(Name) -->
    { size_file(Name, Size)
    },
    html('~D'-[Size]).

%!  mime_type_icon(+MimeType, -Icon) is det.
%
%   Determine the icon that is used  to   show  a  file of the given
%   extension. This predicate can  be   hooked  using  the multifile
%   http:mime_type_icon/2 hook with the same  signature. Icon is the
%   plain name of an image file that appears in the file-search-path
%   =icons=.
%
%   @param  MimeType  is  a  term    Type/SubType   as  produced  by
%   file_mime_type/2.

mime_type_icon(Ext, Icon) :-
    http:mime_type_icon(Ext, Icon),
    !.
mime_type_icon(_, 'generic.png').

%!  http:mime_type_icon(+MimeType, -IconName) is nondet.
%
%   Multi-file hook predicate that can be used to associate icons to
%   files listed by http_reply_dirindex/3. The   actual icon file is
%   located by absolute_file_name(icons(IconName), Path, []).
%
%   @see serve_files_in_directory/2 serves the images.

:- multifile
    http:mime_type_icon/2.

http:mime_type_icon(application/pdf,      'layout.png').
http:mime_type_icon(text/csrc,            'c.png').
http:mime_type_icon(application/'x-gzip', 'compressed.png').
http:mime_type_icon(application/'x-gtar', 'compressed.png').
http:mime_type_icon(application/zip,      'compressed.png').


                 /*******************************
                 *            RESOURCES         *
                 *******************************/

:- html_resource(http_dirindex,
                 [ virtual(true),
                   requires([ css('dirindex.css')
                            ])
                 ]).
