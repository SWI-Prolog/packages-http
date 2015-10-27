/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2009, University of Amsterdam

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

:- module(http_dispatch,
	  [ http_dispatch/1,		% +Request
	    http_handler/3,		% +Path, +Predicate, +Options
	    http_delete_handler/1,	% +Path
	    http_reply_file/3,		% +File, +Options, +Request
	    http_redirect/3,		% +How, +Path, +Request
	    http_404/2,			% +Options, +Request
	    http_switch_protocol/2,	% :Goal, +Options
	    http_current_handler/2,	% ?Path, ?Pred
	    http_current_handler/3,	% ?Path, ?Pred
	    http_location_by_id/2,	% +ID, -Location
	    http_link_to_id/3,		% +ID, +Parameters, -HREF
	    http_reload_with_parameters/3, % +Request, +Parameters, -HREF
	    http_safe_file/2		% +Spec, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_header)).
:- use_module(library(http/thread_httpd)).

:- predicate_options(http_404/2, 1, [index(any)]).
:- predicate_options(http_reply_file/3, 2,
		     [ cache(boolean),
		       mime_type(any),
		       static_gzip(boolean),
		       pass_to(http_safe_file/2, 2),
		       headers(list)
		     ]).
:- predicate_options(http_safe_file/2, 2, [unsafe(boolean)]).
:- predicate_options(http_switch_protocol/2, 2, []).

/** <module> Dispatch requests in the HTTP server

This module can be placed between   http_wrapper.pl  and the application
code to associate HTTP _locations_ to   predicates that serve the pages.
In addition, it associates parameters  with   locations  that  deal with
timeout handling and user authentication.  The typical setup is:

==
server(Port, Options) :-
	http_server(http_dispatch,
		    [ port(Port)
		    | Options
		    ]).

:- http_handler('/index.html', write_index, []).

write_index(Request) :-
	...
==
*/

:- setting(http:time_limit, nonneg, 300,
	   'Time limit handling a single query (0=infinite)').

%%	http_handler(+Path, :Closure, +Options) is det.
%
%	Register Closure as a handler for HTTP requests. Path is a
%	specification as provided by http_path.pl.  If an HTTP
%	request arrives at the server that matches Path, Closure
%	is called with one extra argument: the parsed HTTP request.
%	Options is a list containing the following options:
%
%		* authentication(+Type)
%		Demand authentication.  Authentication methods are
%		pluggable.  The library http_authenticate.pl provides
%		a plugin for user/password based =Basic= HTTP
%		authentication.
%
%		* chunked
%		Use =|Transfer-encoding: chunked|= if the client
%		allows for it.
%
%		* content_type(+Term)
%		Specifies the content-type of the reply.  This value is
%		currently not used by this library.  It enhances the
%		reflexive capabilities of this library through
%		http_current_handler/3.
%
%		* id(+Term)
%		Identifier of the handler.  The default identifier is
%		the predicate name.  Used by http_location_by_id/2.
%
%		* hide_children(+Bool)
%		If =true= on a prefix-handler (see prefix), possible
%		children are masked.  This can be used to (temporary)
%		overrule part of the tree.
%
%		* prefix
%		Call Pred on any location that is a specialisation of
%		Path.  If multiple handlers match, the one with the
%		longest path is used.  Options defined with a prefix
%		handler are the default options for paths that start
%		with this prefix.  Note that the handler acts as a
%		fallback handler for the tree below it:
%
%		  ==
%		  :- http_handler(/, http_404([index('index.html')]),
%				  [spawn(my_pool),prefix]).
%		  ==
%
%		* priority(+Integer)
%		If two handlers handle the same path, the one with the
%		highest priority is used.  If equal, the last registered
%		is used.  Please be aware that the order of clauses in
%		multifile predicates can change due to reloading files.
%		The default priority is 0 (zero).
%
%		* spawn(+SpawnOptions)
%		Run the handler in a seperate thread.  If SpawnOptions
%		is an atom, it is interpreted as a thread pool name
%		(see create_thread_pool/3).  Otherwise the options
%		are passed to http_spawn/2 and from there to
%		thread_create/3.  These options are typically used to
%		set the stack limits.
%
%		* time_limit(+Spec)
%		One of =infinite=, =default= or a positive number
%		(seconds).  If =default=, the value from the setting
%		=http:time_limit= is taken. The default of this
%		setting is 300 (5 minutes).  See setting/2.
%
%	Note that http_handler/3 is normally invoked  as a directive and
%	processed using term-expansion.  Using   term-expansion  ensures
%	proper update through make/0 when the specification is modified.
%	We do not expand when the  cross-referencer is running to ensure
%	proper handling of the meta-call.
%
%	@error	existence_error(http_location, Location)
%	@see    http_reply_file/3 and http_redirect/3 are generic
%		handlers to serve files and achieve redirects.

:- dynamic handler/4.			% Path, Action, IsPrefix, Options
:- multifile handler/4.
:- dynamic generation/1.

:- meta_predicate
	http_handler(+, :, +),
	http_current_handler(?, :),
	http_current_handler(?, :, ?),
	http_switch_protocol(2, +).

http_handler(Path, Pred, Options) :-
	strip_module(Pred, M, P),
	compile_handler(Path, M:P, Options, Clause),
	next_generation,
	assert(Clause).

:- multifile
	system:term_expansion/2.

system:term_expansion((:- http_handler(Path, Pred, Options)), Clause) :-
	\+ current_prolog_flag(xref, true),
	prolog_load_context(module, M),
	compile_handler(Path, M:Pred, Options, Clause),
	next_generation.


%%	http_delete_handler(+Spec) is det.
%
%	Delete handler for Spec. Typically, this should only be used for
%	handlers that are registered dynamically. Spec is one of:
%
%	    * id(Id)
%	    Delete a handler with the given id.  The default id is the
%	    handler-predicate-name.
%
%	    * path(Path)
%	    Delete handler that serves the given path.

http_delete_handler(id(Id)) :- !,
	clause(handler(_Path, _:Pred, _, Options), true, Ref),
	functor(Pred, DefID, _),
	option(id(Id0), Options, DefID),
	Id == Id0,
	erase(Ref),
	next_generation.
http_delete_handler(path(Path)) :- !,
	retractall(handler(Path, _Pred, _, _Options)),
	next_generation.
http_delete_handler(Path) :-
	http_delete_handler(path(Path)).


%%	next_generation is det.
%%	current_generation(-G) is det.
%
%	Increment the generation count.

next_generation :-
	retractall(id_location_cache(_,_)),
	with_mutex(http_dispatch, next_generation_unlocked).

next_generation_unlocked :-
	retract(generation(G0)), !,
	G is G0	+ 1,
	assert(generation(G)).
next_generation_unlocked :-
	assert(generation(1)).

current_generation(G) :-
	with_mutex(http_dispatch, generation(G)), !.
current_generation(0).


%%	compile_handler(+Path, :Pred, +Options) is det.
%
%	Compile a handler specification. For now we this is a no-op, but
%	in the feature can make this more efficiently, especially in the
%	presence of one or multiple prefix declarations. We can also use
%	this to detect conflicts.

compile_handler(prefix(Path), Pred, Options,
		http_dispatch:handler(Path, Pred, true, Options)) :- !,
	check_path(Path, Path1),
	print_message(warning, http_dispatch(prefix(Path1))).
compile_handler(Path, Pred, Options0,
		http_dispatch:handler(Path1, Pred, IsPrefix, Options)) :-
	check_path(Path, Path1),
	(   select(prefix, Options0, Options)
	->  IsPrefix = true
	;   IsPrefix = false,
	    Options = Options0
	).

%%	check_path(+PathSpecIn, -PathSpecOut) is det.
%
%	Validate the given path specification.  We want one of
%
%		* AbsoluteLocation
%		* Alias(Relative)
%
%	Similar  to  absolute_file_name/3,  Relative  can    be  a  term
%	_|Component/Component/...|_
%
%	@error	domain_error, type_error
%	@see	http_absolute_location/3

check_path(Path, Path) :-
	atom(Path), !,
	(   sub_atom(Path, 0, _, _, /)
	->  true
	;   domain_error(absolute_http_location, Path)
	).
check_path(Alias, AliasOut) :-
	compound(Alias),
	Alias =.. [Name, Relative], !,
	to_atom(Relative, Local),
	(   sub_atom(Local, 0, _, _, /)
	->  domain_error(relative_location, Relative)
	;   AliasOut =.. [Name, Local]
	).
check_path(PathSpec, _) :-
	type_error(path_or_alias, PathSpec).

to_atom(Atom, Atom) :-
	atom(Atom), !.
to_atom(Path, Atom) :-
	phrase(path_to_list(Path), Components), !,
	atomic_list_concat(Components, '/', Atom).
to_atom(Path, _) :-
	ground(Path), !,
	type_error(relative_location, Path).
to_atom(Path, _) :-
	instantiation_error(Path).

path_to_list(Var) -->
	{ var(Var), !,
	  fail
	}.
path_to_list(A/B) -->
	path_to_list(A),
	path_to_list(B).
path_to_list(Atom) -->
	{ atom(Atom) },
	[Atom].



%%	http_dispatch(Request) is det.
%
%	Dispatch a Request using http_handler/3 registrations.

http_dispatch(Request) :-
	memberchk(path(Path), Request),
	find_handler(Path, Pred, Options),
	authentication(Options, Request, Fields),
	append(Fields, Request, AuthRequest),
	action(Pred, AuthRequest, Options).


%%	http_current_handler(+Location, :Closure) is semidet.
%%	http_current_handler(-Location, :Closure) is nondet.
%
%	True if Location is handled by Closure.

http_current_handler(Path, Closure) :-
	atom(Path), !,
	path_tree(Tree),
	find_handler(Tree, Path, Closure, _).
http_current_handler(Path, M:C) :-
	handler(Spec, M:C, _, _),
	http_absolute_location(Spec, Path, []).

%%	http_current_handler(+Location, :Closure, -Options) is semidet.
%%	http_current_handler(?Location, :Closure, ?Options) is nondet.
%
%	Resolve the current handler and options to execute it.

http_current_handler(Path, Closure, Options) :-
	atom(Path), !,
	path_tree(Tree),
	find_handler(Tree, Path, Closure, Options).
http_current_handler(Path, M:C, Options) :-
	handler(Spec, M:C, _, _),
	http_absolute_location(Spec, Path, []),
	path_tree(Tree),
	find_handler(Tree, Path, _, Options).


%%	http_location_by_id(+ID, -Location) is det.
%
%	Find the HTTP Location of handler with   ID. If the setting (see
%	setting/2)  http:prefix  is  active,  Location  is  the  handler
%	location prefixed with the prefix setting.   Handler  IDs can be
%	specified in two ways:
%
%	    * id(ID)
%	    If this appears in the option list of the handler, this
%	    it is used and takes preference over using the predicate.
%	    * M:PredName
%	    The module-qualified name of the predicate.
%	    * PredName
%	    The unqualified name of the predicate.
%
%	@error existence_error(http_handler_id, Id).
%	@deprecated The predicate http_link_to_id/3 provides the same
%	functionality with the option to add query parameters or a
%	path parameter.

:- dynamic
	id_location_cache/2.

http_location_by_id(ID, Location) :-
	must_be(ground, ID),
	id_location_cache(ID, L0), !,
	Location = L0.
http_location_by_id(ID, Location) :-
	findall(P-L, location_by_id(ID, L, P), List),
	keysort(List, RevSorted),
	reverse(RevSorted, Sorted),
	(   Sorted = [_-One]
	->  assert(id_location_cache(ID, One)),
	    Location = One
	;   List == []
	->  existence_error(http_handler_id, ID)
	;   List = [P0-Best,P1-_|_]
	->  (   P0 == P1
	    ->	print_message(warning,
			      http_dispatch(ambiguous_id(ID, Sorted, Best)))
	    ;	true
	    ),
	    assert(id_location_cache(ID, Best)),
	    Location = Best
	).

location_by_id(ID, Location, Priority) :-
	location_by_id_raw(ID, L0, Priority),
	to_path(L0, Location).

to_path(prefix(Path0), Path) :- !,	% old style prefix notation
	add_prefix(Path0, Path).
to_path(Path0, Path) :-
	atomic(Path0), !,		% old style notation
	add_prefix(Path0, Path).
to_path(Spec, Path) :-			% new style notation
	http_absolute_location(Spec, Path, []).

add_prefix(P0, P) :-
	(   catch(setting(http:prefix, Prefix), _, fail),
	    Prefix \== ''
	->  atom_concat(Prefix, P0, P)
	;   P = P0
	).

location_by_id_raw(ID, Location, Priority) :-
	handler(Location, _, _, Options),
	option(id(ID), Options),
	option(priority(P0), Options, 0),
	Priority is P0+1000.		% id(ID) takes preference over predicate
location_by_id_raw(ID, Location, Priority) :-
	handler(Location, M:C, _, Options),
	option(priority(Priority), Options, 0),
	functor(C, PN, _),
	(   ID = M:PN
	;   ID = PN
	), !.


%%	http_link_to_id(+HandleID, +Parameters, -HREF)
%
%	HREF is a link on the local server   to a handler with given ID,
%	passing the given Parameters. This   predicate is typically used
%	to formulate a HREF that resolves   to  a handler implementing a
%	particular predicate. The code below provides a typical example.
%	The predicate user_details/1 returns a page with details about a
%	user from a given id. This predicate is registered as a handler.
%	The DCG user_link//1 renders a link   to  a user, displaying the
%	name and calling user_details/1  when   clicked.  Note  that the
%	location (root(user_details)) is irrelevant in this equation and
%	HTTP locations can thus be moved   freely  without breaking this
%	code fragment.
%
%	  ==
%	  :- http_handler(root(user_details), user_details, []).
%
%	  user_details(Request) :-
%	      http_parameters(Request,
%			      [ user_id(ID)
%			      ]),
%	      ...
%
%	  user_link(ID) -->
%	      { user_name(ID, Name),
%	        http_link_to_id(user_details, [id(ID)], HREF)
%	      },
%	      html(a([class(user), href(HREF)], Name)).
%	  ==
%
%	@arg Parameters is one of
%
%		- path_postfix(File) to pass a single value as the last
%		  segment of the HTTP location (path). This way of
%		  passing a parameter is commonly used in REST APIs.
%		- A list of search parameters for a =GET= request.
%
%	@see	http_location_by_id/2 and http_handler/3 for defining and
%		specifying handler IDs.

http_link_to_id(HandleID, path_postfix(File), HREF) :- !,
	http_location_by_id(HandleID, HandlerLocation),
	uri_encoded(path, File, EncFile),
	directory_file_path(HandlerLocation, EncFile, Location),
	uri_data(path, Components, Location),
	uri_components(HREF, Components).
http_link_to_id(HandleID, Parameters, HREF) :-
	must_be(list, Parameters),
	http_location_by_id(HandleID, Location),
	uri_data(path, Components, Location),
	uri_query_components(String, Parameters),
	uri_data(search, Components, String),
	uri_components(HREF, Components).

%%	http_reload_with_parameters(+Request, +Parameters, -HREF) is det.
%
%	Create a request on the current handler with replaced search
%	parameters.

http_reload_with_parameters(Request, NewParams, HREF) :-
	memberchk(path(Path), Request),
	(   memberchk(search(Params), Request)
	->  true
	;   Params = []
	),
	merge_options(NewParams, Params, AllParams),
	uri_query_components(Search, AllParams),
	uri_data(path, Data, Path),
	uri_data(search, Data, Search),
	uri_components(HREF, Data).


%	hook into html_write:attribute_value//1.

:- multifile
	html_write:expand_attribute_value//1.

html_write:expand_attribute_value(location_by_id(ID)) -->
	{ http_location_by_id(ID, Location) },
	html_write:html_quoted_attribute(Location).


%%	authentication(+Options, +Request, -Fields) is det.
%
%	Verify  authentication  information.   If    authentication   is
%	requested through Options, demand it. The actual verification is
%	done by the multifile   predicate  http_dispatch:authenticate/3.
%	The  library  http_authenticate.pl  provides  an  implementation
%	thereof.
%
%	@error	permission_error(access, http_location, Location)

:- multifile
	http:authenticate/3.

authentication([], _, []).
authentication([authentication(Type)|Options], Request, Fields) :- !,
	(   http:authenticate(Type, Request, XFields)
	->  append(XFields, More, Fields),
	    authentication(Options, Request, More)
	;   memberchk(path(Path), Request),
	    throw(error(permission_error(access, http_location, Path), _))
	).
authentication([_|Options], Request, Fields) :-
	authentication(Options, Request, Fields).


%%	find_handler(+Path, -Action, -Options) is det.
%
%	Find the handler to call from Path.  Rules:
%
%		* If there is a matching handler, use this.
%		* If there are multiple prefix(Path) handlers, use the
%		  longest.
%
%	If there is a handler for =|/dir/|=   and  the requested path is
%	=|/dir|=, find_handler/3 throws a  http_reply exception, causing
%	the wrapper to generate a 301 (Moved Permanently) reply.
%
%	@error	existence_error(http_location, Location)
%	@throw	http_reply(moved(Dir))
%	@tbd	Introduce automatic redirection to indexes here?

find_handler(Path, Action, Options) :-
	path_tree(Tree),
	(   find_handler(Tree, Path, Action, Options)
	->  true
	;   \+ sub_atom(Path, _, _, 0, /),
	    atom_concat(Path, /, Dir),
	    find_handler(Tree, Dir, Action, Options)
	->  throw(http_reply(moved(Dir)))
	;   throw(error(existence_error(http_location, Path), _))
	).


find_handler([node(prefix(Prefix), PAction, POptions, Children)|_],
	     Path, Action, Options) :-
	sub_atom(Path, 0, _, After, Prefix), !,
	(   option(hide_children(false), POptions, false),
	    find_handler(Children, Path, Action, Options)
	->  true
	;   Action = PAction,
	    path_info(After, Path, POptions, Options)
	).
find_handler([node(Path, Action, Options, _)|_], Path, Action, Options) :- !.
find_handler([_|Tree], Path, Action, Options) :-
	find_handler(Tree, Path, Action, Options).

path_info(0, _, Options,
	  [prefix(true)|Options]) :- !.
path_info(After, Path, Options,
	  [path_info(PathInfo),prefix(true)|Options]) :-
	sub_atom(Path, _, After, 0, PathInfo).


%%	action(+Action, +Request, +Options) is det.
%
%	Execute the action found.  Here we take care of the options
%	=time_limit=, =chunked= and =spawn=.
%
%	@error	goal_failed(Goal)

action(Action, Request, Options) :-
	memberchk(chunked, Options), !,
	format('Transfer-encoding: chunked~n'),
	spawn_action(Action, Request, Options).
action(Action, Request, Options) :-
	spawn_action(Action, Request, Options).

spawn_action(Action, Request, Options) :-
	option(spawn(Spawn), Options), !,
	spawn_options(Spawn, SpawnOption),
	http_spawn(time_limit_action(Action, Request, Options), SpawnOption).
spawn_action(Action, Request, Options) :-
	time_limit_action(Action, Request, Options).

spawn_options([], []) :- !.
spawn_options(Pool, Options) :-
	atom(Pool), !,
	Options = [pool(Pool)].
spawn_options(List, List).

time_limit_action(Action, Request, Options) :-
	(   option(time_limit(TimeLimit), Options),
	    TimeLimit \== default
	->  true
	;   setting(http:time_limit, TimeLimit)
	),
	number(TimeLimit),
	TimeLimit > 0, !,
	call_with_time_limit(TimeLimit, call_action(Action, Request, Options)).
time_limit_action(Action, Request, Options) :-
	call_action(Action, Request, Options).


%%	call_action(+Action, +Request, +Options)
%
%	@tbd	reply_file is normal call?

call_action(reply_file(File, FileOptions), Request, _Options) :- !,
	http_reply_file(File, FileOptions, Request).
call_action(Pred, Request, Options) :-
	memberchk(path_info(PathInfo), Options), !,
	call_action(Pred, [path_info(PathInfo)|Request]).
call_action(Pred, Request, _Options) :-
	call_action(Pred, Request).

call_action(Pred, Request) :-
	(   call(Pred, Request)
	->  true
	;   extend(Pred, [Request], Goal),
	    throw(error(goal_failed(Goal), _))
	).

extend(Var, _, Var) :-
	var(Var), !.
extend(M:G0, Extra, M:G) :-
	extend(G0, Extra, G).
extend(G0, Extra, G) :-
	G0 =.. List,
	append(List, Extra, List2),
	G =.. List2.

%%	http_reply_file(+FileSpec, +Options, +Request) is det.
%
%	Options is a list of
%
%		* cache(+Boolean)
%		If =true= (default), handle If-modified-since and send
%		modification time.
%
%		* mime_type(+Type)
%		Overrule mime-type guessing from the filename as
%		provided by file_mime_type/2.
%
%		* static_gzip(+Boolean)
%		If true (default =false=) and, in addition to the plain
%		file, there is a =.gz= file that is not older than the
%		plain file and the client acceps =gzip= encoding, send
%		the compressed file with =|Transfer-encoding: gzip|=.
%
%		* unsafe(+Boolean)
%		If =false= (default), validate that FileSpec does not
%		contain references to parent directories.  E.g.,
%		specifications such as =|www('../../etc/passwd')|= are
%		not allowed.
%
%		* headers(+List)
%		Provides additional reply-header fields, encoded as a
%		list of =Field(Value)=.
%
%	If caching is not disabled,  it   processes  the request headers
%	=|If-modified-since|= and =Range=.
%
%	@throws	http_reply(not_modified)
%	@throws http_reply(file(MimeType, Path))

http_reply_file(File, Options, Request) :-
	http_safe_file(File, Options),
	absolute_file_name(File, Path,
			   [ access(read)
			   ]),
	(   option(cache(true), Options, true)
	->  (   memberchk(if_modified_since(Since), Request),
	        time_file(Path, Time),
		catch(http_timestamp(Time, Since), _, fail)
	    ->  throw(http_reply(not_modified))
	    ;	true
	    ),
	    (	memberchk(range(Range), Request)
	    ->	Reply = file(Type, Path, Range)
	    ;	option(static_gzip(true), Options),
		accepts_encoding(Request, gzip),
		file_name_extension(Path, gz, PathGZ),
		access_file(PathGZ, read),
		time_file(PathGZ, TimeGZ),
		time_file(Path, Time),
		TimeGZ >= Time
	    ->	Reply = gzip_file(Type, PathGZ)
	    ;	Reply = file(Type, Path)
	    )
	;   Reply = tmp_file(Type, Path)
	),
	(   option(mime_type(Type), Options)
	->  true
	;   file_mime_type(Path, Type)
	->  true
	;   Type = text/plain		% fallback type
	),
	option(headers(Headers), Options, []),
	throw(http_reply(Reply, Headers)).

accepts_encoding(Request, Enc) :-
	memberchk(accept_encoding(Accept), Request),
	split_string(Accept, ",", " ", Parts),
	member(Part, Parts),
	split_string(Part, ";", " ", [EncS|_]),
	atom_string(Enc, EncS).


%%	http_safe_file(+FileSpec, +Options) is det.
%
%	True if FileSpec is considered _safe_.  If   it  is  an atom, it
%	cannot  be  absolute  and  cannot   have  references  to  parent
%	directories. If it is of the   form  alias(Sub), than Sub cannot
%	have references to parent directories.
%
%	@error instantiation_error
%	@error permission_error(read, file, FileSpec)

http_safe_file(File, _) :-
	var(File), !,
	instantiation_error(File).
http_safe_file(_, Options) :-
	option(unsafe(true), Options, false), !.
http_safe_file(File, _) :-
	http_safe_file(File).

http_safe_file(File) :-
	compound(File),
	functor(File, _, 1), !,
	arg(1, File, Name),
	safe_name(Name, File).
http_safe_file(Name) :-
	(   is_absolute_file_name(Name)
	->  permission_error(read, file, Name)
	;   true
	),
	safe_name(Name, Name).

safe_name(Name, _) :-
	must_be(atom, Name),
	prolog_to_os_filename(FileName, Name),
	\+ unsafe_name(FileName), !.
safe_name(_, Spec) :-
	permission_error(read, file, Spec).

unsafe_name(Name) :- Name == '..'.
unsafe_name(Name) :- sub_atom(Name, 0, _, _, '../').
unsafe_name(Name) :- sub_atom(Name, _, _, _, '/../').
unsafe_name(Name) :- sub_atom(Name, _, _, 0, '/..').


%%	http_redirect(+How, +To, +Request) is det.
%
%	Redirect to a new  location.  The   argument  order,  using  the
%	Request as last argument, allows for  calling this directly from
%	the handler declaration:
%
%	    ==
%	    :- http_handler(root(.),
%			    http_redirect(moved, myapp('index.html')),
%			    []).
%	    ==
%
%	@param How is one of =moved=, =moved_temporary= or =see_other=
%	@param To is an atom, a aliased path as defined by
%	http_absolute_location/3. or a term location_by_id(Id). If To is
%	not absolute, it is resolved relative to the current location.

http_redirect(How, To, Request) :-
	(   To = location_by_id(Id)
	->  http_location_by_id(Id, URL)
	;   memberchk(path(Base), Request),
	    http_absolute_location(To, URL, [relative_to(Base)])
	),
	must_be(oneof([moved, moved_temporary, see_other]), How),
	Term =.. [How,URL],
	throw(http_reply(Term)).


%%	http_404(+Options, +Request) is det.
%
%	Reply using an "HTTP  404  not   found"  page.  This  handler is
%	intended as fallback handler  for   _prefix_  handlers.  Options
%	processed are:
%
%	    * index(Location)
%	    If there is no path-info, redirect the request to
%	    Location using http_redirect/3.
%
%	@error http_reply(not_found(Path))

http_404(Options, Request) :-
	option(index(Index), Options),
	\+ ( option(path_info(PathInfo), Request),
	     PathInfo \== ''
	   ), !,
	http_redirect(moved, Index, Request).
http_404(_Options, Request) :-
	option(path(Path), Request), !,
	throw(http_reply(not_found(Path))).
http_404(_Options, Request) :-
	domain_error(http_request, Request).


%%	http_switch_protocol(:Goal, +Options)
%
%	Send an =|HTTP 101 Switching   Protocols"|= reply. After sending
%	the  reply,  the  HTTP  library    calls   call(Goal,  InStream,
%	OutStream), where InStream and OutStream are  the raw streams to
%	the HTTP client. This allows the communication to continue using
%	an an alternative protocol.
%
%	If Goal fails or throws an exception,  the streams are closed by
%	the server. Otherwise  Goal  is   responsible  for  closing  the
%	streams. Note that  Goal  runs  in   the  HTTP  handler  thread.
%	Typically, the handler should be   registered  using the =spawn=
%	option if http_handler/3 or Goal   must  call thread_create/3 to
%	allow the HTTP worker to return to the worker pool.
%
%	The streams use binary  (octet)  encoding   and  have  their I/O
%	timeout set to the server  timeout   (default  60  seconds). The
%	predicate set_stream/2 can  be  used   to  change  the encoding,
%	change or cancel the timeout.
%
%	This predicate interacts with the server  library by throwing an
%	exception.
%
%	@param	Options is reserved for future extensions.  It must be
%		initialised to the empty list ([]).
%	@throws	http_reply(switch_protocol(Goal, Options))

http_switch_protocol(Goal, Options) :-
	throw(http_reply(switching_protocols(Goal, Options))).


		 /*******************************
		 *	  PATH COMPILATION	*
		 *******************************/

%%	path_tree(-Tree) is det.
%
%	Compile paths into  a  tree.  The   treee  is  multi-rooted  and
%	represented as a list of nodes, where each node has the form:
%
%		node(PathOrPrefix, Action, Options, Children)
%
%	The tree is a potentially complicated structure. It is cached in
%	a global variable. Note that this   cache is per-thread, so each
%	worker thread holds a copy of  the   tree.  If handler facts are
%	changed the _generation_ is  incremented using next_generation/0
%	and each worker thread will  re-compute   the  tree  on the next
%	ocasion.

path_tree(Tree) :-
	current_generation(G),
	nb_current(http_dispatch_tree, G-Tree), !. % Avoid existence error
path_tree(Tree) :-
	path_tree_nocache(Tree),
	current_generation(G),
	nb_setval(http_dispatch_tree, G-Tree).

path_tree_nocache(Tree) :-
	findall(Prefix, prefix_handler(Prefix, _, _), Prefixes0),
	sort(Prefixes0, Prefixes),
	prefix_tree(Prefixes, [], PTree),
	prefix_options(PTree, [], OPTree),
	add_paths_tree(OPTree, Tree).

prefix_handler(Prefix, Action, Options) :-
	handler(Spec, Action, true, Options),
	http_absolute_location(Spec, Prefix, []).

%%	prefix_tree(PrefixList, +Tree0, -Tree)
%
%	@param Tree	list(Prefix-list(Children))

prefix_tree([], Tree, Tree).
prefix_tree([H|T], Tree0, Tree) :-
	insert_prefix(H, Tree0, Tree1),
	prefix_tree(T, Tree1, Tree).

insert_prefix(Prefix, Tree0, Tree) :-
	select(P-T, Tree0, Tree1),
	sub_atom(Prefix, 0, _, _, P), !,
	insert_prefix(Prefix, T, T1),
	Tree = [P-T1|Tree1].
insert_prefix(Prefix, Tree, [Prefix-[]|Tree]).


%%	prefix_options(+PrefixTree, +DefOptions, -OptionTree)
%
%	Generate the option-tree for all prefix declarations.
%
%	@tbd	What to do if there are more?

prefix_options([], _, []).
prefix_options([P-C|T0], DefOptions,
	       [node(prefix(P), Action, Options, Children)|T]) :-
	once(prefix_handler(P, Action, Options0)),
	merge_options(Options0, DefOptions, Options),
	delete(Options, id(_), InheritOpts),
	prefix_options(C, InheritOpts, Children),
	prefix_options(T0, DefOptions, T).


%%	add_paths_tree(+OPTree, -Tree) is det.
%
%	Add the plain paths.

add_paths_tree(OPTree, Tree) :-
	findall(path(Path, Action, Options),
		plain_path(Path, Action, Options),
		Triples),
	add_paths_tree(Triples, OPTree, Tree).

add_paths_tree([], Tree, Tree).
add_paths_tree([path(Path, Action, Options)|T], Tree0, Tree) :-
	add_path_tree(Path, Action, Options, [], Tree0, Tree1),
	add_paths_tree(T, Tree1, Tree).


%%	plain_path(-Path, -Action, -Options) is nondet.
%
%	True if {Path,Action,Options} is registered and  Path is a plain
%	(i.e. not _prefix_) location.

plain_path(Path, Action, Options) :-
	handler(Spec, Action, false, Options),
	catch(http_absolute_location(Spec, Path, []), E,
	      (print_message(error, E), fail)).


%%	add_path_tree(+Path, +Action, +Options, +Tree0, -Tree) is det.
%
%	Add a path to a tree. If a  handler for the same path is already
%	defined, the one with the highest   priority or the latest takes
%	precedence.

add_path_tree(Path, Action, Options0, DefOptions, [],
	      [node(Path, Action, Options, [])]) :- !,
	merge_options(Options0, DefOptions, Options).
add_path_tree(Path, Action, Options, _,
	      [node(prefix(Prefix), PA, DefOptions, Children0)|RestTree],
	      [node(prefix(Prefix), PA, DefOptions, Children)|RestTree]) :-
	sub_atom(Path, 0, _, _, Prefix), !,
	delete(DefOptions, id(_), InheritOpts),
	add_path_tree(Path, Action, Options, InheritOpts, Children0, Children).
add_path_tree(Path, Action, Options1, DefOptions, [H0|T], [H|T]) :-
	H0 = node(Path, _, Options2, _),
	option(priority(P1), Options1, 0),
	option(priority(P2), Options2, 0),
	P1 >= P2, !,
	merge_options(Options1, DefOptions, Options),
	H = node(Path, Action, Options, []).
add_path_tree(Path, Action, Options, DefOptions, [H|T0], [H|T]) :-
	add_path_tree(Path, Action, Options, DefOptions, T0, T).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(http_dispatch(ambiguous_id(ID, _List, Selected))) -->
	[ 'HTTP dispatch: ambiguous handler ID ~q (selected ~q)'-[ID, Selected]
	].
prolog:message(http_dispatch(prefix(_Path))) -->
	[ 'HTTP dispatch: prefix(Path) is replaced by the option prefix'-[]
	].


		 /*******************************
		 *	      XREF		*
		 *******************************/

:- multifile
	prolog:meta_goal/2.
:- dynamic
	prolog:meta_goal/2.

prolog:meta_goal(http_handler(_, G, _), [G+1]).
prolog:meta_goal(http_current_handler(_, G), [G+1]).


		 /*******************************
		 *	       EDIT		*
		 *******************************/

% Allow edit(Location) to edit the implementation for an HTTP location.

:- multifile
	prolog_edit:locate/3.

prolog_edit:locate(Path, Spec, Location) :-
	atom(Path),
	sub_atom(Path, 0, _, _, /),
	Pred = _M:_H,
	catch(http_current_handler(Path, Pred), _, fail),
	closure_name_arity(Pred, 1, PI),
	prolog_edit:locate(PI, Spec, Location).

closure_name_arity(M:Term, Extra, M:Name/Arity) :- !,
	callable(Term),
	functor(Term, Name, Arity0),
	Arity is Arity0 + Extra.
closure_name_arity(Term, Extra, Name/Arity) :-
	callable(Term),
	functor(Term, Name, Arity0),
	Arity is Arity0 + Extra.


		 /*******************************
		 *	  CACHE CLEANUP		*
		 *******************************/

:- listen(settings(changed(http:prefix, _, _)),
	  next_generation).

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

user:message_hook(make(done(Reload)), _Level, _Lines) :-
	Reload \== [],
	next_generation,
	fail.
