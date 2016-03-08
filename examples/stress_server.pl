/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
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

:- module(http_stress_server,
	  [ server/1,			% +Port
	    profile/0
	  ]).
:- load_files([ library(http/thread_httpd),
		library(http/html_write),
		library(http/http_session),
		library(http/http_dispatch),
		library(http/http_parameters),
		library(http/http_error),
		library(thread_pool)
	      ],
	      [ silent(true)
	      ]).

/** <module> Sample HTTP server to run some stress tests

*/

%%	server(+Port) is det.
%
%	Start the server at Port.

server(Port) :-
	create_pools,
	server(Port,
	       [ workers(1)
	       ]).

server(Port, Options) :-
	http_server(http_dispatch,
		    [ port(Port),
		      timeout(20)
		    | Options
		    ]).

%%	create_pools
%
%	Create our thread pools.

create_pools :-
	thread_pool_create(single, 1, [backlog(0)]).

%%	profile
%
%	Run thread profiler on the one and only server.

profile :-
	findall(Id, http_current_worker(_, Id), Ids),
	(   Ids = [Id]
	->  tprofile(Id)
	;   Ids == []
	->  format(user_error, 'No HTTP server!~n', []),
	    fail
	;   format(user_error, 'Multiple HTPP workers: ~p~n', [Ids]),
	    fail
	).


		 /*******************************
		 *	     METHODS		*
		 *******************************/

:- http_handler('/ping', ping, []).
:- http_handler('/wait', wait, [chunked]).
:- http_handler(prefix('/spawn/'), spawn, [spawn(single)]).
:- http_handler(prefix('/spawn2/'), spawn, [spawn(single)]).

ping(_Request) :-
	format('Content-type: text/plain~n~n'),
	format('alive~n').

wait(Request) :-
	http_parameters(Request,
			[ wait(Time, [default(1)]),
			  count(N, [default(10)])
			]),
	wait(Time, N).

wait(Time, N) :-
	format('Content-type: text/plain~n~n'),
	forall(between(1, N, I),
	       (   sleep(Time),
		   format('~D~n', [I]),
		   flush_output
	       )).

%%	spawn(+Request)
%
%	Run requests under /spawn/ in their own thread.

spawn(Request) :-
	selectchk(path(Path), Request, Request1),
	(   sub_atom(Path, Start, _, _, /), Start > 0
	->  sub_atom(Path, Start, _, 0, NewPath)
	),
	http_dispatch([path(NewPath)|Request1]).

