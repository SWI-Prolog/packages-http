/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam
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

:- module(http_inetd,
          [ http_server/2               % :Goal, +Options
          ]).
:- use_module(http_wrapper).

:- meta_predicate
    http_server(:, +),
    server_loop(:, +).

/** <module> Run Prolog HTTP server from Unix inetd

This module implements handling a single request (or multiple as long as
=Keep-Alive= is respected), talking to stardard input and output.

@deprecated     This type of handling of HTTP requests should be
                considered outdated.  See library(http/thread_httpd).
*/

%!  http_server(:Goal, +Options)
%
%   Start the server from inetd. This is really easy as user_input
%   is connected to the HTTP input and user_output is the place to
%   write our reply to.

http_server(Goal, Options) :-
    prompt(_, ''),
    set_stream(user_output, buffer(full)),
    set_stream(user_output, encoding(octet)),
    set_stream(user_input, buffer(full)),
    set_stream(user_input, encoding(octet)),
    server_loop(Goal, Options).

server_loop(_, _) :-
    at_end_of_stream(user_input),
    !,
    halt.
server_loop(Goal, Options) :-
    http_wrapper(Goal, user_input, user_output, Connection, []),
    (   downcase_atom(Connection, 'keep-alive')
    ->  server_loop(Goal, Options)
    ;   halt
    ).
server_loop(_, _) :-                    % wrapper failed
    halt.
