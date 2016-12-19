:- module(demo_digest,
          [ run/1
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_digest)).

:- http_handler(/, home, [authentication(digest(passwd, 'Bob/secret'))]).

/** <module> Demo HTTP digest authentication

Run this demo. Point your browser at the reported URL and login with the
user "Bob" and password "secret". The dumped  HTTP request should have a
field user('Bob').
*/

run(Port) :-
    http_server(http_dispatch, [port(Port)]).

home(Request) :-
    format('Content-type: text/plain~n~n'),
    print_term(Request, [output(current_output)]).



