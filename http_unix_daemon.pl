:- module(http_unix_daemon,
	  [ http_daemon/0,
	    http_daemon/1			% +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(uid)).
:- use_module(library(unix)).
:- use_module(library(syslog)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- multifile
	http_server_hook/1.			% +Options

/** <module> Run SWI-Prolog HTTP server as a Unix system daemon

This module provides the logic that  is   needed  to integrate a process
into the Unix service (daemon) architecture. It deals with the following
aspects,  all  of  which  may  be   used/ignored  and  configured  using
commandline options:

  - Select the port of the server
  - Run the startup of the process as root to perform priviledged
    tasks and the server itself as unpriviledged user, for example
    to open ports below 1000.
  - Fork and detach from the controlling terminal
  - Handle console and debug output using a file and/or the syslog
    daemon.
  - Manage a _|pid file|_

The typical use scenario is to  write   a  file that loads the following
components:

  1. The application code, including http handlers (see http_handler/3).
  2. This library
  3. Use an initialization directive to start http_daemon/0

In the code below, =|load|= loads the remainder of the webserver code.

  ==
  :- use_module(library(http/http_unix_daemon)).
  :- initialization http_daemon.

  :- [load].
  ==

Now,  the  server  may  be  started    using   the  command  below.  See
http_daemon/0 for supported options.

  ==
  % [sudo] swipl -s mainfile.pl -- [option ...]
  ==

Below are some examples. Our first example is completely silent, running
on port 80 as user =www=.

  ==
  % swipl -s mainfile.pl -- --user=www --pidfile=/var/run/http.pid
  ==

Our second example logs HTTP  interaction   with  the  syslog daemon for
debugging purposes. Note that the argument   to =|--debug|== is a Prolog
term and must often be escaped to   avoid  misinterpretation by the Unix
shell.   The debug option can be repeated to log multiple debug topics.

  ==
  % swipl -s mainfile.pl -- --user=www --pidfile=/var/run/http.pid \
	  --debug='http(request)' --syslog=http
  ==

*Broadcasting* The library uses  broadcast/1   to  allow hooking certain
events:

  - http(pre_server_start)
  Run _after_ _fork_, just before starting the HTTP server.  Can be used
  to load additional files or perform additional initialisation, such as
  starting additional threads.  Recall that it is not possible to start
  threads _before_ forking.

  - http(post_server_start)
  Run _after_ starting the HTTP server.

@tbd	Make it work with SSL
@see	The file <swi-home>/doc/packages/examples/http/debian-init-script
	provides a /etc/init.d script for controlling a server as a normal
	Unix service.
*/

:- debug(daemon).

%%	http_daemon
%
%	Start the HTTP server  as  a   daemon  process.  This  predicate
%	processes the following commandline arguments:
%
%	  $ --port=Port :
%	  Start HTTP server at Port. It requires root permission and the
%	  option =|--user=User|= to open ports below 1000.  The default
%	  port is 80.
%
%	  $ --ip=IP :
%	  Only listen to the given IP address.  Typically used as
%	  =|--ip=localhost|= to restrict access to connections from
%	  _localhost_ if the server itself is behind an (Apache)
%	  proxy server running on the same host.
%
%	  $ --debug=Topic :
%	  Enable debugging Topic.  See debug/3.
%
%	  $ --syslog=Ident :
%	  Write debug messages to the syslog daemon using Ident
%
%	  $ --user=User :
%	  When started as root to open a port below 1000, this option
%	  must be provided to switch to the target user. Three actions
%	  are performed as user: open the socket, write the pidfile and
%	  setup syslog interaction.
%
%	  $ --group=Group :
%	  May be used in addition to =|--user|=.  If omitted, the login
%	  group of the target user is used.
%
%	  $ --pidfile=File :
%	  Write the PID of the daemon process to File.
%
%	  $ --output=File :
%	  Send output of the process to File.  By default, all
%	  Prolog console output` is discarded.
%
%	  $ --fork[=Bool] :
%	  If given as =|--no-fork|= or =|--fork=false|=, the process
%	  runs in the foreground.
%
%	  $ --gtrace=[Bool] :
%	  Use the debugger to trace http_daemon/1.
%
%	Other options are converted  by   argv_options/3  and  passed to
%	http_server/1.  For example, this allows for:
%
%	  $ --workers=Count :
%	  Set the number of workers for the multi-threaded server.

http_daemon :-
	catch(http_daemon_2, E,
	      (	  print_message(error, E),
		  halt(1)
	      )).

http_daemon_2 :-
	current_prolog_flag(argv, Argv),
	argv_options(Argv, _RestArgv, Options),
	(   option(gtrace(true), Options)
	->  gtrace
	;   true
	),
	http_daemon(Options).

%%	argv_options(+Argv, -RestArgv, -Options) is det.
%
%	Generic transformation of long commandline arguments to options.
%	Each --Name=Value is mapped to Name(Value).   Each plain name is
%	mapped to Name(true), unless Name starts  with =|no-|=, in which
%	case the option is mapped to   Name(false).  Numeric options are
%	mapped to Prolog numbers.

argv_options([], [], []).
argv_options([H0|T0], R, [H|T]) :-
	sub_atom(H0, 0, _, _, --), !,
	(   sub_atom(H0, B, _, A, =)
	->  B2 is B-2,
	    sub_atom(H0, 2, B2, _, Name),
	    sub_atom(H0, _, A,  0, Value0),
	    (	atom_number(Value0, Number)
	    ->	Value = Number
	    ;	Value = Value0
	    )
	;   sub_atom(H0, 2, _, 0, Name0),
	    (	sub_atom(Name0, 0, _, _, 'no-')
	    ->	sub_atom(Name0, 3, _, 0, Name),
		Value = false
	    ;	Name = Name0,
		Value = true
	    )
	),
	H =.. [Name,Value],
	argv_options(T0, R, T).
argv_options([H|T0], [H|R], T) :-
	argv_options(T0, R, T).


%%	http_daemon(+Options)
%
%	Helper that is started from http_daemon/0. See http_daemon/0 for
%	options that are processed.

http_daemon(Options) :-
	setup_debug(Options),
	option(port(Port), Options, 80),
	merge_options([port(Port)], Options, Options1),
	make_socket(Options1, Socket),
	debug(daemon(socket),
	      'Created socket ~q, listening to port ~q', [Socket, Port]),
	(   option(fork(true), Options1, true)
	->  fork(Who),
	    (   Who \== child
	    ->  halt
	    ;   disable_development_system,
	        setup_syslog(Options1),
		write_pid(Options1),
		setup_output(Options1),
	        switch_user(Options1),
		setup_signals,
		start_server([tcp_socket(Socket)|Options1]),
		wait
	    )
	;   write_pid(Options1),
	    switch_user(Options1),
	    start_server([tcp_socket(Socket)|Options1])
	).

%%	start_server(+Options) is det.
%
%	Start the HTTP server.  It performs the following steps:
%
%	  1. Call broadcast(http(pre_server_start))
%	  2. Call http_server(http_dispatch, Options)
%	  2. Call broadcast(http(post_server_start))
%
%	This predicate can be  hooked   using  http_server_hook/1.  This
%	predicate is executed after
%
%	  - Forking
%	  - Setting I/O (e.g., to talk to the syslog daemon)
%	  - Dropping root privileges (--user)
%	  - Setting up signal handling

start_server(Options) :-
	http_server_hook(Options), !.
start_server(Options) :-
	broadcast(http(pre_server_start)),
	http_server(http_dispatch, Options),
	option(port(Port), Options),
	debug(daemon, 'Started server at port ~w', [Port]),
	broadcast(http(post_server_start)).

make_socket(Options, Socket) :-
	option(port(Port), Options),
	(   option(ip(IP), Options)
	->  Address = IP:Port
	;   Address = Port
	),
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Address),
	tcp_listen(Socket, 5).

%%	disable_development_system
%
%	Disable some development stuff.

disable_development_system :-
	set_prolog_flag(editor, '/bin/false').

%%	setup_syslog(+Options) is det.
%
%	Setup syslog interaction.

setup_syslog(Options) :-
	option(syslog(Ident), Options), !,
	openlog(Ident, [pid], user).
setup_syslog(_).


%%	setup_output(+Options) is det.
%
%	Setup output from the daemon process. The default is to send all
%	output to a  null-stream  (see   open_null_stream/1).  With  the
%	option output(File), all output is written to File.

setup_output(Options) :-
	option(output(File), Options), !,
	open(File, write, Out, [encoding(utf8)]),
	set_stream(Out, buffer(line)),
	detach_IO(Out).
setup_output(_) :-
	open_null_stream(Out),
	detach_IO(Out).


%%	write_pid(+Options) is det.
%
%	If the option pidfile(File) is  present,   write  the PID of the
%	daemon to this file.

write_pid(Options) :-
	option(pidfile(File), Options),
	current_prolog_flag(pid, PID), !,
	setup_call_cleanup(
	    open(File, write, Out),
	    format(Out, '~d~n', [PID]),
	    close(Out)),
	at_halt(catch(delete_file(File), _, true)).
write_pid(_).


%%	switch_user(+Options) is det.
%
%	Switch to the target user and group. If the server is started as
%	root, this option *must* be present.

switch_user(Options) :-
	option(user(User), Options), !,
	(   option(group(Group), Options)
	->  set_user_and_group(User, Group)
	;   set_user_and_group(User)
	).
switch_user(Options) :-
	geteuid(0), !,
	option(port(Port), Options),
	throw(error(permission_error(open, http_server, Port),
		    context('Refusing to run HTTP server as root', _))).
switch_user(_).

%%	setup_debug(+Options) is det.
%
%	Initialse debug/3 topics. The  =|--debug|=   option  may be used
%	multiple times.

setup_debug([]).
setup_debug([debug(Topic)|T]) :- !,
	atom_to_term(Topic, Term, _),
	debug(Term),
	setup_debug(T).
setup_debug([_|T]) :-
	setup_debug(T).

%%	setup_signals
%
%	Kill the server on SIGINT, SIGHUP and SIGTERM.

setup_signals :-
	on_signal(int,  _, quit),
	on_signal(hup,  _, quit),
	on_signal(term, _, quit).

quit(Signal) :-
	debug(daemon, 'Dying on signal ~w', [Signal]),
	thread_send_message(main, quit).

%%	wait
%
%	This predicate runs in the main   thread,  waiting for a message
%	from quit/0.

wait :-
	repeat,
	thread_get_message(Msg),
	Msg == quit,
	halt(0).

		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	http_server_hook(+Options) is semidet.
%
%	Hook that is called to start the  HTTP server. This hook must be
%	compatible to http_server(Handler,  Options).   The  default  is
%	provided by start_server/1.
