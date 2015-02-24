/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(hub,
	  [ hub_create/3,		% +HubName, -Hub, +Options
	    hub_add/3,			% +HubName, +Websocket, ?Id
	    hub_send/2,			% +ClientId, +Message
	    hub_broadcast/2,		% +HubName, +Message
	    current_hub/2		% ?HubName, ?Hub
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(gensym)).
:- use_module(library(aggregate)).
:- use_module(library(uuid)).
:- use_module(library(ordsets)).
:- use_module(library(http/websocket)).

/** <module> Manage a hub for websockets

This library manages a hub that consists   of clients that are connected
using a websocket. Messages arriving at any   of the websockets are sent
to the _event_ queue  of  the  hub.   In  addition,  the  hub provides a
_broadcast_ interface. A typical usage scenario  for   a  hub is a _chat
server_ A scenario for realizing an chat server is:

  1. Create a new hub using hub_create/3.
  2. Create one or more threads that listen to Hub.queues.event from
     the created hub.  These threads can update the shared view of the
     world. A message is a dict as returned by ws_receive/2 or a
     hub control message. Currently, the following control messages
     are defined:

       - hub{left:ClientId, reason:Reason, error:Error}
       A client left us because of an I/O error.  Reason is =read=
       or =write= and Error is the Prolog I/O exception.

       - hub{joined:ClientId}
       A new client has joined the chatroom.

     The thread(s) can talk to clients using two predicates:

       - hub_send/2 sends a message to a specific client
       - hub_broadcast/2 sends a message to all clients of the
         hub.

A hub consists of (currenty) four message   queues  and a simple dynamic
fact. Threads that are needed for the communication tasks are created on
demand and die if no more work needs to be done.

@tbd	The current design does not use threads to perform tasks for
	multiple hubs.  This implies that the design scales rather
	poorly for hosting many hubs with few users.
*/

:- dynamic
	hub/2,				% Hub, Queues ...
	websocket/5.			% Hub, Socket, Queue, Lock, Id

%%	hub_create(+Name, -Hub, +Options) is det.
%
%	Create a new hub. Hub is a  dict containing the following public
%	information:
%
%	  - Hub.name
%	    The name of the hub (the Name argument)
%	  - queues.event
%	    Message queue to which the hub thread(s) can listen.
%
%	After creating a hub, the application  normally creates a thread
%	that listens to Hub.queues.event and  exposes some mechanisms to
%	establish websockets and add them to the hub using hub_add/3.
%
%	@see	http_upgrade_to_websocket/3 establishes a websocket from
%		the SWI-Prolog webserver.

hub_create(HubName, Hub, _Options) :-
	must_be(atom, HubName),
	message_queue_create(WaitQueue),
	message_queue_create(ReadyQueue),
	message_queue_create(EventQueue),
	message_queue_create(BroadcastQueue),
	Hub = hub{name:HubName,
		  queues:_{wait:WaitQueue,
			   ready:ReadyQueue,
			   event:EventQueue,
			   broadcast:BroadcastQueue
			  }},
	assertz(hub(HubName, Hub)).


%%	current_hub(?Name, ?Hub) is nondet.
%
%	True when there exists a hub Hub with Name.

current_hub(HubName, Hub) :-
	hub(HubName, Hub).


		 /*******************************
		 *	      WAITERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of this layer is to wait   for  (a potentially large number of)
websockets. Whenever there is data on one   of these sockets, the socket
is handed to Hub.queues.ready. This is realised using wait_for_input/3,
which allows a single thread  to  wait   for  many  sockets.  But ... on
Windows it allows to wait for at most  64 sockets. In addition, there is
no way to add an additional input   for control messages because Windows
select() can only wait for sockets. On Unix   we could use pipe/2 to add
the control channal. On Windows  we   would  need  an additional network
service, giving rise its own  problems   with  allocation, firewalls and
security.

So, instead we keep a queue of websockets   that  need to be waited for.
Whenever we add a  websocket,  we  create   a  waiter  thread  that will
typically start waiting for this socket.   In  addition, we schedule any
waiting thread that has less  than  the   maximum  number  of sockets to
timeout at as good as we can the same   time.  All of them will hunt for
the same set of queues,  but  they  have   to  wait  for  each other and
therefore most of the time one thread will walk away with all websockets
and the others commit suicide because there is nothing to wait for.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate
	hub_thread(0, +, +).

%%	hub_add(+Hub, +WebSocket, ?Id) is det.
%
%	Add a WebSocket to the hub.  Id   is  used to identify this
%	user. It may be provided (as a ground term) or is generated as a
%	UUID.

hub_add(HubName, WebSocket, Id) :-
	must_be(atom, HubName),
	hub(HubName, Hub),
	(   var(Id)
	->  uuid(Id)
	;   true
	),
	message_queue_create(OutputQueue),
	mutex_create(Lock),
	assertz(websocket(HubName, WebSocket, OutputQueue, Lock, Id)),
	thread_send_message(Hub.queues.wait, WebSocket),
	thread_send_message(Hub.queues.event,
			    hub{joined:Id}),
	debug(hub(gate), 'Joined ~w: ~w', [HubName, Id]),
	create_wait_thread(Hub).

create_wait_thread(Hub) :-
	hub_thread(wait_for_sockets(Hub), Hub, hub_wait_).

wait_for_sockets(Hub) :-
	wait_for_sockets(Hub, 64).

wait_for_sockets(Hub, Max) :-
	Queues = Hub.queues,
	repeat,
	  get_messages(Queues.wait, Max, List),
	  (   List \== []
	  ->  create_new_waiter_if_needed(Hub),
	      sort(List, Set),
	      length(Set, Len),
	      wait_timeout(List, Max, Timeout),
	      debug(hub(wait),
		    'Waiting for ~d queues for ~w sec', [Len, Timeout]),
	      wait_for_input(Set, ReadySet, Timeout),
	      (	  ReadySet \== []
	      ->  debug(hub(ready), 'Data on ~p', [ReadySet]),
		  Ready = Queues.ready,
		  maplist(thread_send_message(Ready), ReadySet),
		  create_reader_threads(Hub),
		  ord_subtract(Set, ReadySet, NotReadySet)
	      ;	  NotReadySet = Set		% timeout
	      ),
	      debug(hub(wait), 'Re-scheduling: ~p', [NotReadySet]),
	      Wait = Queues.wait,
	      maplist(thread_send_message(Wait), NotReadySet),
	      fail
	  ;   !
	  ).

create_new_waiter_if_needed(Hub) :-
	message_queue_property(Hub.queues.wait, size(0)), !.
create_new_waiter_if_needed(Hub) :-
	create_wait_thread(Hub).

%%	wait_timeout(+WaitForList, +Max, -TimeOut) is det.
%
%	Determine the timeout, such that   multiple  threads waiting for
%	less than the maximum number of  sockets   time  out at the same
%	moment and we can combine them on a single thread.

:- dynamic
	scheduled_timeout/1.

wait_timeout(List, Max, Timeout) :-
	length(List, Max), !,
	Timeout = infinite.
wait_timeout(_, _, Timeout) :-
	get_time(Now),
	(   scheduled_timeout(SchedAt)
	->  (   SchedAt > Now
	    ->	At = SchedAt
	    ;	retractall(scheduled_timeout(_)),
		At is ceiling(Now) + 1,
		asserta(scheduled_timeout(At))
	    )
	;   At is ceiling(Now) + 1,
	    asserta(scheduled_timeout(At))
	),
	Timeout is At - Now.


%%	get_messages(+Queue, +Max, -List) is det.
%
%	Get the next Max messages from  Queue   or  as many as there are
%	available without blocking very long.   This routine is designed
%	such that if multiple threads are running for messages, one gets
%	all of them and the others nothing.

get_messages(Q, N, List) :-
	with_mutex(hub_wait,
		   get_messages_sync(Q, N, List)).

get_messages_sync(Q, N, [H|T]) :-
	succ(N2, N),
	thread_get_message(Q, H, [timeout(0.01)]), !,
	get_messages_sync(Q, N2, T).
get_messages_sync(_, _, []).


		 /*******************************
		 *	      READERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The next layer consists of `readers'.   Whenever  one or more websockets
have   data,   the   socket   is    added   to   Hub.queues.ready   and
create_reader_threads/1 is called. This  examines   the  number of ready
sockets and fires a number  of  threads   to  handle  the read requests.
Multiple threads are mainly needed for the case that a client signals to
be  ready,  but  only  provides  an   incomplete  message,  causing  the
ws_receive/2 to block.

Each  of  the  threads  reads  the  next   message  and  sends  this  to
Hub.queues.event. The websocket is then rescheduled   to listen for new
events. This read either fires a thread   to  listen for the new waiting
socket using create_wait_thread/1 or, if there   are no more websockets,
does this job itself. This  deals  with   the  common  scenario that one
client wakes up, starts a thread to  read   its  event and waits for new
messages on the same websockets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

create_reader_threads(Hub) :-
	message_queue_property(Hub.queues.ready, size(Ready)),
	Threads is ceiling(sqrt(Ready)),
	forall(between(1, Threads, _),
	       create_reader_thread(Hub)).

create_reader_thread(Hub) :-
	hub_thread(read_message(Hub), Hub, hub_read_ws_).

read_message(Hub) :-
	Queues = Hub.queues,
	thread_get_message(Queues.ready, WS, [timeout(0)]), !,
	catch(ws_receive(WS, Message), Error, true),
	(   var(Error),
	    websocket(HubName, WS, _, _, Id)
	->  (   _{opcode:close, data:end_of_file} :< Message
	    ->	eof(WS)
	    ;	Event = Message.put(_{client:Id, hub:HubName}),
		debug(hub(event), 'Event: ~p', [Event]),
		thread_send_message(Queues.event, Event),
		thread_send_message(Queues.wait, WS),
		(   message_queue_property(Queues.ready, size(0))
		->  !,
		    wait_for_sockets(Hub)
		;   create_wait_thread(Hub),
		    read_message(Hub)
		)
	    )
	;   websocket(_, WS, _, _, _)
	->  io_error(WS, read, Error),
	    read_message(Hub)
	;   read_message(Hub)			% already destroyed
	).
read_message(_).


%%	io_error(+WebSocket, +ReadWrite, +Error)
%
%	Called on a read or  write  error   to  WebSocket.  We close the
%	websocket and send the hub an event  that we lost the connection
%	to the specified client. Note that   we leave destruction of the
%	anonymous  message  queue  and  mutex   to  the  Prolog  garbage
%	collector.

io_error(WebSocket, RW, Error) :-
	debug(hub(gate), 'Got ~w error on ~w: ~p',
	      [RW, WebSocket, Error]),
	retract(websocket(HubName, WebSocket, _Queue, _Lock, Id)), !,
	catch(ws_close(WebSocket, 1011, Error), E,
	      print_message(warning, E)),
	hub(HubName, Hub),
	thread_send_message(Hub.queues.event,
			    hub{left:Id,
				     hub:HubName,
				     reason:RW,
				     error:Error}).
io_error(_, _, _).			% already considered gone

eof(WebSocket) :-
	io_error(WebSocket, read, end_of_file).


		 /*******************************
		 *	  SENDING MESSAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
My  initial  thought  about  sending  messages    was  to  add  a  tuple
WebSocket-Message to an output  queue  and   have  a  dynamic  number of
threads sending these messages to the   websockets. But, it is desirable
that, if multiple messages are sent to  a particular client, they arrive
in this order. As multiple threads are performing this task, this is not
easy to guarantee. Therefore, we create an  output queue and a mutex for
each client. An output thread will   walk  along the websockets, looking
for one that has pending messages.  It   then  grabs the lock associated
with the client and sends all waiting output messages.

The price is that we might peek   a significant number of message queues
before we find one that  contains  messages.   If  this  proves  to be a
significant  problem,  we  could  mantain  a  queue  of  queues  holding
messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	hub_send(+ClientId, +Message) is det.
%
%	Send message to the indicated ClientId.
%
%	@arg	Message is either a single message (as accepted by
%		ws_send/2) or a list of such messages.

hub_send(ClientId, Message) :-
	websocket(HubName, _WS, ClientQueue, _Lock, ClientId),
	hub(HubName, Hub),
	(   is_list(Message)
	->  maplist(queue_output(ClientQueue), Message)
	;   queue_output(ClientQueue, Message)
	),
	create_output_thread(Hub, ClientQueue).

create_output_thread(Hub, Queue) :-
	hub_thread(broadcast_from_queue(Queue, [timeout(0)]),
			Hub, hub_out_q_).

%%	hub_broadcast(+Hub, +Message) is det.
%
%	Send Message to all websockets associated   with  Hub. Note that
%	this  process  is   _asynchronous_:    this   predicate  returns
%	immediately after putting all requests in  a broadcast queue. If
%	a message cannot be delivered due to a network error, the hub is
%	informed through io_error/3.

hub_broadcast(HubName, Message) :-
	must_be(atom, HubName),
	hub(HubName, Hub),
	forall(websocket(HubName, _WS, ClientQueue, _Lock, _Id),
	       queue_output(ClientQueue, Message)),
	create_broadcast_threads(Hub).

queue_output(Queue, Message) :-
	thread_send_message(Queue, Message).


create_broadcast_threads(Hub) :-
	aggregate_all(count, websocket(Hub.name, _, _, _, _), Count),
	Threads is ceiling(sqrt(Count)),
	forall(between(1, Threads, _),
	       create_broadcast_thread(Hub)).

create_broadcast_thread(Hub) :-
	hub_thread(broadcast_from_queues(Hub, [timeout(0)]),
			Hub, hub_out_all_).


%%	broadcast_from_queues(+Hub, +Options) is det.
%
%	Broadcast from over all known queues.

broadcast_from_queues(Hub, Options) :-
	forall(websocket(Hub.name, _WebSocket, Queue, _Lock, _Id),
	       broadcast_from_queue(Queue, Options)).


%%	broadcast_from_queue(+Queue, +Options) is det.
%
%	Send all messages pending for Queue.   Note  that this predicate
%	locks the mutex associated  with  the   Queue,  such  that other
%	workers cannot start sending messages to this client. Concurrent
%	sending  would  lead  to  out-of-order    arrival  of  broadcast
%	messages.  If  the  mutex  is  already  held,  someone  else  is
%	processing this message queue, so we don't have to worry.

broadcast_from_queue(Queue, _Options) :-
	message_queue_property(Queue, size(0)), !.
broadcast_from_queue(Queue, Options) :-
	websocket(_Hub, _WebSocket, Queue, Lock, _Id), !,
	(   setup_call_cleanup(
		mutex_trylock(Lock),
		broadcast_from_queue_sync(Queue, Options),
		mutex_unlock(Lock))
	->  true
	;   true
	).
broadcast_from_queue(_, _).

% Note that we re-fetch websocket/5, such that we terminate if something
% closed the websocket.

broadcast_from_queue_sync(Queue, Options) :-
	repeat,
	  (   websocket(_Hub, WebSocket, Queue, _Lock, _Id),
	      thread_get_message(Queue, Message, Options)
	  ->  debug(hub(broadcast),
		    'To: ~p messages: ~p', [WebSocket, Message]),
	      catch(ws_send(WebSocket, Message), E,
		    io_error(WebSocket, write, E)),
	      fail
	  ;   !
	  ).

%%	hub_thread(:Goal, +Hub, +Task) is det.
%
%	Create a (temporary) thread for the hub to perform Task. We
%	created named threads if debugging hub(thread) is enabled.

hub_thread(Goal, _, Task) :-
	debugging(hub(thread)), !,
	gensym(Task, Alias),
	thread_create(Goal, _, [detached(true), alias(Alias)]).
hub_thread(Goal, _, _) :-
	thread_create(Goal, _, [detached(true)]).
