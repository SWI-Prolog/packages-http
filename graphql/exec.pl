/*  Part of SWI-Prolog

    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions B.V.
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

:- module(graphql_exec,
          [ graphql_execute_document/4    % +URI, +Document, -Result, +Options
          ]).


:- use_module(write).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

/** <module> GraphQL Execution

This module provides the main entry point for requesting execution of
a GraphQL document on a remote GraphQL endpoint.

*/

%! graphql_execute_document(+URI, +Document, -Result, +Options) is det.
%
%  Send GraphQL document Document to be executed by the GraphQL
%  endpoint at URI.
%
%  Document is a Prolog term representing the abstract syntax tree of
%  the GraphQL document, as obtained from e.g. graphql_read_document/3
%  or graphql/4 quasi-quotation.  Result is unified with a dict
%  representing the JSON formatted response received from the server.
%
%  The following example shows how graphql_execute_document/4 can be
%  used to expose a simple GraphQL interface to Prolog:
%
%  ```prolog
%  sourcehut_repository_description(Owner, Repo, Desc) :-
%      graphql_execute_document("https://git.sr.ht/query",
%                               {| graphql(Owner, Repo) ||
%                                  { user(username: <Owner>)
%                                    { repository(name: <Repo>)
%                                      { description } } } |},
%                               Dict,
%                               [token(...)]),
%      Desc = Dict.get(data/user/repository/description).
%
%
%  ?- sourcehut_repository_description("eshel", "sourcehut.pl", Desc).
%  Desc = "SWI-Prolog package implementing a SourceHut GraphQL API client.".
%  ```
%
%  Options is a list whose elemenets are one of the following:
%  - token(+Token)
%    Token is string that will be used as an OAuth2.0 token.
%  - variables(+Variables)
%    Variables is a dict with keys corresponding to GraphQL
%    variables that occur in Document as `$key`.
%    Variables is sent to the remote to the GraphQL endpoint in
%    JSON format for server-side interpolation.
%    For more information about GraphQL variables, see
%    https://spec.graphql.org/draft/#sec-Language.


:- predicate_options(graphql_execute_document/4, 4,
                     [variables(list),
                      data(list),
                      pass_to(graphql_auth_token/3, 3),
                      pass_to(graphql_document_to_string/3, 3)]).


graphql_execute_document(URI,
                         Document,
                         Result,
                         Options) :-
    option(variables(Variables), Options, null),
    option(data(Data), Options, [map=null]),
    graphql_auth_token(URI, Token, Options),
    graphql_document_to_string(Document, Text, Options),
    atom_json_dict(Operations,
                   _{query: Text, variables: Variables},
                   []),
    http_post(URI,
              form_data([operations=Operations|Data]),
              Result,
              [json_object(dict), authorization(bearer(Token))]).


:- predicate_options(graphql_auth_token/3, 3,
                     [token(string)]).


graphql_auth_token(_URI, Token, Options) :-
    option(token(Token), Options),
    !.
graphql_auth_token(URI, Token, _Options) :-
    graphql_auth_token_hook(URI, Token),
    !.


%! graphql_auth_token_hook(+URI, -Token) is nondet.
%
%  Multifile, dynamic hook. graphql_execute_document/4 consults this
%  hook to set authorization token for the GraphQL endpoint at URI.
:- multifile graphql_auth_token_hook/2.
:- dynamic   graphql_auth_token_hook/2.
