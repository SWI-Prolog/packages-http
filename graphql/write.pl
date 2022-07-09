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

:- module(graphql_write,
          [ graphql_document_to_string/3, % +Document, -String, +Options
            graphql_document_to_codes/3   % +Document, -Codes, +Options
          ]).

/** <module> GraphQL to text conversion

This module provides an interface for serializing the Prolog
representation of GraphQL documents into textual format for emission
over the wire.

*/


%! graphql_document_to_string(+Document, -String, +Options) is det.
%
%  Serialize the GraphQL document Document and unify String with the
%  resulting string.
%
%  Options are passed on to graphql_document_to_codes/3.

:- predicate_options(graphql_document_to_string/3, 3,
                     [pass_to(graphql_document_to_codes/3, 3)]).

graphql_document_to_string(Document, String, Options) :-
    graphql_document_to_codes(Document, Codes, Options),
    string_codes(String, Codes).


%! graphql_document_to_codes(+Document, -Codes, +Options) is det.
%
%  Serialize Document, a Prolog term representing a GraphQL document
%  as obtained from graphql_read_document/3 or the graphql/4
%  quasi-quotation, and unify Codes with the resulting list of
%  character codes.
%
%  Options are a list whose elements are one of:
%    - separator(+Sep)
%      Sep is a list of codes to be used for separating adjancent
%      GraphQL values in Codes. Defaults to `[32]` (a single space).
%      This option can be used to separate values with commas
%      (which are optional throughout GraphQL) by passing
%      e.g. `separator([44, 32])`.

:- predicate_options(graphql_document_to_codes/3, 3,
                     [separator(list)]).

graphql_document_to_codes(Document, Codes, Options) :-
    phrase(graphql_write_document(Document, Options), Codes).


graphql_write_document([H|T], Options) -->
    graphql_write_definition(H, Options),
    graphql_write_document(T, Options).
graphql_write_document([], _Options) --> [], !.


graphql_write_definition(operation(Type,
                                   Name,
                                   VariableDefinitions,
                                   Directives,
                                   SelectionSet), Options) -->
    graphql_write_name(Type, Options),
    graphql_write_name_maybe(Name, Options),
    graphql_write_variable_definitions(VariableDefinitions, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_name(Name, _Options) -->
    {   string_codes(Name, Codes)   },
    Codes.


graphql_write_name_maybe(null, _Options) --> [], !.
graphql_write_name_maybe(Name, Options) -->
    graphql_write_separator(Options),
    graphql_write_name(Name, Options).


graphql_write_variable_definitions([   ], _Options) --> [], !.
graphql_write_variable_definitions([H|T], Options) -->
    [40],
    graphql_write_variable_definition(H, Options),
    graphql_write_variable_definitions_(T, Options),
    [41].


graphql_write_variable_definitions_([   ], _Options) --> [], !.
graphql_write_variable_definitions_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_variable_definition(H, Options),
    graphql_write_variable_definitions_(T, Options).


graphql_write_variable_definition(variable_definition(Name,
                                                      Type,
                                                      Default,
                                                      Directives),
                                  Options) -->
    `$`,
    graphql_write_name(Name, Options),
    `:`,
    graphql_write_type(Type, Options),
    graphql_write_value_maybe(Default, Options),
    graphql_write_directives(Directives, Options).


graphql_write_value_maybe(null, _Options) -->
    !,
    [].
graphql_write_value_maybe(Value, Options) -->
    graphql_write_separator(Options),
    graphql_write_value(Value, Options).

graphql_write_value(enum(N), _Options) -->
    !,
    {   string_codes(N, Codes)    },
    Codes.
graphql_write_value(variable(V), _Options) -->
    !,
    {   string_codes(V, Codes)    },
    [0'$|Codes].
graphql_write_value(Atom, _Options) -->
    {   atom(Atom),
        !,
        atom_codes(Atom, Codes)
    },
    Codes.
graphql_write_value(String, Options) -->
    {   string(String),
        !,
        string_codes(String, Codes)
    },
    [34],
    graphql_write_string(Codes, Options),
    [34].
graphql_write_value(Number, _Options) -->
    {   number(Number),
        !,
        number_codes(Number, Codes)
    },
    Codes.
graphql_write_value(List, Options) -->
    {   is_list(List)   },
    !,
    [91],
    graphql_write_list_value(List, Options),
    [93].
graphql_write_value(Dict, Options) -->
    {   is_dict(Dict),
        !,
        dict_pairs(Dict, _, Object)
    },
    [123],
    graphql_write_pairs(Object, Options),
    [125].


graphql_write_pairs([H|T], Options) -->
    !,
    graphql_write_pair(H, Options),
    graphql_write_pairs_(T, Options).
graphql_write_pairs([], _) --> [].


graphql_write_pairs_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_pair(H, Options),
    graphql_write_pairs_(T, Options).
graphql_write_pairs_([], _) --> [].


graphql_write_pair(N-V, Options) -->
    graphql_write_name(N, Options),
    [58],
    graphql_write_value(V, Options).


graphql_write_string([], _Options) --> !, [].
graphql_write_string([34|T], Options) -->
    !,
    [92, 34],
    graphql_write_string(T, Options).
graphql_write_string([92|T], Options) -->
    !,
    [92, 92],
    graphql_write_string(T, Options).
graphql_write_string([H|T], Options) -->
    [H],
    graphql_write_string(T, Options).


graphql_write_list_value([], _Options) --> !, [].
graphql_write_list_value([H|T], Options) -->
    graphql_write_value(H, Options),
    graphql_write_list_value_(T, Options).


graphql_write_list_value_([], _Options) --> !, [].
graphql_write_list_value_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_value(H, Options),
    graphql_write_list_value_(T, Options).


graphql_write_type(non_null_type(Type), Options) -->
    !,
    graphql_write_type(Type, Options),
    [33].
graphql_write_type(named_type(Name), Options) -->
    !,
    graphql_write_name(Name, Options).
graphql_write_type(list_type(Type), Options) -->
    [91],
    graphql_write_type(Type, Options),
    [93].


graphql_write_directives([   ], _Options) --> [], !.
graphql_write_directives([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_directive(H, Options),
    graphql_write_directives(T, Options).


graphql_write_directive(Name-Arguments, Options) -->
    graphql_write_name(Name, Options),
    graphql_write_arguments(Arguments, Options).


graphql_write_arguments(_{}, _Options) --> !, [].
graphql_write_arguments(Args, Options) -->
    {   dict_pairs(Args, _, Pairs)   },
    [40],
    graphql_write_pairs(Pairs, Options),
    [41].


graphql_write_selection_set([   ], _Options) --> [], !.
graphql_write_selection_set([H|T], Options) -->
    [123],
    graphql_write_selection(H, Options),
    graphql_write_selection_set_(T, Options),
    [125].


graphql_write_selection_set_([   ], _Options) --> [], !.
graphql_write_selection_set_([H|T], Options) -->
    graphql_write_separator(Options),
    graphql_write_selection(H, Options),
    graphql_write_selection_set_(T, Options).


graphql_write_selection(field(Alias,
                              Name,
                              Args,
                              Directives,
                              SelectionSet),
                       Options) -->
    graphql_write_field(Alias,
                        Name,
                        Args,
                        Directives,
                        SelectionSet,
                        Options).
graphql_write_selection(fragment_spread(Name, Directives),
                        Options) -->
    graphql_write_fragment_spread(Name, Directives, Options).
graphql_write_selection(inline_fragment(Type,
                                        Directives,
                                        SelectionSet),
                        Options) -->
    graphql_write_inline_fragment(Type,
                                  Directives,
                                  SelectionSet,
                                  Options).


graphql_write_field(null,
                    Name,
                    Arguments,
                    Directives,
                    SelectionSet,
                    Options) -->
    !,
    graphql_write_field_(Name,
                         Arguments,
                         Directives,
                         SelectionSet,
                         Options).
graphql_write_field(Alias,
                    Name,
                    Arguments,
                    Directives,
                    SelectionSet,
                    Options) -->
    graphql_write_name(Alias, Options),
    [58, 32],
    graphql_write_field_(Name,
                         Arguments,
                         Directives,
                         SelectionSet,
                         Options).


graphql_write_field_(Name,
                     Arguments,
                     Directives,
                     SelectionSet,
                     Options) -->
    graphql_write_name(Name, Options),
    graphql_write_arguments(Arguments, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_directives_and_selection_set(Directives,
                                           SelectionSet,
                                           Options) -->
    graphql_write_directives(Directives, Options),
    graphql_write_selection_set(SelectionSet, Options).


graphql_write_separator(Options) -->
    {   option(separator(Sep), Options, [32])   },
    Sep.


graphql_write_fragment_spread(Name, Directives, Options) -->
    `...`,
    graphql_write_separator(Options),
    graphql_write_name(Name, Options),
    graphql_write_directives(Directives, Options).


graphql_write_inline_fragment(TypeCondition,
                              Directives,
                              SelectionSet,
                              Options) -->
    `...`,
    graphql_write_separator(Options),
    graphql_write_type_condition(TypeCondition, Options),
    graphql_write_directives_and_selection_set(Directives,
                                               SelectionSet,
                                               Options).


graphql_write_type_condition(TypeCondition, Options) -->
    `on `, graphql_write_name(TypeCondition, Options).
