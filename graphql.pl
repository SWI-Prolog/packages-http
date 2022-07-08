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

:- module(graphql,
          [ graphql_read/2,  % +Source, -Document
            graphql/4        % Quasi-quotation syntax
          ]).


:- use_module(library(quasi_quotations)).
:- use_module(library(dcg/high_order)).


%! graphql(+Content, +Args, -VariableNames, -Result) is det.
%
%  Quasi-quotation syntax for embedding GraphQL in Prolog text.
%  Result is a term representing the given GraphQL document in the
%  same format as used by graphql_read/2.
:- quasi_quotation_syntax(graphql).

graphql(Content, _Args, _VariableNames, Result) :-
    phrase_from_quasi_quotation(graphql_tokens(Tokens), Content),
    phrase(graphql_executable_document(Result), Tokens).


%! graphql_read(+Source, -Document) is semidet.
%
%  True when Document is a term representing the abstract syntax tree
%  obtained from parsing Source as a GraphQL executable document.
%
%  Source can be one of:
%    - codes(+Codes, -Rest)
%      Codes is a list of codes representing the source GraphQL text.
%      Rest is unified with the list of trailing codes starting from
%      the first code that was not recognized as part of a valid
%      GraphQL token.
%    - codes(+Codes)
%      Same as codes(Codes, []).
%    - string(+String)
%      String is a string representing the source GraphQL text.
%    - +Stream
%      Stream is a stream from which the source GraphQL text
%      is read with phrase_from_stream/2.
%
%  Document is a list of

graphql_read(codes(Codes, Rest), Document) =>
    phrase(graphql_tokens(Tokens), Codes, Rest),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read(codes(Codes), Document) =>
    phrase(graphql_tokens(Tokens), Codes),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read(string(String), Document) =>
    string_codes(String, Codes),
    phrase(graphql_tokens(Tokens), Codes),
    phrase(graphql_executable_document(Document), Tokens).
graphql_read(Stream, Document) =>
    phrase_from_stream(graphql_tokens(Tokens), Stream),
    phrase(graphql_executable_document(Document), Tokens).


graphql_executable_document([H|T]) -->
    graphql_executable_definition(H),
    graphql_executable_definitions(T).


graphql_executable_definitions([H|T]) -->
    graphql_executable_definition(H),
    !,
    graphql_executable_definitions(T).
graphql_executable_definitions([]) --> [].


graphql_executable_definition(operation(Type,
                                        Name,
                                        VariableDefinitions,
                                        Directives,
                                        SelectionSet)) -->
    graphql_operation_definition(Type,
                                 Name,
                                 VariableDefinitions,
                                 Directives,
                                 SelectionSet),
    !.
graphql_executable_definition(fragment(Name,
                                       Type,
                                       Directives,
                                       SelectionSet)) -->
    [name("fragment"), name(Name), name("on"), name(Type)],
    graphql_inline_fragment(Directives, SelectionSet).


graphql_operation_definition(T, N, V, D, S) -->
    graphql_operation_type(T),
    !,
    graphql_query(N, V, D, S).
graphql_operation_definition(query, null, null, null, S) -->
    graphql_selection_set(S).


graphql_operation_type(query) -->
    [name("query")],
    !.
graphql_operation_type(mutation) -->
    [name("mutation")],
    !.
graphql_operation_type(subscription) -->
    [name("subscription")],
    !.


graphql_query(N, V, D, S) -->
    optional([name(N)],
             {N=null}),
    optional(graphql_variables_definition(V),
             {V=[]}),
    optional(graphql_directives(D),
             {D=[]}),
    graphql_selection_set(S).


graphql_variables_definition([H|T]) -->
    ['('],
    graphql_variable_definition(H),
    sequence(graphql_variable_definition, T),
    [')'].


graphql_variable_definition(variable_definition(Var, Type, Def, Dirs)) -->
    graphql_variable(Var),
    [':'],
    graphql_type(Type),
    optional(graphql_default_value(Def),
             {Def=null}),
    optional(graphql_directives(Dirs),
             {Dirs=[]}).


graphql_type(named(T, N)) -->
    [name(T)],
    !,
    graphql_non_null(N).
graphql_type(list(T, N)) -->
    ['['],
    !,
    graphql_type(T),
    [']'],
    graphql_non_null(N).


graphql_non_null(true ) --> ['!'], !.
graphql_non_null(false) --> [].


graphql_variable(V) -->
    ['$', name(V)].


graphql_default_value(V) -->
    graphql_value([const(true)], V).


graphql_value(Options, variable(V)) -->
    {   \+ option(const(true), Options)   },
    graphql_variable(V),
    !.
graphql_value(_, integer(N)) -->
    [integer(N)],
    !.
graphql_value(_, float(F)) -->
    [float(F)],
    !.
graphql_value(_, string(S)) -->
    [string(S)],
    !.
graphql_value(_, V) -->
    [name(N)],
    !,
    {   graphql_name_value(N, V)   }.
graphql_value(Options, list(L)) -->
    graphql_list_value(Options, L),
    !.
graphql_value(Options, object(O)) -->
    graphql_object_value(Options, O),
    !.


graphql_name_value("true", true) :- !.
graphql_name_value("false", false) :- !.
graphql_name_value("null", null) :- !.
graphql_name_value(N, enum(N)).


graphql_list_value(Options, L) -->
    ['['],
    sequence(graphql_value(Options), L),
    [']'].


graphql_object_value(Options, O) -->
    ['{'],
    sequence(graphql_object_field(Options), O0),
    {   keysort(O0, O)   },
    ['}'].


graphql_object_field(Options, Name-Value) -->
    [name(Name)],
    graphql_value(Options, Value).


graphql_directives([H|T]) -->
    graphql_directive(H),
    graphql_directives_(T).


graphql_directives_([H|T]) -->
    graphql_directive(H),
    !,
    graphql_directives_(T).
graphql_directives_([]) --> [].


graphql_directive(N-A) -->
    ['@', name(N)],
    optional(graphql_arguments(A),
             {A=[]}).


graphql_arguments(A) -->
    ['('],
    graphql_argument(H),
    sequence(graphql_argument, T),
    {   keysort([H|T], A)   },
    [')'].


graphql_argument(N-V) -->
    [name(N), ':'],
    graphql_value([], V).


graphql_selection_set([H|T]) -->
    ['{'],
    graphql_selection(H),
    sequence(graphql_selection, T),
    ['}'].

graphql_selection(field(A, N, R, D, S)) -->
    graphql_field(A, N, R, D, S),
    !.
graphql_selection(F) -->
    ['...'],
    graphql_selection_(F).


graphql_selection_(F) -->
    [name(N)],
    !,
    graphql_selection__(N, F).
graphql_selection_(inline_fragment(null, D, S)) -->
    graphql_inline_fragment(D, S).


graphql_selection__("on", inline_fragment(T, D, S)) -->
    [name(T)],
    graphql_inline_fragment(D, S).
graphql_selection__(N, fragment_spread(N, D)) -->
    optional(graphql_directives(D),
             {D=[]}).


graphql_inline_fragment(D, S) -->
    optional(graphql_directives(D),
             {D=[]}),
    graphql_selection_set(S).


graphql_field(Alias, Name, Args, Directives, SelectionSet) -->
    [name(Name0)],
    graphql_field_(Name0, Alias, Name, Args, Directives, SelectionSet).


graphql_field_(Alias, Alias, Name, Args, Directives, SelectionSet) -->
    [':'],
    !,
    [name(Name)],
    graphql_field__(Args, Directives, SelectionSet).
graphql_field_(Name, null, Name, Args, Directives, SelectionSet) -->
    graphql_field__(Args, Directives, SelectionSet).


graphql_field__(Args, Directives, SelectionSet) -->
    optional(graphql_arguments(Args),
             {Args=[]}),
    optional(graphql_directives(Directives),
             {Directives=[]}),
    optional(graphql_selection_set(SelectionSet),
             {SelectionSet=[]}).


%! graphql_tokens(-Ts)// is semidet.
graphql_tokens(Ts) --> graphql_ignored, graphql_tokens_(Ts).


graphql_tokens_([H|T]) --> graphql_token(H), !, graphql_tokens(T).
graphql_tokens_([]) --> [].


%! graphql_token(-T)// is semidet.
%
%  https://spec.graphql.org/draft/#Token
graphql_token(P)         --> graphql_punctuator(P).
graphql_token(name(N))   --> graphql_name(N).
graphql_token(N)         --> graphql_numeric_value(N).
graphql_token(string(S)) --> graphql_string_value(S).


%! graphql_ignored// is semidet.
%
%  https://spec.graphql.org/draft/#Ignored
graphql_ignored --> graphql_white_space    , !, graphql_ignored.
graphql_ignored --> graphql_line_terminator, !, graphql_ignored.
graphql_ignored --> graphql_comment        , !, graphql_ignored.
graphql_ignored --> graphql_comma          , !, graphql_ignored.
graphql_ignored --> [].


%! graphql_whitespace// is semidet.
%
%  https://spec.graphql.org/draft/#WhiteSpace
graphql_white_space --> graphql_white_space(_).


graphql_white_space(32) --> ` `, !.
graphql_white_space(9)  --> `\t`.


%! graphql_line_terminator// is semidet.
%
%  https://spec.graphql.org/draft/#LineTerminator
graphql_line_terminator --> `\n`.
graphql_line_terminator --> `\r`.


%! graphql_comment// is semidet.
%
%  https://spec.graphql.org/draft/#Comment
graphql_comment --> `#`, graphql_comment_chars.


%! graphql_comment_chars// is semidet.
graphql_comment_chars --> graphql_comment_char, !, graphql_comment_chars.
graphql_comment_chars --> [].


%! graphql_comment_char// is semidet.
%
%  https://spec.graphql.org/draft/#CommentChar
graphql_comment_char --> graphql_line_terminator, !, { false }.
graphql_comment_char --> [_], !.


%! graphql_commma// is semidet.
%
%  https://spec.graphql.org/draft/#Comma
graphql_comma --> `,`.


%! graphql_punctuator(-P)// is semidet.
%
%  https://spec.graphql.org/draft/#Punctuator
graphql_punctuator('!') --> `!`, !.
graphql_punctuator('$') --> `$`, !.
graphql_punctuator('&') --> `&`, !.
graphql_punctuator('(') --> `(`, !.
graphql_punctuator(')') --> `)`, !.
graphql_punctuator('...') --> `...`, !.
graphql_punctuator(':') --> `:`, !.
graphql_punctuator('=') --> `=`, !.
graphql_punctuator('@') --> `@`, !.
graphql_punctuator('[') --> `[`, !.
graphql_punctuator(']') --> `]`, !.
graphql_punctuator('{') --> `{`, !.
graphql_punctuator('}') --> `}`, !.
graphql_punctuator('|') --> `|`, !.


%! graphql_name(-N)// is semidet.
%
%  https://spec.graphql.org/draft/#Name
graphql_name(N) -->
    graphql_name_start(H),
    graphql_name_(T),
    {   string_codes(N, [H|T])   }.


graphql_name_([H|T]) -->
    graphql_name_continue(H),
    !,
    graphql_name_(T).
graphql_name_([]) --> [].


%! graphql_name_start(-S)// is semidet.
%
%  https://spec.graphql.org/draft/#NameStart
graphql_name_start(L)  --> graphql_letter(L).
graphql_name_start(95) --> [95].


%! graphql_name_continue(-C)// is semidet.
%
%  https://spec.graphql.org/draft/#NameContinue
graphql_name_continue(L)  --> graphql_letter(L).
graphql_name_continue(D)  --> graphql_digit(D).
graphql_name_continue(95) --> [95].


%! graphql_letter(-L)// is semidet.
%
%  https://spec.graphql.org/draft/#Letter
graphql_letter(L) -->
    [L],
    {   (   0'A =< L, L =< 0'Z
        ->  true
        ;   0'a =< L, L =< 0'z
        )
    }.


%! graphql_digit(-D)// is semidet.
%
%  https://spec.graphql.org/draft/#Digit
graphql_digit(D) -->
    [D],
    {   0'0 =< D, D =< 0'9  }.


graphql_numeric_value(N) -->
    graphql_integer_part(I),
    graphql_numeric_value_(I, N).


graphql_numeric_value_(I, N) -->
    graphql_fractional_part(F),
    !,
    graphql_numeric_value__(I, F, N).
graphql_numeric_value_(I, N) -->
    graphql_numeric_value__(I, [], N).


graphql_fractional_part([0'., D|T]) -->
    `.`,
    !,
    graphql_digit(D),
    graphql_digits(T).


graphql_exponent_part([E|T]) -->
    graphql_exponent_indicator(E),
    !,
    graphql_exponent_part_(T).


graphql_exponent_part_([S,D|T]) -->
    graphql_sign(S),
    graphql_digit(D),
    graphql_digits(T).
graphql_exponent_part_([D|T]) -->
    graphql_digit(D),
    graphql_digits(T).


graphql_exponent_indicator(0'e) --> `e`, !.
graphql_exponent_indicator(0'E) --> `E`.


graphql_sign(0'-) --> `-`, !.
graphql_sign(0'+) --> `+`.


graphql_numeric_value__(I, F, float(N)) -->
    graphql_exponent_part(E),
    !,
    {   append(I, F, H),
        append(H, E, C),
        number_codes(N, C)
    }.
graphql_numeric_value__(I, [], integer(N)) -->
    !,
    {    number_codes(N, I)
    }.
graphql_numeric_value__(I, F, float(N)) -->
    {   append(I, F, C),
        number_codes(N, C)
    }.


graphql_integer_part([0'-|T]) -->
    `-`,
    !,
    graphql_natural_part(T).
graphql_integer_part(T) -->
    graphql_natural_part(T).

graphql_natural_part([0]) -->
    `0`,
    !.
graphql_natural_part([H|T]) -->
    graphql_non_zero_digit(H),
    graphql_digits(T).


graphql_non_zero_digit(D) -->
    [D],
    {   0'1 =< D, D =< 0'9   }.


graphql_digits([H|T]) -->
    graphql_digit(H),
    !,
    graphql_digits(T).
graphql_digits([]) --> [].


graphql_string_value(S) -->
    [34],
    graphql_string_value_(S).


graphql_string_value_(S) -->
    [34],
    !,
    graphql_string_value__(S).
graphql_string_value_(S) -->
    graphql_string_body(S).


graphql_string_value__(S) -->
    [34],
    !,
    graphql_block_string(S).
graphql_string_value__("") --> [].


graphql_string_body(S) -->
    graphql_string_character(H),
    graphql_string_body_(H, S).

graphql_string_body_(H, S) -->
    graphql_string_characters(T),
    {   string_codes(S, [H|T])   }.


graphql_string_characters([]) --> [34], !.
graphql_string_characters([H|T]) -->
    graphql_string_character(H),
    graphql_string_characters(T).


graphql_string_character(C) -->
    [92],
    !,
    graphql_string_escape_sequence(C).
graphql_string_character(C) -->
    [C].


graphql_string_escape_sequence(U) -->
    [0'u],
    !,
    graphql_string_escape_hex(U).
graphql_string_escape_sequence(C) -->
    [C],
    {   memberchk(C, [34, 92, 47, 0'b, 0'f, 0'n, 0'r, 0't])   }.

graphql_string_escape_hex(U) -->
    [123],
    !,
    graphql_string_escape_hex_(U).
graphql_string_escape_hex(U) -->
    graphql_hex_digit(A),
    graphql_hex_digit(B),
    graphql_hex_digit(C),
    graphql_hex_digit(D),
    {   number_codes(U, [0'0, 0'x, A, B, C, D])   }.


graphql_string_escape_hex_(U) -->
    graphql_hex_digit(H),
    graphql_string_escape_hex__(H, U).


graphql_string_escape_hex__(H, U) -->
    graphql_hex_digits(T),
    {   number_codes(U, [0'0, 0'x, H | T])   }.


graphql_hex_digit(H) --> graphql_digit(H), !.
graphql_hex_digit(H) -->
    [H],
    {   (   0'A =< H, H =< 0'F
        ->  true
        ;   0'a =< H, H =< 0'f
        )
    }.


graphql_hex_digits([]) --> [125], !.
graphql_hex_digits([H|T]) --> graphql_hex_digit(H), graphql_hex_digits(T).


graphql_block_string("") -->
    [34,34,34],
    !.
graphql_block_string(S) -->
    graphql_line_terminator,
    !,
    graphql_block_string(S).
graphql_block_string(S) -->
    graphql_white_space(C),
    !,
    graphql_block_string_empty_initial_line([C|T]-T, 1, S).
graphql_block_string(S) -->
    graphql_block_string_characters(C),
    {   append(C, T, H)   },
    graphql_block_string_first_line(H-T, S).


graphql_block_string_empty_initial_line(_, _, "") -->
    [34,34,34],
    !.
graphql_block_string_empty_initial_line(_, _, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string(S).
graphql_block_string_empty_initial_line(H-[C|T], I0, S) -->
    graphql_white_space(C),
    !,
    {   I is I0 + 1   },
    graphql_block_string_empty_initial_line(H-T, I, S).
graphql_block_string_empty_initial_line(H-T0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N0),
        N is N0 + I
    },
    graphql_block_string_initial_line(H-T, N, I, S).


graphql_block_string_first_line(L, S) -->
    [34,34,34],
    !,
    {   graphql_block_string_close(L, [], 0, S)  }.
graphql_block_string_first_line(L, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_line_indent(L, M-M, C-C, 0, 1Inf, S).
graphql_block_string_first_line(H-T0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0)   },
    graphql_block_string_first_line(H-T, S).


graphql_block_string_initial_line(CH-CT, N, I, S) -->
    [34,34,34],
    !,
    {   graphql_block_string_close(F-F, [line(CH, CT, N)], I, S)  }.
graphql_block_string_initial_line(CH-CT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_line_indent(F-F, [line(CH,CT,N)|MoreLines]-MoreLines, L-L, 0, I, S).
graphql_block_string_initial_line(H-T0, N0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_initial_line(H-T, N, I, S).


graphql_block_string_characters([34,34,34]) -->
    [92,34,34,34],
    !.
graphql_block_string_characters([C]) -->
    [C].


graphql_block_string_line_indent(F, MH-[], _, _, I, S) -->
    [34,34,34],
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_line_indent(F, M, LH-LT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, M, [line(LH, LT, N)|T]-T, C-C, 0, I, S).
graphql_block_string_line_indent(F, M, H-[C|T], N0, I, S) -->
    graphql_white_space(C),
    !,
    {   N is N0 + 1   },
    graphql_block_string_line_indent(F, M, H-T, N, I, S).
graphql_block_string_line_indent(F, M, H-T0, N0, I0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        I is min(N0, I0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, M, H-T, N, I, S).


graphql_block_string_maybe_trailing_empty_line(F, MH-[], _W, _C, _N, I, S) -->
    [34,34,34],
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_maybe_trailing_empty_line(F, M, WH-[line(CH0,CT0,N)|WT], CH0-CT0, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, M, WH-WT, C-C, 0, I, S).
graphql_block_string_maybe_trailing_empty_line(F, M, W, CH-[C|CT], N0, I, S) -->
    graphql_white_space(C),
    !,
    {   N is N0 + 1   },
    graphql_block_string_maybe_trailing_empty_line(F, M, W, CH-CT, N, I, S).
graphql_block_string_maybe_trailing_empty_line(F, MH-WH, WH-WT, H-T0, N0, I0, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        I is min(N0, I0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, MH-WT, H-T, N, I, S).


graphql_block_string_line(F, MH-[line(CH, CT, N)], CH-CT, N, I, S) -->
    [34,34,34],
    !,
    {   graphql_block_string_close(F, MH, I, S)  }.
graphql_block_string_line(F, MH-[line(CH, CT, N)|MT], CH-CT, N, I, S) -->
    graphql_line_terminator,
    !,
    graphql_block_string_maybe_trailing_empty_line(F, MH-MT, W-W, C-C, 0, I, S).
graphql_block_string_line(F, M, H-T0, N0, I, S) -->
    graphql_block_string_characters(C),
    {   append(C, T, T0),
        length(C, N1),
        N is N0 + N1
    },
    graphql_block_string_line(F, M, H-T, N, I, S).


graphql_block_string_close(FirstLineH-FirstLineT, [line(H0, T, L)|MoreLines], Indent, String) :-
    FirstLineH == FirstLineT,
    !,
    graphql_block_string_dedent_line(H0, L, Indent, H),
    graphql_block_string_combine_more_lines(MoreLines, Indent, T),
    string_codes(String, H).
graphql_block_string_close(FirstLineH-FirstLineT, MoreLines, Indent, String) :-
    graphql_block_string_combine_more_lines(MoreLines, Indent, FirstLineT),
    string_codes(String, FirstLineH).

graphql_block_string_combine_more_lines([], _, []) :-
    !.
graphql_block_string_combine_more_lines([line(H0, T, L)|MoreLines], Indent, [10|H]) :-
    graphql_block_string_dedent_line(H0, L, Indent, H),
    graphql_block_string_combine_more_lines(MoreLines, Indent, T).


graphql_block_string_dedent_line(Line0, Length, Indent, Line) :-
    PrefixLength is min(Length, Indent),
    length(Prefix, PrefixLength),
    append(Prefix, Line, Line0).
