% This module provides a toolbox for writing recursive descent parsers. A
% parser is a predicate that reads from a stream of tokens and produces
% either a result or an error. Parsers are det; error reporting should happen
% through a sum type rather than through semidet. Parsers track source
% position information, allowing for improved diagostics.
:- module koi_recursive_descent.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module char.
:- import_module list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positions

% A lexeme pairs a token with a source position.
:- type lexeme(T) ==
    {position, T}.

% Source position, consisting of a file name and a position within the file.
:- type position --->
    position(
        position_file   :: string,
        position_line   :: int,
        position_column :: int
    ).

% Construct the position at line 1 and column 1 in a given file.
:- pred initial_position(string, position).
:- mode initial_position(in, out) is det.

% Advance the line and column numbers of a source position according to an
% input character.
:- pred advance_position(char, position, position).
:- mode advance_position(in, in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Results

% A parse result is either a value or an error.
:- type result(E, T) ---> err(E); ok(T).
:- inst always_ok ---> ok(ground).
:- mode out_ok == free >> always_ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Streams

% A stream S produces tokens T on demand. A stream may be finite; at some
% point giving no more tokens.
:- typeclass stream(S, E, T) <= (S -> E, T) where [
    % Consume a token from the stream, advancing the stream to its new state.
    pred consume(S, S, result(E, T)),
    mode consume(in, out, out) is det,

    pred current_position(S, position),
    mode current_position(in, out) is det
].

% Like consume, but do not advance the stream.
:- pred peek(S, result(E, T)) <= stream(S, E, T).
:- mode peek(in, out) is det.

% Stream of chars, backed by a list.
:- type char_stream --->
    char_stream(
        char_stream_position :: position,
        char_stream_chars    :: list(char)
    ).
:- instance stream(char_stream, {}, char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combinators

% Many combinators are higher-order. This type synonym and mode synonym are
% convenient for declaring such combinators.
:- type parser(S, E, T) == pred(S, S, result(E, T)).
:- mode in_parser == in(pred(in, out, out) is det).

% Consume a token if it satisfies the given predicate.
:- pred satisfy(pred(T), S, S, result({}, T)) <= stream(S, {}, T).
:- mode satisfy(in(pred(in) is semidet), in, out, out) is det.

% Run a given parser until it fails and collect the results. The parser must
% consume input, otherwise this predicate enters an infinite loop.
:- pred many(parser(S, E, T), S, S, result(E, list(T))).
:- mode many(in_parser, in, out, out_ok) is det.

% Like many, but expect a certain parser to succeed between every other
% token. An optionial dangling separator at the end is also accepted. The
% parsers must consume input, othewise this predicate enters an infinite
% loop.
:- pred sep_dangling(parser(S, E, X), parser(S, E, T), S, S, result(E, list(T))).
:- mode sep_dangling(in_parser, in_parser, in, out, out_ok) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positions

initial_position(File, position(File, 1, 1)).

advance_position(Char, !Position) :-
    ( if Char = '\n'
    then !Position ^ position_line   := !.Position ^ position_line + 1,
         !Position ^ position_column := 1
    else !Position ^ position_column := !.Position ^ position_column + 1 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Streams

peek(S, Res) :-
    consume(S, _, Res).

:- instance stream(char_stream, {}, char) where [
    pred(consume/3) is char_stream_consume,
    pred(current_position/2) is char_stream_current_position
].

:- pred char_stream_consume(char_stream, char_stream, result({}, char)).
:- mode char_stream_consume(in, out, out) is det.
char_stream_consume(char_stream(Pos, []),
                    char_stream(Pos, []),
                    err({})).
char_stream_consume(char_stream(!.Pos, [Char | Chars]),
                    char_stream(!:Pos,         Chars ),
                    ok(Char)) :-
    advance_position(Char, !Pos).

:- pred char_stream_current_position(char_stream, position).
:- mode char_stream_current_position(in, out) is det.
char_stream_current_position(char_stream(Pos, _), Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combinators

satisfy(P, !S, Res) :-
    ( if consume(!S, ok(T)), P(T)
    then Res = ok(T)
    else Res = err({}) ).

many(P, !S, Res) :-
    ( if P(!S, ok(Res1)) then
        many(P, !S, ok(Res2)),
        Res = ok([Res1 | Res2])
    else
        Res = ok([]) ).

sep_dangling(Sep, P, !S, Res) :-
    ( if P(!S, ok(Res1)) then
        ( if Sep(!S, ok(_))
        then sep_dangling(Sep, P, !S, Res)
        else Res = ok([Res1]) )
    else
        Res = ok([]) ).
