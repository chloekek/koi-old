:- module koi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module koi_expression.
:- import_module koi_lex.
:- import_module koi_parse.
:- import_module koi_recursive_descent.
:- import_module koi_schema.
:- import_module koi_value.
:- import_module list.
:- import_module string.

:- pred phase_lex(string, io, io).
:- mode phase_lex(in, di, uo) is det.

:- pred phase_parse(list(token), io, io).
:- mode phase_parse(in, di, uo) is det.

:- pred phase_check(expression, io, io).
:- mode phase_check(in, di, uo) is det.

:- pred phase_evaluate(expression, io, io).
:- mode phase_evaluate(in, di, uo) is det.

main(!IO) :-
    Source = "
        let
            fun deployment (files : [string])
                           (program : string)
                           (arguments : [string]) =
                __builtin_deployment {files, program, arguments};

            fun sequence (first : deployment)
                         (second : deployment) =
                __builtin_sequence {first, second};

            fun remote (host : string)
                       (inner : deployment) =
                __builtin_remote {host, inner};

            val command = deployment [];

            val systemctl = command \"systemctl\";

            fun systemctl_reload (unit : string) =
                systemctl [\"reload\", unit];

        in
            remote \"wiki\" (
                sequence
                    (systemctl_reload \"nginx\")
                    (systemctl_reload \"php-fpm\")
            )
    ",
    phase_lex(Source, !IO).

phase_lex(Source, !IO) :-
    string.to_char_list(Source, Chars),
    koi_recursive_descent.initial_position("<example>", Pos),
    Stream = koi_recursive_descent.char_stream(Pos, Chars),
    koi_lex.lex_tokens(Stream, Rest, Res),
    % TODO: Check that Rest is empty.

    write_string("Tokens:       ", !IO),
    write(Res, !IO),
    write_string("\n\n", !IO),

    write_string("Rest:         ", !IO),
    write(Rest, !IO),
    write_string("\n\n", !IO),

    ( if Res = koi_recursive_descent.ok(Lexemes) then
        Tokens = list.map((func({_, T}) = T), Lexemes),
        phase_parse(Tokens, !IO)
    else
        true ).

phase_parse(Tokens, !IO) :-
    ( if koi_parse.expression_1(Expr, Tokens, []) then
        write_string("Expression:   ", !IO),
        write(Expr, !IO),
        write_string("\n\n", !IO),
        phase_check(Expr, !IO)
    else
        write_string("!! Bad parse\n", !IO) ).

phase_check(Expr, !IO) :-
    ( if koi_expression.schema(koi_expression.builtin, Expr, Sch) then
        write_string("Type:         ", !IO),
        write_string(koi_schema.pretty(Sch), !IO),
        write_string("\n\n", !IO),
        phase_evaluate(Expr, !IO)
    else
        write_string("!! Bad type\n", !IO) ).

phase_evaluate(Expr, !IO) :-
    ( if koi_value.evaluate(koi_value.builtin, Expr, Value) then
        write_string("Value:        ", !IO),
        write(Value, !IO),
        write_string("\n\n", !IO)
    else
        write_string("!! Bad program\n", !IO) ).
