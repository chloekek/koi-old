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
:- import_module koi_schema.
:- import_module list.
:- import_module string.

:- pred phase_lex(string, io, io).
:- mode phase_lex(in, di, uo) is det.

:- pred phase_parse(list(token), io, io).
:- mode phase_parse(in, di, uo) is det.

:- pred phase_check(expression, io, io).
:- mode phase_check(in, di, uo) is det.

:- pred phase_run(expression, io, io).
:- mode phase_run(in, di, uo) is det.

main(!IO) :-
    Source = "
        let
            val command = deployment [];

            val systemctl = command \"systemctl\";

            fun systemctl_reload (unit : string) =
                systemctl [\"reload\", unit];

        in
            systemctl_reload \"postgresql\"
    ",
    phase_lex(Source, !IO).

phase_lex(Source, !IO) :-
    string.to_char_list(Source, Chars),
    ( if koi_lex.lex(Chars, Tokens) then
        write_string("Tokens:       ", !IO),
        write(Tokens, !IO),
        write_string("\n\n", !IO),
        phase_parse(Tokens, !IO)
    else
        write_string("!! Bad lex\n", !IO) ).

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
        phase_run(Expr, !IO)
    else
        write_string("!! Bad type\n", !IO) ).

phase_run(_Expr, !IO).
