:- module koi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module koi_expression.
:- import_module koi_schema.
:- import_module list.
:- import_module string.

main(!IO) :-
    Expression =
        koi_expression.call(
            koi_expression.call(
                koi_expression.call(
                    koi_expression.variable("deployment"),
                    koi_expression.list([])
                ),
                koi_expression.string("systemctl")
            ),
            koi_expression.list([koi_expression.string("daemon-reload")])
        ),
    ( if koi_expression.schema(koi_expression.builtin, Expression, Schema)
    then write_string(koi_schema.pretty(Schema) ++ "\n", !IO)
    else write_string("!! Ill-typed\n", !IO) ).
