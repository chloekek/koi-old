:- module koi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module koi_schema.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

main(!IO) :-
    Schema1 = koi_schema.record(
        map.from_assoc_list(
            [ "a" - koi_schema.string
            , "b" - koi_schema.string ]
        )
    ),
    Schema2 = koi_schema.function(koi_schema.list(Schema1),
                                  koi_schema.deployment),
    write_string(koi_schema.pretty(Schema2) ++ "\n", !IO).
