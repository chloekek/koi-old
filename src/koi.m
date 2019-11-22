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
    Schema1 = record_schema(
        map.from_assoc_list(
            [ "a" - string_schema
            , "b" - string_schema ]
        )
    ),
    Schema2 = function_schema(list_schema(Schema1), deployment_schema),
    write_string(pretty_schema(Schema2) ++ "\n", !IO).
