:- module koi_schema.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module map.

:- type schema --->
    deployment_schema;
    function_schema(schema, schema);
    list_schema(schema);
    record_schema(map(string, schema));
    string_schema.

:- func pretty_schema(schema) = string.
:- mode pretty_schema(in) = out is det.

:- pred pretty_schema(schema, string).
:- mode pretty_schema(in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

:- func pretty_record_field(pair(string, schema)) = string.
:- mode pretty_record_field(in) = out is det.

pretty_schema(Schema) = Pretty :-
    pretty_schema(Schema, Pretty).

pretty_schema(deployment_schema, "deployment").
pretty_schema(function_schema(A, B),
              "(" ++ pretty_schema(A) ++ " -> " ++ pretty_schema(B) ++ ")").
pretty_schema(list_schema(A), "[" ++ pretty_schema(A) ++ "]").
pretty_schema(record_schema(A), Pretty) :-
    Fields   = map.to_sorted_assoc_list(A),
    Pretties = list.map(pretty_record_field, Fields),
    Pretty   = "{" ++ string.join_list(", ", Pretties) ++ "}".
pretty_schema(string_schema, "string").

pretty_record_field(Name - Schema) = Name ++ " : " ++ pretty_schema(Schema).
