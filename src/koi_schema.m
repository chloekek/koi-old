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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping

% The predicate subschema(A, B) checks whether A is a subschema of B.
:- pred subschema(schema, schema).
:- mode subschema(in, in) is semidet.

% The predicate superschema(A, B) checks whether B is a subschema of A.
:- pred superschema(schema, schema).
:- mode superschema(in, in) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty

:- func pretty_schema(schema) = string.
:- mode pretty_schema(in) = out is det.

:- pred pretty_schema(schema, string).
:- mode pretty_schema(in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping

:- pred subschema_record_field(map(string, schema), string, schema, {}, {}).
:- mode subschema_record_field(in, in, in, in, out) is semidet.

subschema(deployment_schema, deployment_schema).
subschema(function_schema(A, B), function_schema(C, D)) :-
    superschema(A, C), subschema(B, D).
subschema(list_schema(A), list_schema(B)) :- subschema(A, B).
subschema(record_schema(A), record_schema(B)) :-
    map.foldl(subschema_record_field(A), B, {}, {}).
subschema(string_schema, string_schema).

superschema(A, B) :- subschema(B, A).

subschema_record_field(Subrecord, Name, Superschema, {}, {}) :-
    map.search(Subrecord, Name, Subschema),
    subschema(Subschema, Superschema).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty

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
