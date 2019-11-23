:- module koi_schema.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module map.

:- type schema --->
    deployment;
    function(schema, schema);
    list(schema);
    record(map(string, schema));
    string.

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

:- func pretty(schema) = string.
:- mode pretty(in) = out is det.

:- pred pretty(schema, string).
:- mode pretty(in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping

:- pred subschema_field(map(string, schema), string, schema, {}, {}).
:- mode subschema_field(in, in, in, in, out) is semidet.

subschema(deployment, deployment).
subschema(function(A, B), function(C, D)) :- superschema(A, C), subschema(B, D).
subschema(list(A), list(B)) :- subschema(A, B).
subschema(record(A), record(B)) :- map.foldl(subschema_field(A), B, {}, {}).
subschema(string, string).

superschema(A, B) :- subschema(B, A).

subschema_field(Subrecord, Name, Superschema, {}, {}) :-
    map.search(Subrecord, Name, Subschema),
    subschema(Subschema, Superschema).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty

:- func pretty_field(pair(string, schema)) = string.
:- mode pretty_field(in) = out is det.

pretty(Schema) = Pretty :-
    pretty(Schema, Pretty).

pretty(deployment, "deployment").
pretty(function(A, B), "(" ++ pretty(A) ++ " -> " ++ pretty(B) ++ ")").
pretty(list(A), "[" ++ pretty(A) ++ "]").
pretty(record(A), "{" ++ string.join_list(", ", Pretties) ++ "}") :-
    Pretties = list.map(pretty_field, map.to_sorted_assoc_list(A)).
pretty(string, "string").

pretty_field(Name - Schema) = Name ++ " : " ++ pretty(Schema).
