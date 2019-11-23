:- module koi_schema.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module map.

:- type schema --->
    bottom;
    deployment;
    function(schema, schema);
    list(schema);
    record(map(string, schema));
    string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtyping

:- func lub(schema, schema) = schema.
:- mode lub(in, in) = out is semidet.

:- pred lub(schema, schema, schema).
:- mode lub(in, in, out) is semidet.

:- pred subschema(schema, schema).
:- mode subschema(in, in) is semidet.

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

lub(A, B) = C :-
    lub(A, B, C).

lub(bottom, bottom, bottom).
lub(bottom, deployment, deployment).
lub(bottom, function(A, B), function(A, B)).
lub(bottom, list(A), list(A)).
lub(bottom, record(A), record(A)).
lub(bottom, string, string).

lub(deployment, bottom, deployment).
lub(deployment, deployment, deployment).

lub(function(A, B), bottom, function(A, B)).
lub(function(A, B), function(A, C), function(A, lub(B, C))). % TODO: Parameter?

lub(list(A), bottom, list(A)).
lub(list(A), list(B), list(lub(A, B))).

lub(record(A), bottom, record(A)).
lub(record(A), record(B), record(C)) :- map.union(lub, A, B, C).

lub(string, bottom, string).
lub(string, string, string).

subschema(A, B) :- lub(A, B, B).
superschema(A, B) :- subschema(B, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty

:- func pretty_field(pair(string, schema)) = string.
:- mode pretty_field(in) = out is det.

pretty(Schema) = Pretty :-
    pretty(Schema, Pretty).

pretty(bottom, "bottom").
pretty(deployment, "deployment").
pretty(function(A, B), "(" ++ pretty(A) ++ " -> " ++ pretty(B) ++ ")").
pretty(list(A), "[" ++ pretty(A) ++ "]").
pretty(record(A), "{" ++ string.join_list(", ", Pretties) ++ "}") :-
    Pretties = list.map(pretty_field, map.to_sorted_assoc_list(A)).
pretty(string, "string").

pretty_field(Name - Schema) = Name ++ " : " ++ pretty(Schema).
