:- module koi_expression.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module bitmap.
:- import_module koi_schema.
:- import_module list.
:- import_module map.

:- type expression --->
    call(expression, expression);
    field(expression, string);
    lambda(string, schema, expression);
    let(string, expression, expression);
    list(list(expression));
    record(map(string, expression));
    string(bitmap);
    variable(string).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type checking

:- type environment ==
    map(string, schema).

:- func builtin = environment.
:- mode builtin = out is det.

:- pred schema(environment, expression, schema).
:- mode schema(in, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module pair.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type checking

builtin = map.from_assoc_list([
    "__builtin_deployment" -
        koi_schema.function(
            koi_schema.record(
                map.from_assoc_list([
                    "files"     - koi_schema.list(koi_schema.string),
                    "program"   - koi_schema.string,
                    "arguments" - koi_schema.list(koi_schema.string)
                ])
            ),
            koi_schema.deployment
        ),

    "__builtin_identity" -
        koi_schema.deployment,

    "__builtin_remote" -
        koi_schema.function(
            koi_schema.record(
                map.from_assoc_list([
                    "host"  - koi_schema.string,
                    "inner" - koi_schema.deployment
                ])
            ),
            koi_schema.deployment
        ),

    "__builtin_sequence" -
        koi_schema.function(
            koi_schema.record(
                map.from_assoc_list([
                    "first"  - koi_schema.deployment,
                    "second" - koi_schema.deployment
                ])
            ),
            koi_schema.deployment
        )
]).

schema(Env, call(Function, Argument), ResultSch) :-
    schema(Env, Function, koi_schema.function(ExpectedArgSch, ResultSch)),
    schema(Env, Argument, ArgumentSch),
    koi_schema.subschema(ArgumentSch, ExpectedArgSch).

schema(Env, field(Record, Field), FieldSch) :-
    schema(Env, Record, koi_schema.record(FieldSchs)),
    map.search(FieldSchs, Field, FieldSch).

schema(Env, lambda(Var, VarSch, Body), koi_schema.function(VarSch, BodySch)) :-
    InnerEnv = map.set(Env, Var, VarSch),
    schema(InnerEnv, Body, BodySch).

schema(Env, let(Var, Value, Body), BodySch) :-
    schema(Env, Value, ValueSch),
    InnerEnv = map.set(Env, Var, ValueSch),
    schema(InnerEnv, Body, BodySch).

schema(Env, list(Exprs), koi_schema.list(Union)) :-
    Pred = (pred(Expr::in, ElementSchIn::in, ElementSchOut::out) is semidet :-
                schema(Env, Expr, ExprSch),
                lub(ExprSch, ElementSchIn, ElementSchOut)),
    list.foldl(Pred, Exprs, koi_schema.bottom, Union).

schema(Env, record(Fields), koi_schema.record(FieldSchs)) :-
    map.map_values_only(schema(Env), Fields, FieldSchs).

schema(_, string(_), koi_schema.string).

schema(Env, variable(Var), map.elem(Var, Env)).
