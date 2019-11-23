:- module koi_value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module bitmap.
:- import_module koi_expression.
:- import_module list.
:- import_module map.
:- import_module set.

:- type value --->
    deployment(deployment);
    function(function);
    list(list(value));
    record(map(string, value));
    string(bitmap).

:- type deployment --->
    identity;
    deployment(set(bitmap), bitmap, list(bitmap));
    sequence(deployment, deployment);
    remote(bitmap, deployment).

:- type function --->
    builtin_deployment;
    builtin_remote;
    builtin_sequence;
    lambda(koi_value.environment, string, expression).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation

:- type environment ==
    map(string, value).

:- func builtin = koi_value.environment.
:- mode builtin = out is det.

:- func evaluate(koi_value.environment, expression) = value.
:- mode evaluate(in, in) = out is semidet.

:- pred evaluate(koi_value.environment, expression, value).
:- mode evaluate(in, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module pair.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions

:- pred get_deployment(value, string, deployment).
:- mode get_deployment(in, in, out) is semidet.
get_deployment(record(Record), Field, Deployment) :-
    map.search(Record, Field, deployment(Deployment)).

:- pred get_string(value, string, bitmap).
:- mode get_string(in, in, out) is semidet.
get_string(record(Record), Field, String) :-
    map.search(Record, Field, string(String)).

:- pred get_string_list(value, string, list(bitmap)).
:- mode get_string_list(in, in, out) is semidet.
get_string_list(record(Record), Field, Strings) :-
    map.search(Record, Field, list(Values)),
    GetString = (pred(string(S)::in, S::out) is semidet),
    list.map(GetString, Values, Strings).

:- pred call_function(function, value, value).
:- mode call_function(in, in, out) is semidet.

call_function(builtin_deployment, Options, Result) :-
    get_string_list(Options, "files",     Files),
    get_string(     Options, "program",   Program),
    get_string_list(Options, "arguments", Arguments),
    Deployment = deployment(set.list_to_set(Files), Program, Arguments),
    Result = deployment(Deployment).

call_function(builtin_remote, Options, Result) :-
    get_string(    Options, "host",  Host),
    get_deployment(Options, "inner", Inner),
    Deployment = remote(Host, Inner),
    Result = deployment(Deployment).

call_function(builtin_sequence, Options, Result) :-
    get_deployment(Options, "first",  First),
    get_deployment(Options, "second", Second),
    Deployment = sequence(First, Second),
    Result = deployment(Deployment).

call_function(lambda(Env, Var, Body), Argument, Result) :-
    InnerEnv = map.set(Env, Var, Argument),
    evaluate(InnerEnv, Body, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation

builtin = map.from_assoc_list([
    "__builtin_deployment" - function(builtin_deployment),
    "__builtin_identity" - deployment(identity),
    "__builtin_remote" - function(builtin_remote),
    "__builtin_sequence" - function(builtin_sequence)
]).

evaluate(Env, Expr) = Value :-
    evaluate(Env, Expr, Value).

evaluate(Env, koi_expression.call(Function, Argument), Result) :-
    evaluate(Env, Function, function(FunctionFunc)),
    evaluate(Env, Argument, ArgumentValue),
    call_function(FunctionFunc, ArgumentValue, Result).

evaluate(Env, koi_expression.field(Record, Field), Result) :-
    evaluate(Env, Record, record(RecordMap)),
    map.search(RecordMap, Field, Result).

evaluate(Env, koi_expression.lambda(Var, _Sch, Body), FunctionValue) :-
    Function = lambda(Env, Var, Body),
    FunctionValue = function(Function).

evaluate(Env, koi_expression.let(Var, Value, Body), BodyValue) :-
    evaluate(Env, Value, ValueValue),
    InnerEnv = map.set(Env, Var, ValueValue),
    evaluate(InnerEnv, Body, BodyValue).

evaluate(Env, koi_expression.list(Elements), list(ElementValues)) :-
    list.map(evaluate(Env), Elements, ElementValues).

evaluate(Env, koi_expression.record(Fields), record(FieldValues)) :-
    map.map_values_only(evaluate(Env), Fields, FieldValues).

evaluate(_Env, koi_expression.string(Value), string(Value)).

evaluate(Env, koi_expression.variable(Var), map.elem(Var, Env)).
