:- module koi_parse.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module koi_expression.
:- import_module koi_lex.
:- import_module koi_schema.
:- import_module list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 1)

:- pred expression_1(expression, list(token), list(token)).
:- mode expression_1(out, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 2)

:- pred expression_2(expression, list(token), list(token)).
:- mode expression_2(out, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 3)

:- pred expression_3(expression, list(token), list(token)).
:- mode expression_3(out, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type (level 1)

:- pred schema_1(schema, list(token), list(token)).
:- mode schema_1(out, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type (level 2)

:- pred schema_2(schema, list(token), list(token)).
:- mode schema_2(out, in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module map.

:- pred many(pred(R, list(token), list(token)), list(R), list(token), list(token)).
:- mode many(pred(out, in, out) is semidet, out, in, out) is det.

:- pred separated(pred(R, list(token), list(token)), list(R), list(token), list(token)).
:- mode separated(pred(out, in, out) is semidet, out, in, out) is det.

many(P, R, !Tokens) :-
    ( if P(R2, !Tokens) then
        many(P, R3, !Tokens),
        R = [R2 | R3]
    else
        R = [] ).

separated(P, R, !Tokens) :-
    ( if P(R2, !Tokens) then
        ( if !.Tokens = [token_comma | !:Tokens]
        then separated(P, R3, !Tokens), R = [R2 | R3]
        else R = [R2] )
    else
        R = [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 1)

expression_1(Expr, !Tokens) :-
    expression_2(FunctionExpr, !Tokens),
    many(expression_2, ArgumentExprs, !Tokens),

    Call = (func(A, F) = koi_expression.call(F, A)),
    Expr = list.foldl(Call, ArgumentExprs, FunctionExpr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 2)

expression_2(Expr, !Tokens) :-
    % TODO: Parse field expressions.
    expression_3(Expr, !Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression (level 3)

:- pred param({string, schema}, list(token), list(token)).
:- mode param(out, in, out) is semidet.

:- pred binding({string, expression}, list(token), list(token)).
:- mode binding(out, in, out) is semidet.

param({Name, Sch}, !Tokens) :-
    !.Tokens = [token_paren_left        | !:Tokens],
    !.Tokens = [token_identifier(Name)  | !:Tokens],
    !.Tokens = [token_colon             | !:Tokens],
    schema_1(Sch,                          !Tokens),
    !.Tokens = [token_paren_right       | !:Tokens].

binding({Name, Value}, !Tokens) :-
    ( !.Tokens = [token_val              | !:Tokens],
      !.Tokens = [token_identifier(Name) | !:Tokens],
      !.Tokens = [token_equal            | !:Tokens],
      expression_1(Value,                   !Tokens),
      !.Tokens = [token_semicolon        | !:Tokens]
    ;
      !.Tokens = [token_fun              | !:Tokens],
      !.Tokens = [token_identifier(Name) | !:Tokens],
      many(param, Params,                   !Tokens),
      !.Tokens = [token_equal            | !:Tokens],
      expression_1(Body,                    !Tokens),
      !.Tokens = [token_semicolon        | !:Tokens],

      Lambda = (
          func({ParamName, ParamSch}, Inner) =
              koi_expression.lambda(ParamName, ParamSch, Inner)
      ),
      Value = list.foldr(Lambda, Params, Body) ).

expression_3(Expr, !Tokens) :-
    !.Tokens = [token_fun            | !:Tokens],
    many(param, Params,                 !Tokens),
    !.Tokens = [token_hyphen_greater | !:Tokens],
    expression_1(Body,                  !Tokens),

    Lambda = (
        func({Name, Sch}, Inner) =
            koi_expression.lambda(Name, Sch, Inner)
    ),
    Expr = list.foldr(Lambda, Params, Body).

expression_3(Expr, !Tokens) :-
    !.Tokens = [token_let   | !:Tokens],
    many(binding, Bindings,    !Tokens),
    !.Tokens = [token_in    | !:Tokens],
    expression_1(Body,         !Tokens),

    Let = (
        func({Name, Value}, Inner) =
            koi_expression.let(Name, Value, Inner)
    ),
    Expr = list.foldr(Let, Bindings, Body).

expression_3(Expr, !Tokens) :-
    !.Tokens = [token_bracket_left  | !:Tokens],
    separated(expression_1, Exprs,     !Tokens),
    !.Tokens = [token_bracket_right | !:Tokens],
    Expr = koi_expression.list(Exprs).

expression_3(Expr, !Tokens) :-
    % TODO: Parse non-empty records.
    !.Tokens = [token_brace_left  | !:Tokens],
    !.Tokens = [token_brace_right | !:Tokens],
    Expr = koi_expression.record(map.init).

expression_3(Expr, !Tokens) :-
    !.Tokens = [token_string(Value) | !:Tokens],
    Expr = koi_expression.string(Value).

expression_3(Expr, !Tokens) :-
    !.Tokens = [token_identifier(Name) | !:Tokens],
    Expr = koi_expression.variable(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type (level 1)

schema_1(Sch, !Tokens) :-
    schema_2(ArgumentSch, !Tokens),
    ( if !.Tokens = [token_hyphen_greater | !:Tokens] then
        schema_1(ReturnSch, !Tokens),
        Sch = koi_schema.function(ArgumentSch, ReturnSch)
    else
        Sch = ArgumentSch ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type (level 2)

schema_2(Sch, !Tokens) :-
    !.Tokens = [token_identifier("bottom") | !:Tokens],
    Sch = koi_schema.bottom.

schema_2(Sch, !Tokens) :-
    !.Tokens = [token_identifier("deployment") | !:Tokens],
    Sch = koi_schema.deployment.

schema_2(Sch, !Tokens) :-
    !.Tokens = [token_bracket_left  | !:Tokens],
    schema_1(ElementSch,               !Tokens),
    !.Tokens = [token_bracket_right | !:Tokens],
    Sch = koi_schema.list(ElementSch).

% TODO: Parse record types.

schema_2(Sch, !Tokens) :-
    !.Tokens = [token_identifier("string") | !:Tokens],
    Sch = koi_schema.string.
