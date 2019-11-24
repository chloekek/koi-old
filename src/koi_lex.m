:- module koi_lex.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module bitmap.
:- import_module char.
:- import_module koi_recursive_descent.
:- import_module list.

:- type error --->
    bad_string_literal(position);
    unexpected_char(position, char);
    unexpected_eof.

:- type token --->
    token_brace_left;
    token_brace_right;
    token_bracket_left;
    token_bracket_right;
    token_colon;
    token_comma;
    token_equal;
    token_hyphen_greater;
    token_paren_left;
    token_paren_right;
    token_semicolon;

    token_fun;
    token_in;
    token_let;
    token_val;

    token_string(bitmap);

    token_identifier(string).

:- pred lex_tokens(char_stream, char_stream, result(error, list(lexeme(token)))).
:- mode lex_tokens(in, out, out) is det.

:- pred lex_token(char_stream, char_stream, result(error, lexeme(token))).
:- mode lex_token(in, out, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module int.
:- import_module string.

:- pred keyword(string, token).
:- mode keyword(in, out) is semidet.
keyword("fun", token_fun).
keyword("in",  token_in).
keyword("let", token_let).
keyword("val", token_val).

lex_tokens(!S, Res) :-
    many(lex_token, !S, Res).

lex_token(!S, Res) :-
    peek(!.S, Head),

    ( if Head = ok(Char), char.is_whitespace(Char) then
        consume(!S, _),
        lex_token(!S, Res)

    else if Head = ok(Char), char.is_alpha_or_underscore(Char) then
        lex_keyword_or_identifier(!S, Res)

    else if Head = ok(Char), Char = '"' then
        lex_string_literal(!S, Res)

    else
        lex_punctuation(!S, Res)
    ).

:- pred lex_keyword_or_identifier(char_stream, char_stream, result(error, lexeme(token))).
:- mode lex_keyword_or_identifier(in, out, out) is det.
lex_keyword_or_identifier(!S, Res) :-
    current_position(!.S, Pos),
    many(satisfy(char.is_alnum_or_underscore), !S, ok(Chars)),
    string.to_char_list(Name, Chars),
    ( if keyword(Name, Token)
    then Res = ok({Pos, Token})
    else Res = ok({Pos, token_identifier(Name)}) ).

:- pred lex_string_literal(char_stream, char_stream, result(error, lexeme(token))).
:- mode lex_string_literal(in, out, out) is det.
lex_string_literal(!S, Res) :-
    current_position(!.S, Pos),

    ( if
        consume(!S, ok('"')),
        IsntQuote = (pred(C::in) is semidet :- not C = '"'),
        many(satisfy(IsntQuote), !S, ok(Chars)),
        consume(!S, ok('"')),

        % Convert the string to UTF-8.
        list.map(char.to_utf8, Chars, ValueUtf8Points),
        require_det (
            list.condense(ValueUtf8Points, ValueUtf8Units),
            {_, ValueBytes} = list.foldl(
                (func(Unit, {Index, Bytes}) =
                    {Index + 1, Bytes ^ byte(Index) := Unit}),
                ValueUtf8Units,
                {0, bitmap.init(8 * list.length(ValueUtf8Units))}
            )
        )

    then
        Res = ok({Pos, token_string(ValueBytes)})

    else
        Res = err(bad_string_literal(Pos)) ).

:- pred lex_punctuation(char_stream, char_stream, result(error, lexeme(token))).
:- mode lex_punctuation(in, out, out) is det.
lex_punctuation(!S, Res) :-
    current_position(!.S, Pos),

    ( if consume(!S, ok(Char1)) then
        (    if Char1 = ('(') then Res = ok({Pos, token_paren_left})
        else if Char1 = (')') then Res = ok({Pos, token_paren_right})
        else if Char1 = (',') then Res = ok({Pos, token_comma})
        else if Char1 = (':') then Res = ok({Pos, token_colon})
        else if Char1 = (';') then Res = ok({Pos, token_semicolon})
        else if Char1 = ('=') then Res = ok({Pos, token_equal})
        else if Char1 = ('[') then Res = ok({Pos, token_bracket_left})
        else if Char1 = (']') then Res = ok({Pos, token_bracket_right})
        else if Char1 = ('{') then Res = ok({Pos, token_brace_left})
        else if Char1 = ('}') then Res = ok({Pos, token_brace_right})
        else if Char1 = ('-') then
            ( if consume(!S, ok(Char2)) then
                ( if Char2 = ('>') then Res = ok({Pos, token_hyphen_greater})
                else Res = err(unexpected_char(Pos, Char2)) )
            else
                Res = err(unexpected_eof) )
        else Res = err(unexpected_char(Pos, Char1)) )
    else
        Res = err(unexpected_eof) ).

/*


*/
