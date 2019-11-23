:- module koi_lex.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module bitmap.
:- import_module char.
:- import_module list.

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

:- pred lex(list(char), list(token)).
:- mode lex(in, out) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module int.
:- import_module string.

:- pred keyword(string, token).
:- mode keyword(in, out) is semidet.
:- mode keyword(out, in) is semidet.

:- pred keyword_or_identifier(list(char), list(token)).
:- mode keyword_or_identifier(in, out) is semidet.

:- pred string_literal(list(char), list(token)).
:- mode string_literal(in, out) is semidet.

:- pred punctuation(list(char), list(token)).
:- mode punctuation(in, out) is semidet.

lex([], []).
lex([Hd | Tl], Tokens) :-
    ( if char.is_whitespace(Hd) then
        lex(Tl, Tokens)

    else if char.is_alpha_or_underscore(Hd) then
        keyword_or_identifier([Hd | Tl], Tokens)

    else if Hd = '"' then
        string_literal(Tl, Tokens)

    else
        punctuation([Hd | Tl], Tokens) ).

keyword("fun", token_fun).
keyword("in",  token_in).
keyword("let", token_let).
keyword("val", token_val).

keyword_or_identifier(Chars, [Token | Tokens]) :-
    list.takewhile(char.is_alnum_or_underscore, Chars, Name, Tl),
    string.to_char_list(NameStr, Name),
    ( if keyword(NameStr, Keyword)
    then Token = Keyword
    else Token = token_identifier(NameStr) ),
    lex(Tl, Tokens).

string_literal(Chars, [Token | Tokens]) :-
    IsntQuote = (pred(C::in) is semidet :- not C = '"'),
    list.takewhile(IsntQuote, Chars, Value, ['"' | Tl]),

    list.map(char.to_utf8, Value, ValueUtf8Points),
    list.condense(ValueUtf8Points, ValueUtf8Units),
    {_, ValueBytes} = list.foldl(
        (func(Unit, {Index, Bytes}) = {Index + 1, Bytes ^ byte(Index) := Unit}),
        ValueUtf8Units,
        {0, bitmap.init(8 * list.length(ValueUtf8Units))}
    ),

    Token = token_string(ValueBytes),
    lex(Tl, Tokens).

punctuation(['('      | Tl], [token_paren_left     | TlTokens]) :- lex(Tl, TlTokens).
punctuation([')'      | Tl], [token_paren_right    | TlTokens]) :- lex(Tl, TlTokens).
punctuation([','      | Tl], [token_comma          | TlTokens]) :- lex(Tl, TlTokens).
punctuation(['-', '>' | Tl], [token_hyphen_greater | TlTokens]) :- lex(Tl, TlTokens).
punctuation([':'      | Tl], [token_colon          | TlTokens]) :- lex(Tl, TlTokens).
punctuation([';'      | Tl], [token_semicolon      | TlTokens]) :- lex(Tl, TlTokens).
punctuation(['='      | Tl], [token_equal          | TlTokens]) :- lex(Tl, TlTokens).
punctuation(['['      | Tl], [token_bracket_left   | TlTokens]) :- lex(Tl, TlTokens).
punctuation([']'      | Tl], [token_bracket_right  | TlTokens]) :- lex(Tl, TlTokens).
punctuation(['{'      | Tl], [token_brace_left     | TlTokens]) :- lex(Tl, TlTokens).
punctuation(['}'      | Tl], [token_brace_right    | TlTokens]) :- lex(Tl, TlTokens).
