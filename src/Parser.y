{
module Parser (
    parser,
) where

import Tokens
import Ast

}

%name parser
%tokentype { Token }
%error { parseError }

%token
    ';'             { Token SEMICOLON _ }
    '('             { Token OPEN_PAREN _ }
    '['             { Token OPEN_BRACK _ }
    '{'             { Token OPEN_BRACE _ }
    ')'             { Token CLOSE_PAREN _ }
    ']'             { Token CLOSE_BRACK _ }
    '}'             { Token CLOSE_BRACE _ }
    '+'             { Token PLUS _ }
    '-'             { Token DASH _ }
    '*'             { Token STAR _ }
    '/'             { Token SLASH _ }
    '%'             { Token PERC _ }
    '+='            { Token PLUS_EQ _ }
    '-='            { Token DASH_EQ _ }
    '*='            { Token STAR_EQ _ }
    '/='            { Token SLASH_EQ _ }
    '%='            { Token PERC_EQ _ }
    '='             { Token EQUAL _ }
    ident           { Token (IDENTIFIER _) _ }
    dec             { Token (DECNUM _) _ }
    hex             { Token (HEXNUM _) _ }
    while           { Token WHILE _ }
    for             { Token OPEN_PAREN _ }
    continue        { Token CONTINUE _ }
    break           { Token BREAK _ }
    return          { Token RETURN _ }
    assert          { Token ASSERT _ }
    true            { Token TRUE _ }
    false           { Token FALSE _ }
    null            { Token NULL _ }
    alloc           { Token ALLOC _ }
    alloc_arr       { Token ALLOC_ARRAY _ }
    int             { Token OPEN_PAREN _ }
    bool            { Token BOOL _ }
    void            { Token VOID _ }
    char            { Token CHAR _ }
    string          { Token STRING _ }
    eof             { Token EOF _ }

%right '-'
%left '*' '/' '%'
%left '+' '-'
%left '=' '+=' '-=' '*=' '/=' '%='
%%

Exp : '(' Exp ')'   { $2 }
    | Intconst      { INTCONST_EXP $1 }
    | ident         { IDENTIFIER_EXP $1 }
    | Exp '+' Exp   { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '-' Exp   { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '*' Exp   { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '/' Exp   { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '%' Exp   { BINOP_EXP (Binop $2 $1 $3) }
    | '-' Exp       { UNOP_EXP (Unop $1 $2) }

Intconst : dec      { DECNUM_INTCONST $1 }
    | hex           { HEXNUM_INTCONST $1 }



{
parseError :: [Token] -> a
parseError tokens = error (show tokens)
}