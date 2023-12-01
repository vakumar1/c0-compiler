{
module Parser (
    parser,
) where

import Tokens
import Types
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
    main            { Token (IDENTIFIER "main") _ }
    ident           { Token (IDENTIFIER _) _ }
    dec             { Token (DECNUM _) _ }
    hex             { Token (HEXNUM _) _ }
    while           { Token WHILE _ }
    for             { Token FOR _ }
    continue        { Token CONTINUE _ }
    break           { Token BREAK _ }
    return          { Token RETURN _ }
    assert          { Token ASSERT _ }
    true            { Token TRUE _ }
    false           { Token FALSE _ }
    null            { Token NULL _ }
    alloc           { Token ALLOC _ }
    alloc_arr       { Token ALLOC_ARRAY _ }
    int             { Token INT _ }
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

Function : int main '(' void ')' Block   { Function $2 (Type INT_TYPE $1) $6 }

Block : '{' Stmts '}'   { $2 }

Stmts :                 { [] }
    | Stmt Stmts        { ($1):($2) }

Stmt : Decl ';'         { DECL_STMT $1 }
    | Simp ';'          { SIMP_STMT $1 }
    | return Exp ';'    { RET_STMT $2 }

Decl : int ident        { Decl $2 (Type INT_TYPE $1) Nothing Nothing }
    | int ident '=' Exp { Decl $2 (Type INT_TYPE $1) (Just $3) (Just $4)}

Simp : Lval '=' Exp     { Simp $2 $1 $3 }
    | Lval '+=' Exp     { Simp $2 $1 $3 }
    | Lval '-=' Exp     { Simp $2 $1 $3 }
    | Lval '*=' Exp     { Simp $2 $1 $3 }
    | Lval '/=' Exp     { Simp $2 $1 $3 }
    | Lval '%=' Exp     { Simp $2 $1 $3 }

Lval : ident            { Lval $1 }
    | '(' Lval ')'      { $2 }

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