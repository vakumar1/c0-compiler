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
    '!'             { Token EXCL _ }
    '~'             { Token TILDE _ }
    '<<'            { Token LEFT_LEFT _ }
    '>>'            { Token RIGHT_RIGHT _ }
    '<'             { Token LEFT _ }
    '>'             { Token RIGHT _ }
    '<='            { Token LEFT_EQ _ }
    '>='            { Token RIGHT_EQ _ }
    '=='            { Token EQ_EQ _ }
    '!='            { Token EXCL_EQ _ }
    '&'             { Token AMP _ }
    '^'             { Token CARET _ }
    '|'             { Token PIPE _ }
    '&&'            { Token AMP_AMP _ }
    '||'            { Token PIPE_PIPE _ }
    '?'             { Token QUEST _ }
    ':'             { Token COLON _ }

    '='             { Token EQUAL _ }
    '+='            { Token PLUS_EQ _ }
    '-='            { Token DASH_EQ _ }
    '*='            { Token STAR_EQ _ }
    '/='            { Token SLASH_EQ _ }
    '%='            { Token PERC_EQ _ }
    '&='            { Token AMP_EQ _ }
    '^='            { Token CARET_EQ _ }
    '|='            { Token PIPE_EQ _ }
    '<<='           { Token LEFT_LEFT_EQ _ }
    '>>='           { Token RIGHT_RIGHT_EQ _ }

    '++'            { Token PLUS_PLUS _ }
    '--'            { Token DASH_DASH _ }

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

%right '-' '!' '~' '++' '--'
%left '*' '/' '%'
%left '+' '-'
%left '<<' '>>'
%left '<' '<=' '>' '>='
%left '==' '!='
%left '&'
%left '^'
%left '|'
%left '&&'
%left '||'

%left '=' '+=' '-=' '*=' '/=' '%=' '&=' '^=' '|=' '<<=' '>>='
%%

Function : int main '(' void ')' Block   { Function $2 (Type INT_TYPE $1) $6 }

Block : '{' Stmts '}'   { $2 }

Stmts :                 { [] }
    | Stmt Stmts        { ($1):($2) }

Stmt : Simp ';'         { SIMP_STMT $1 }
    | Block             { BLOCK_STMT $1 }
    | return Exp ';'    { RET_STMT $2 }

Simp : Asn              { ASN_SIMP $1 }
    | Decl              { DECL_SIMP $1 }
    | Post              { POSTOP_SIMP $1 }
    | Exp               { EXP_SIMP $1 }

Asn : Lval Asnop Exp    { Asn $2 $1 $3 }

Decl : Type ident        { Decl $2 (Type INT_TYPE $1) Nothing Nothing }
    | Type ident '=' Exp { Decl $2 (Type INT_TYPE $1) (Just $3) (Just $4) }

Type : int          { Type INT_TYPE $1 }
    | bool          { Type BOOL_TYPE $1 }

Post : Lval Postop      { Post $2 $1 }

Lval : ident            { Lval $1 }
    | '(' Lval ')'      { $2 }

Asnop : '='             { $1 }
    | '+='              { $1 }
    | '-='              { $1 }
    | '*='              { $1 }
    | '/='              { $1 }
    | '%='              { $1 }
    | '&='              { $1 }
    | '^='              { $1 }
    | '|='              { $1 }
    | '<<='             { $1 }
    | '>>='             { $1 }

Postop : '++'           { $1 }
    | '--'              { $1 }

Exp : '(' Exp ')'   { $2 }
    | hex           { HEXNUM_EXP $1 }
    | dec           { DECNUM_EXP $1 }
    | bool          { BOOL_EXP $1 }
    | ident         { IDENTIFIER_EXP $1 }
    | Exp Binop Exp { BINOP_EXP (Binop $2 $1 $3) }
    | Unop Exp      { UNOP_EXP (Unop $1 $2) }
    | Exp '?' Exp ':' Exp   { TERN_EXP $1 $2 $3 }

Binop : '+' { $1 }
    | '-'   { $1 }
    | '*'   { $1 }
    | '/'   { $1 }
    | '%'   { $1 }
    | '<'   { $1 }
    | '<='  { $1 }
    | '>'   { $1 }
    | '>='  { $1 }
    | '=='  { $1 }
    | '!='  { $1 }
    | '&&'  { $1 }
    | '||'  { $1 }
    | '&'   { $1 }
    | '^'   { $1 }
    | '|'   { $1 }
    | '>>'  { $1 }
    | '<<'  { $1 }

Unop : '-'  { $1 }
    | '~'   { $1 }
    | '!'   { $1 }

{
parseError :: [Token] -> a
parseError tokens = error (show tokens)
}