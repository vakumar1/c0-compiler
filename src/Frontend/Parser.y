{
module Frontend.Parser (
    parser,
) where

import Model.Tokens
import Model.Types
import Model.Ast

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
    ','             { Token COMMA _ }

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

    ident           { Token (IDENTIFIER _) _ }
    dec             { Token (DECNUM _) _ }
    hex             { Token (HEXNUM _) _ }
    if              { Token IF _ }
    else            { Token ELSE _ }
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
    type            { Token (TYPE _) _ }
    typedef         { Token TYPEDEF _ }
    eof             { Token EOF _ }


-- PRECEDENCE RULES: predence rules are applied to DEFINITIONS and TOKENS
-- i.e., the parser will compare the precedence of the in-progress definition
-- against the precedence of the lookahead token
-- - definition > token --> reduce using the definition rule
-- - definition < token --> shift the token
-- (GENERALLY) APPLYING PRECEDENCE RULES:
-- - if a definition associates RIGHT --> the infix token operator takes precedence over the definition
--   e.g. the ternary operator . ? . : . associates right --> the '?' token takes precedence 
--   over the ternary definition (TERN)
-- - if a definition associates LEFT --> the definition takes precedence over the infix token
--   e.g. the addition operation . + . associates left --> the addition definition (BINOP2) takes precedence
--   over the '+' token
-- ** note this is only a general rule for infix operators (see UNOP prefix operators below)


-- else token (takes precendence over lone if statement)
%nonassoc LONE_IF
%nonassoc else

-- assignment operators
%nonassoc ASNOP
%nonassoc '=' '+=' '-=' '*=' '/=' '%=' '&=' '^=' '|=' '<<=' '>>='

-- ternary operator
%nonassoc TERN
%nonassoc '?'

-- binop operators
%nonassoc '||'
%nonassoc BINOP10

%nonassoc '&&'
%nonassoc BINOP9

%nonassoc '|'
%nonassoc BINOP8

%nonassoc '^'
%nonassoc BINOP7

%nonassoc '&'
%nonassoc BINOP6

%nonassoc '==' '!='
%nonassoc BINOP5

%nonassoc '<' '<=' '>' '>='
%nonassoc BINOP4

%nonassoc '<<' '>>'
%nonassoc BINOP3

%nonassoc '+' '-'
%nonassoc BINOP2

%nonassoc '*' '/' '%'
%nonassoc BINOP1

-- unary operators
%nonassoc UNOP
%right '!' '~' '++' '--'

-- parentheses
%right PAREN
%left ')'
%right '('
%%

Program :               { [] }
    | GDecl Program     { ($1):($2) }

GDecl : Typedef         { TYPEDEF_GDECL $1 }
    | FunctionDecl      { FNDECL_GDECL $1 }
    | Function          { FNDEFN_GDECL $1 }

Typedef : typedef Type ident ';'
                        { Typedef $2 $3 }

FunctionDecl : Type ident Params ';'
                        { FunctionSignature $2 $3 $1 }

Function : Type ident Params Block   
                        { Function (FunctionSignature $2 $3 $1) $4 }

Params : '(' ')'        { [] }  
    | '(' Param ParamFollow ')'
                        { ($2):($3) }

ParamFollow :           { [] }
    | ',' Param ParamFollow
                        { ($2):($3) }

Param : Type ident      { Param $2 $1 }

Block : '{' Stmts '}'   { $2 }

Stmts :                 { [] }
    | Stmt Stmts        { ($1):($2) }

Stmt : 
    Simp ';'            { SIMP_STMT $1 }
    | Block             { BLOCK_STMT $1 }
    | Control           { CONTROL_STMT $1 }

Simp : Asn              { ASN_SIMP $1 }
    | Decl              { DECL_SIMP $1 }
    | Post              { POST_SIMP $1 }
    | Exp               { EXP_SIMP $1 }

Control : If            { IF_CTRL $1 }
    | While             { WHILE_CTRL $1 }
    | For               { FOR_CTRL $1 }
    | return Exp ';'    { RET_CTRL $2 }

Asn : Lval Asnop Exp    %prec ASNOP { Asn $2 $1 $3 }

Decl : Type ident        { Decl $2 $1 Nothing Nothing }
    | Type ident '=' Exp { Decl $2 $1 (Just $3) (Just $4) }

Type : type             { Type $1 }

Post : Lval Postop      { Post $2 $1 }

If :
    if '(' Exp ')' Stmt 
                        %prec LONE_IF { If $3 $5 Nothing }
    | if '(' Exp ')' Stmt else Stmt
                        { If $3 $5 (Just $7) }

While : while '(' Exp ')' Stmt
                        { While $3 $5 }

For : for '(' Simpopt ';' Exp ';' Simpopt ')' Stmt
                        { For $3 $5 $7 $9 }

Simpopt :               { Nothing }
    | Simp              { Just $1 }

Lval : '(' Lval ')'     %prec PAREN { $2 }
    | ident             { Lval $1 }

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

Exp : 
    '(' Exp ')'         %prec PAREN { $2 }
    | hex               { HEXNUM_EXP $1 }
    | dec               { DECNUM_EXP $1 }
    | true              { BOOL_EXP $1 }
    | false             { BOOL_EXP $1 }
    | ident             { IDENTIFIER_EXP $1 }
    | Unop              { $1 }
    | Binop             { $1 }
    | Ternary           { $1 }
    | FunctionCall      { FN_CALL_EXP $1 }

Unop :
    '-' Exp             %prec UNOP { UNOP_EXP (Unop $1 $2) }
    | '~' Exp           %prec UNOP { UNOP_EXP (Unop $1 $2) }
    | '!' Exp           %prec UNOP { UNOP_EXP (Unop $1 $2) }

Binop :
    Exp '*' Exp         %prec BINOP1 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '/' Exp       %prec BINOP1 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '%' Exp       %prec BINOP1 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '+' Exp       %prec BINOP2 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '-' Exp       %prec BINOP2 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '>>' Exp      %prec BINOP3 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '<<' Exp      %prec BINOP3 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '<' Exp       %prec BINOP4 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '<=' Exp      %prec BINOP4 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '>' Exp       %prec BINOP4 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '>=' Exp      %prec BINOP4 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '==' Exp      %prec BINOP5 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '!=' Exp      %prec BINOP5 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '&' Exp       %prec BINOP6 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '^' Exp       %prec BINOP7 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '|' Exp       %prec BINOP8 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '&&' Exp      %prec BINOP9 { BINOP_EXP (Binop $2 $1 $3) }
    | Exp '||' Exp      %prec BINOP10 { BINOP_EXP (Binop $2 $1 $3) }

Ternary :
    Exp '?' Exp ':' Exp
                        %prec TERN { TERN_EXP (Ternop $2 $1 $3 $5) }

FunctionCall :
    ident Args          { FunctionCall $1 $2 }

Args : '(' ')'          { [] }
    | '(' Exp ArgFollow ')'
                        { ($2):($3) }

ArgFollow :             { [] }
    | ',' Exp ArgFollow { ($2):($3) }

{
parseError :: [Token] -> a
parseError tokens = error (show tokens)
}