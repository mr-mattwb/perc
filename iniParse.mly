%{
open Unix
open Printf
open Stdlib
%}
%token <string> CONTEXT
%token <string> KEY
%token <string> RESULT
%token <string * string> PAIR
%token EOLN EOF
%start main
%type <IniBase.t> main
%%
main:
    | CONTEXT               { IniBase.Context $1 }
    | KEY RESULT            { IniBase.Pair ($1, $2) }
    | EOF                   { IniBase.Eof }
;
%%
