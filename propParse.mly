%{
open Unix
open Printf
open Stdlib
%}
%token <string> KEY
%token <string> IDENT
%token EOF
%start main
%type <IniBase.t> main
%%
main:
    | KEY IDENT             { IniBase.Pair ($1, $2) }
    | KEY EOF               { IniBase.Pair ($1, "") }
    | EOF                   { IniBase.Eof }
;
%%

