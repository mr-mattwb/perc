%{
open Unix
open Printf
open Stdlib
%}
%token <string> KEY
%token <string> IDENT
%token EOF
%start main
%type <PropBase.t> main
%%
main:
    | KEY IDENT             { PropBase.Pair ($1, $2) }
    | KEY EOF               { PropBase.Pair ($1, "") }
    | EOF                   { PropBase.Eof }
;
%%

