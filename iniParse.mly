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
%type <PropBase.t> main
%%
main:
    | CONTEXT               { PropBase.Context $1 }
    | KEY RESULT            { PropBase.Pair ($1, $2) }
    | EOF                   { PropBase.Eof }
;
%%
