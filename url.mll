{
open Unix
open Printf
open Stdlib

} 

let dig = ['0'-'9']
let dig1 = ['0'-'1']
let dig4 = ['0'-'4']
let dig5 = ['0'-'5']

let dig199 = dig1? dig dig
let dig99 = dig
let dig255 = '2' ('5' dig5 | dig4 dig) | dig199 | dig
let ip = dig255 "." dig255 "." dig255 "." dig255
let domain_type = "com"|"net"|"org"|"edu"|"biz"|"us"
let domst = ['A'-'Z' 'a'-'z' ] 
let domch = [ 'A'-'Z' 'a'-'z' '0'-'9' '.' '_' '-' ]
let domain_name = domst domch* 
let domain = domain_name "." domain_type
let protocol = ['A'-'Z' 'a'-'z' ]+
let pageElts = ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '%' '/' '.' ]

rule host = parse
    | ip as ip                      { `IP ip }
    | domain as dom                  { `Host dom }

and pages = parse
    | pageElts+ as pgs              { `Pages pgs } 

and protocol = parse
    | protocol as prot ':' ('/'+)?  { `Protocol prot }

and uri = parse                     
    | (protocol as prot) ':' ('/'+)? ((ip | domain) as host) (pageElts+ as pages)       {
        prot, host, pages
    }

{
let of_string s = uri (Lexing.from_string s)
}
