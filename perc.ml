open Unix
open Printf
open Stdlib
open Arg

let rec help = ref false
and verbose = ref false
let percolate = ref 10
let seconds = ref 5 
let execfile = ref "/usr/bin/sox"
let infile = ref "perc-5s.wav"
let outfile = ref "perc-10s.wav"


let rec argmsg_help = "Display help message"
and argmsg_verbose = "Verbose logging"
and argmsg_percolate = "Seconds of percolations"
and argmsg_seconds = "Seconds of percolations per input file"
and argmsg_execfile = "Conversion command [Usually sox]"
and argmsg_infile = "Percolation input file"
and argmsg_outfile = "Percolation output file"

let vprintf fmt = 
    let printer msg = 
        if !verbose then 
            begin
                output_string stderr msg;
                output_string stderr "\n";
                flush stderr
            end
    in
    ksprintf printer fmt
;;

let arglist () = 
    [ ("-v", Arg.Set verbose, argmsg_verbose);
      ("--verbose", Arg.Set verbose, argmsg_verbose);
      ("-h", Arg.Set help, argmsg_help);
      ("--help", Arg.Set help, argmsg_help);
      ("-p", Arg.Set_int percolate, argmsg_percolate);
      ("--percolate", Arg.Set_int percolate, argmsg_percolate);
      ("-s", Arg.Set_int seconds, argmsg_seconds);
      ("--secs", Arg.Set_int seconds, argmsg_seconds);
      ("-x", Arg.Set_string execfile, argmsg_execfile);
      ("--exec", Arg.Set_string execfile, argmsg_execfile);
      ("-i", Arg.Set_string infile, argmsg_infile);
      ("--in", Arg.Set_string infile, argmsg_infile);
      ("-o", Arg.Set_string outfile, argmsg_outfile);
      ("--out", Arg.Set_string outfile, argmsg_outfile) ]
;;

let rec arg_parse () = 
    Arg.parse (arglist()) invalid_arg "Invalid Parameter";
    if !help then help_fun()
and invalid_arg s = 
    eprintf "%s Invalid argument [%s]" Sys.argv.(0) s
and help_fun () = 
    eprintf "%s [-v|--verbose] [-h|--help] [-s|--secs <seconds>] [-f|--file <percolate-file>] [-o|--out <output-file>] " Sys.argv.(0);
    eprintf "[-x|--exe <exe-file>]\n%!";
    exit (-2)
;;

let rec command_string () = 
    (createinfiles !percolate !execfile) ^ " " ^ !outfile 
and createinfiles secs cmd =
    vprintf "Command String secs[%d] cmd[%s]" secs cmd;
    match secs with
    | 0 -> cmd
    | secs -> createinfiles (secs - !seconds) (cmd ^ " " ^ !infile)

let run () = 
    let cmd = command_string() in
    vprintf "Command [%s]" cmd;
    match Sys.command cmd with
    | 0 ->
        vprintf "[%s] -> 0" cmd;
        printf "%s\n%!" !outfile
    | n ->
        vprintf "Command failed [%d]" n
;;

let rec main () = 
    arg_parse();
    run ()
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
