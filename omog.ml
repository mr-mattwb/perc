open Unix
open Printf
open Stdlib

let () =
  if not !Sys.interactive then OLog.main ()