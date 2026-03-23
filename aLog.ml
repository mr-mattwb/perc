open Unix
open Printf
open Stdlib

module Ser =
  struct
    module type ELT = 
      sig
        type elt 
        val of_str : string -> elt
        val to_str : elt -> string
      end
    module Int = 
      struct
        type elt = int
        let of_str = int_of_string
        let to_str = string_of_int
      end
    module Str = 
      struct
        type elt = string
        let of_str s = s
        let to_str s = s
      end
  end

module Env =
  struct
    module type ELT =
      sig
        type elt
        val set : elt -> unit
        val get : unit -> elt
      end
    module type VAR =
      sig
        type elt
        val name : string
        val default : elt
        val descr : string
        val switches : string list
      end
    module type STR =
      sig
        val name : string
        val default : string
        val switches : string list
        val descr : string
      end
    module ArgMap = Map.Make(String)
    let gArgs = ref ArgMap.empty
    let add_switch sw spec = gArgs := ArgMap.add sw spec !gArgs

    module Make(S:Ser.ELT)(V:VAR with type elt = S.elt) : ELT with type elt = V.elt =
      struct
        type elt = V.elt
        let set_str v = Unix.putenv V.name v
        let set v = set_str (S.to_str v)
        let rec get () =
          try S.of_str (Unix.getenv V.name)
          with Not_found -> set V.default; get()
        let () = 
          let aux sw =
            let desc = sprintf "%s [%s] %s" sw V.name V.descr in
            add_switch sw (sw, Arg.String (fun v -> set_str v), desc)
          in
          List.iter aux V.switches
      end
    module MakeStr(S : STR) = Make(Ser.Str)(
        struct
          type elt = string
          include S
        end)

    let key_val_pat = "[\ \t]*(.*)[\ \t]*=[\ \t]*(.*)[\ \t]*"
    let parse_config line = 
        let ss = Pcre.get_substrings (Pcre.exec ~pat:key_val_pat line) in
        try
          Unix.putenv ss.(1) ss.(2)
        with e ->
          eprintf "%s\n%!" (Printexc.to_string e)

    let rec input_config_file fin = 
        match Tools.input_line fin with
        | None -> ()
        | Some line -> 
            (parse_config line; 
            input_config_file fin)

    let load_config_file fname = 
      try Tools.with_in_file input_config_file fname
      with e ->
        eprintf "ignore -- load_config_failure:  [%s] : %s\n%!" fname (Printexc.to_string e)

    module ConfigFile = MakeStr(
      struct
          let name = "CONFIG_FILE"
          let default = (Filename.chop_extension Tools.basename) ^ ".cfg"
          let descr = "Configuration file name"
          let switches = [ "--cfg-file" ]
      end)

    let speclist () = List.map snd (ArgMap.bindings !gArgs)
    let arg_parse anons msg = Arg.parse (speclist()) anons msg
    let defaults () = 
        let inv_arg v = 
            eprintf "%s:  Invalid argument [%s]\n%!" Tools.basename v;
            exit (-1)
        in
        arg_parse inv_arg "Invalid argument"
    let config_args anons = arg_parse anons "Invalid argument"

    let config ?(file=(ConfigFile.get())) () =
        load_config_file file;
        defaults()

  end

module I = Env.Make(Ser.Int)(
    struct
        type elt = int
        let name = "I"
        let default = 1
        let descr = "i"
        let switches = ["-i"]
    end) 

let main () = 
    Env.config();
    printf "I=[%d]\n%!" (I.get())

let () = 
    if not !Sys.interactive then main ()

      

