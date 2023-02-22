open Sys
open Unix
open List
open String

let header = "/home/slamko/.cache/header"
let table = "/home/slamko/.cache/table"

let str_regex = Str.regexp_string

let (>>=) f n = match f with 
  | Ok v -> n v
  | Error err -> Error err

let search_f search line =
  try
    let res = Str.search_forward search line 0 in
    Some res
  with Not_found -> None

let parse_lines all_lines =
  let rec parse  = function
    |h::t ->
      begin 
      match h with
      | '(' -> parse t |> parse
      | ')' -> h::t
      | _   -> parse t end
    | [] -> [] in
    
  all_lines
  |> String.to_seq
  |> List.of_seq
  |> parse 

let load_lines header_c line =
  let rec load all_lines =
    match parse_lines all_lines with
    | h::_ -> Ok all_lines
    | [] ->
       try
         (input_line header_c)
         |> cat "\n"
         |> cat all_lines
         |> load 
       with End_of_file -> Error "End of file reached" in 

  load line


let search_line table_c scall_name_reg =
  let rec read_line scall_name =
    try
      let line = input_line table_c in
      begin
        try
          Str.search_forward scall_name line 0 |> ignore;
          Ok (line)
        with Not_found -> read_line scall_name end
    with End_of_file ->
      close_in table_c;
      Error "Syscall not found" in
  scall_name_reg  |> str_regex |> read_line

let get_call_name table_file scall_name =
  let table_c = open_in table_file
  in match search_line table_c scall_name with
  | Ok call_data ->
     let call_l = split_on_char '\t' call_data
     in begin match List.hd call_l |> int_of_string_opt with
     | Some i -> Printf.printf "%d " i;
                 Ok (List.rev call_l |> List.hd)
     | None -> Error "Syscall not found in the table" end
  | Error e -> Error e

let get_args argv =
  match Array.length argv with
  | 2 -> Ok argv.(1)
  | _ -> Error "invalid args number, syscall name expected"

let cat_heads (str_l : string list) =
  tl str_l |> hd |> cat "." |> cat (hd str_l) 

let () =
  let uname_c = Unix.open_process_in "uname -r" in
  let kern_vers = input_line uname_c |> split_on_char '.' |> cat_heads |> cat "v" in

  "wget -q https://raw.githubusercontent.com/torvalds/linux/" ^ kern_vers ^ "/include/linux/syscalls.h -O ~/.cache/header" |> command |> ignore ;
  "wget -q https://raw.githubusercontent.com/torvalds/linux/" ^ kern_vers ^ "/arch/x86/entry/syscalls/syscall_64.tbl -O ~/.cache/table" |> command |> ignore ;

  let header_c = open_in header in

  let res =
    get_args argv
    >>= get_call_name table  
    >>= search_line header_c 
    >>= load_lines header_c

  in match res with
     | Ok call_str -> print_endline call_str
     | Error err -> Printf.eprintf "error: %s\n" err
