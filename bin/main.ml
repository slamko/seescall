open Sys
open Unix
open List

let header = "syscalls.h"
let table = "syscall_64.tbl"

let str_regex = Str.regexp_string

let (>>=) f n = match f with 
  | Ok v -> n v
  | Error err -> Error err

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
         all_lines
            |> String.cat (input_line header_c)
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
      Error "End of file" in
  scall_name_reg  |> str_regex |> read_line

let get_call_name table_file scall_name =
  let table_c = open_in table_file in
  match search_line table_c scall_name with
  | Ok call_data ->
     let call_l = String.split_on_char ' ' call_data in
     List.hd call_l |> print_string ;
     Ok (List.rev call_l |> List.hd)
  | Error e -> Error e

let cat_heads (str_l : string list) =
  tl str_l |> hd |> String.cat (hd str_l)

let () =
  let uname_c = Unix.open_process_in "uname -r" in
  let kern_vers = input_line uname_c |> String.split_on_char '.' |> cat_heads in
  "wget https://raw.githubusercontent.com/torvalds/linux/" ^ kern_vers ^ "/include/linux/syscalls.h -o ~/.cache/header" |> command |> ignore ;
  "wget https://raw.githubusercontent.com/torvalds/linux/" ^ kern_vers ^ "/arch/x86/entry/syscalls/syscall_64.tbl -o ~/.cache/table" |> command |> ignore ;

  let header_c = open_in header in

  let res =
    get_call_name table argv.(1) 
    >>= search_line header_c 
    >>= load_lines header_c

  in match res with
     | Ok call_str -> print_endline call_str
     | Error err -> print_endline err
