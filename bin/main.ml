open Sys
open Unix
open List
open String

let header = "header" |> (^) "/.cache/" |> (^) (getenv "HOME")
let table  = "table" |> (^) "/.cache/" |> (^) (getenv "HOME")

let str_regex = Str.regexp_string

let (>>=) f n = match f with 
  | Ok v -> n v
  | Error err -> Error err

let search_f search line =
  try
    let pos = Str.search_forward search line 0 in
    Some pos
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
    | _::_ -> Ok all_lines
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
        match search_f scall_name line with
        | Some _ -> Ok line
        | None -> read_line scall_name end
    with End_of_file ->
      close_in table_c;
      Error "Syscall not found" in
  scall_name_reg  |> str_regex |> read_line

let lookup_syscall table_c scall_name =
  let rec lookup () =
    match search_line table_c scall_name with
    | Ok line ->
       let str_i = Str.search_forward (str_regex scall_name) line 0 in
       begin match line.[str_i + length scall_name] with
       | '(' -> Ok line
       | _ -> lookup () end
    | Error err -> Error err in

  lookup ()

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
  | _ -> Error "invalid args number, syscall name or number expected"

let cat_heads (str_l : string list) =
  tl str_l |> hd |> cat "." |> cat (hd str_l) 

let get_kern_version uname_c =
  input_line uname_c |> split_on_char '.' |> cat_heads |> cat "v"

let load_table kern_v =
  "wget -q https://raw.githubusercontent.com/torvalds/linux/" ^ kern_v ^ "/arch/x86/entry/syscalls/syscall_64.tbl -O ~/.cache/table" ^ kern_v
  |> command 

let load_header kern_v =
  "wget -q https://raw.githubusercontent.com/torvalds/linux/" ^ kern_v ^ "/include/linux/syscalls.h -O ~/.cache/header" ^ kern_v
  |> command
  
let load_cache kern_v =
  let res = match (header ^ kern_v |> file_exists, table ^ kern_v |> file_exists) with
  | (true, true) -> 0
  | (true, false) -> load_table kern_v
  | (false, true) -> load_header kern_v
  | (false, false) -> (load_table kern_v) + (load_header kern_v) in

  match res with
  | 0 -> Ok ()
  | _ -> Error "failed to fetch the kernel data"
  
let perror err =
  Printf.eprintf "error: %s\n" err

let clean c_in res =
  close_in c_in ;
  Ok res

let seescall () =
  let header_c = open_in header in

  let res =
    get_args argv
    >>= get_call_name table  
    >>= lookup_syscall header_c 
    >>= load_lines header_c
    >>= clean header_c

  in match res with
     | Ok call_str -> print_endline call_str
     | Error err -> perror err

let () =
  let uname_c = Unix.open_process_in "uname -r" in
  let kern_vers = get_kern_version uname_c in
  let _ = Unix.close_process_in uname_c in

  match load_cache kern_vers with
  | Ok () -> seescall ()
  | Error err -> perror err
 
