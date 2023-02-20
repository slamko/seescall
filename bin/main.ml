open Sys

let tbl_file = "syscalls.h"


let parse_line table_c scal_name line =
  let rec parse scal_name_reg acc_lines =
    String.iter
      (fun c ->
        match c with
        | '(' -> line :: acc_lines |> parse scal_name
        | ')' -> acc_lines


  parse (Str.regexp_string scall_name) []

let read table_c (scall_name_reg: string) =
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
      Error () in

  Str.regexp_string scall_name_reg |> read_line


let () =
  let table_c = open_in tbl_file in
  read table_c Sys.argv.(1) |> print_endline
