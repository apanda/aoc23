open Base
open Stdio
let digits = List.map ~f:(fun d -> (Int.to_string d), d) (List.range 0 10);;
let words = List.zip_exn ["one"; "two"; "three"; "four"; "five"; "six";
                      "seven"; "eight"; "nine"] (List.range 1 10);;
let all = List.concat [digits; words];;
let pattern = let pat_str = String.concat ~sep:"|" 
        (List.map ~f:(fun s -> Printf.sprintf "(%s)" (fst s)) all) in
    Re2.create_exn pat_str;;
let lookup_map = Map.of_alist_exn (module String) all;;
let num_for_string s = 
    let matches = Re2.find_all_exn pattern s in
    let first = Map.find_exn lookup_map (List.hd_exn matches) in
    let last = Map.find_exn lookup_map (List.hd_exn (List.rev matches)) in
    first * 10 + last;;

let show_file filename =
    let s = In_channel.with_file filename ~f:(fun file ->
        List.fold (List.map ~f:(fun line -> num_for_string line)
        (In_channel.input_lines file)) ~init:0 ~f:(+)) in
    printf "R: %d\n" s;;

show_file (Sys.get_argv ()).(1);;
