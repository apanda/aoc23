open Base
open Stdio

(* Pattern for getting Game ID *)
let game_pattern = "Game (?P<gid>\\d+)";;
let game_aut = Re2.create_exn game_pattern;;
(* Pattern for getting descriptions *)
let descriptions = "(?P<count>\\d+) (?P<color>red|green|blue)";;
let desc_aut = Re2.create_exn descriptions;;

let _cubes = Map.of_alist_exn (module String) 
                ["red", 12; "green", 13; "blue", 14];;

let extract_pair m =
    let color = Option.value_exn (Re2.Match.get m ~sub:(`Name "color")) in
    let  count = Int.of_string 
        (Option.value_exn (Re2.Match.get m ~sub:(`Name "count"))) in
    (color, count);;

let pair_is_valid (color, count) =
    (Map.find_exn _cubes color) >= count;; 

let validate_game game = 
    let _cubes = List.map ~f:extract_pair 
         (Re2.get_matches_exn desc_aut (String.strip game)) in
    List.fold (List.map ~f:pair_is_valid _cubes) ~init:true ~f:(&&);;

let get_gid g =
    let g = Re2.get_matches_exn game_aut (String.strip g) in
    let g = Option.value_exn (List.hd g) in
    let gid = Option.value_exn (Re2.Match.get g ~sub:(`Name "gid")) in
    Int.of_string gid;;

let get_id_valid line =
    let split = String.split ~on:':' line in
    let gid = List.hd_exn split in
    let gid = get_gid gid in
    let games = List.hd_exn (List.tl_exn split) in
    let games = String.split ~on:';' games in
    let validity = List.map ~f:validate_game games in
    gid, (List.fold validity ~init:true ~f:(&&));;

let update_map m game =
    let pairs = List.map ~f:extract_pair 
         (Re2.get_matches_exn desc_aut (String.strip game)) in
    List.fold pairs 
        ~f: (fun map g -> (Map.change map (fst g) 
            ~f: (fun v -> if (Option.value_exn v) < (snd g) 
                          then (Some (snd g))
        else v))) ~init: m;;

let unit_cubes = Map.of_alist_exn (module String) 
                ["red", 0; "green", 0; "blue", 0];;

let get_power line =
    let split = String.split ~on:':' line in
    let games = List.hd_exn (List.tl_exn split) in
    let games = String.split ~on:';' games in
    let umap = List.fold games ~f:update_map ~init: unit_cubes in
    let red = (Map.find_exn umap "red") in 
    let blue = (Map.find_exn umap "blue") in 
    let green = (Map.find_exn umap "green") in
    red * blue * green;;
    

(* Part 1 *)
let _compute_file_1 file = 
    let all_lines = List.map ~f:get_id_valid
            (In_channel.input_lines file) in
    let valid_lines = List.filter ~f:snd all_lines in
    let valid_ids = List.map ~f:fst valid_lines in
    List.fold valid_ids ~init:0 ~f:(+);;

(* Part 2 *)
let compute_file_2 file = 
    let power = List.map ~f:get_power
            (In_channel.input_lines file) in
    List.fold power ~init:0 ~f:(+);;

let process_file filename =
    let s = In_channel.with_file filename ~f:compute_file_2 in
    printf "R: %d\n" s;;

process_file (Sys.get_argv ()).(1);;
