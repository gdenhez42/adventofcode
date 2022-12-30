(*let average a b =
  (a +. b) /. 2.0
let average_str a =
  Float.to_string a*)

type blueprint = {
  ore: int;
  clay: int;
  obsidian: int*int;
  geode: int*int;
};;

type inventory = {
  ore: int;
  clay: int;
  obsidian: int;
  geode: int;
  ore_robot: int;
  clay_robot: int;
  obsidian_robot: int;
  geode_robot: int;
};;

let _debug_blueprint (b: blueprint) =
  Format.printf "%d %d (%d %d) (%d %d)\n" b.ore b.clay (fst b.obsidian) (snd b.obsidian) (fst b.geode) (snd b.geode);;

let _debug_inventory (i: inventory) =
  Format.printf "resources: ore: %d clay: %d obsidian: %d geode: %d -- robots: ore: %d clay: %d obsidian: %d geode: %d\n"
  i.ore i.clay i.obsidian i.geode i.ore_robot i.clay_robot i.obsidian_robot i.geode_robot;;

let build_ore_robot (b: blueprint) (i: inventory) =
  {
    ore = i.ore - b.ore;
    clay = i.clay;
    obsidian = i.obsidian;
    geode = i.geode;
    ore_robot = i.ore_robot + 1;
    clay_robot = i.clay_robot;
    obsidian_robot = i.obsidian_robot;
    geode_robot = i.geode_robot;
  }

let build_clay_robot (b: blueprint) (i: inventory) =
  {
    ore = i.ore - b.clay;
    clay = i.clay;
    obsidian = i.obsidian;
    geode = i.geode;
    ore_robot = i.ore_robot;
    clay_robot = i.clay_robot + 1;
    obsidian_robot = i.obsidian_robot;
    geode_robot = i.geode_robot;
  }

let build_obsidian_robot (b: blueprint) (i: inventory) =
  {
    ore = i.ore - (fst b.obsidian);
    clay = i.clay - (snd b.obsidian);
    obsidian = i.obsidian;
    geode = i.geode;
    ore_robot = i.ore_robot;
    clay_robot = i.clay_robot;
    obsidian_robot = i.obsidian_robot + 1;
    geode_robot = i.geode_robot;
  }

let build_geode_robot (b: blueprint) (i: inventory) =
  {
    ore = i.ore - (fst b.geode);
    clay = i.clay;
    obsidian = i.obsidian - (snd b.geode);
    geode = i.geode;
    ore_robot = i.ore_robot;
    clay_robot = i.clay_robot;
    obsidian_robot = i.obsidian_robot;
    geode_robot = i.geode_robot + 1;
  }

let read_input () =
  let lines = Arg.read_arg "input.txt" in
  let read_cost = fun parts part_nb -> int_of_string (List.nth parts part_nb) in
  Array.map (fun l ->
    let parts = (String.split_on_char ' ' l) in
    {
      ore = (read_cost parts 6);
      clay = (read_cost parts 12);
      obsidian = ((read_cost parts 18), (read_cost parts 21));
      geode = ((read_cost parts 27), (read_cost parts 30))
    }) lines;;


let turn (b: blueprint) (i: inventory) =
  let i2 : inventory =
    {
      ore = i.ore_robot + i.ore;
      clay = i.clay_robot + i.clay;
      obsidian = i.obsidian_robot + i.obsidian;
      geode = i.geode_robot + i.geode;
      ore_robot = i.ore_robot;
      clay_robot = i.clay_robot;
      obsidian_robot = i.obsidian_robot;
      geode_robot = i.geode_robot;
    } in
  let max_ore_robots = List.fold_left max 0 [b.ore; b.clay; (fst b.obsidian); (fst b.geode)] in
  let max_clay_robots = (snd b.obsidian) in
  let max_obsidian_robots = (snd b.geode) in
  let maybe_build_nothing = [i2] in
  let maybe_add_ore_robot =
    if i.ore >= b.ore && i.ore_robot < max_ore_robots then
      (build_ore_robot b i2) :: maybe_build_nothing
    else
      maybe_build_nothing in
  let maybe_add_clay_robot =
    if i.ore >= b.clay && i.clay_robot < max_clay_robots then
      (build_clay_robot b i2) :: maybe_add_ore_robot
    else
      maybe_add_ore_robot in
  let maybe_add_obsidian_robot =
    if i.ore >= (fst b.obsidian) && i.clay >= (snd b.obsidian) && i.obsidian_robot < max_obsidian_robots then
      (build_obsidian_robot b i2) :: maybe_add_clay_robot
    else
      maybe_add_clay_robot in
  let maybe_add_geode_robot =
    if i.ore >= (fst b.geode) && i.obsidian >= (snd b.geode) then
      (build_geode_robot b i2) :: maybe_add_obsidian_robot
    else
      maybe_add_obsidian_robot in
  maybe_add_geode_robot

let is_different_or_more_resource (elem: inventory) (elem2: inventory) =
  elem2.ore_robot != elem.ore_robot ||
  elem2.clay_robot != elem.clay_robot ||
  elem2.obsidian_robot != elem.obsidian_robot ||
  elem2.geode_robot != elem.geode_robot ||
  elem2.ore < elem.ore ||
  elem2.clay < elem.clay ||
  elem2.obsidian < elem.obsidian ||
  elem2.geode < elem.geode

let is_different_or_more_robots (elem: inventory) (elem2: inventory) =
  elem2.ore_robot < elem.ore_robot ||
  elem2.clay_robot < elem.clay_robot ||
  elem2.obsidian_robot < elem.obsidian_robot ||
  elem2.geode_robot < elem.geode_robot ||
  elem2.ore != elem.ore ||
  elem2.clay != elem.clay ||
  elem2.obsidian != elem.obsidian ||
  elem2.geode != elem.geode

let rec filter_worst_results (is: inventory list) (result: inventory list) =
  match is with
  | [] -> result
  | elem :: rst ->
    if List.for_all (fun elem2 -> 
      (is_different_or_more_resource elem elem2) && (is_different_or_more_robots elem elem2)) (List.append rst result) then
        (filter_worst_results rst (elem :: result)) else (filter_worst_results rst result)

let filter_bad_result (is: inventory list) =
  filter_worst_results is []

let rec turns min (b: blueprint) (is: inventory list) =
  match min with
  | 0 -> is
  | _ -> turns (min-1) b (filter_bad_result (List.concat (List.map (turn b) is)))

let _quality_level_part1 (b: blueprint) =
  let potential_strats = (turns 24 b [{ore = 0; clay = 0; obsidian = 0; geode = 0; ore_robot = 1; clay_robot = 0; obsidian_robot = 0; geode_robot = 0}]) in
  List.fold_left max 0 (List.map (fun elem -> elem.geode) potential_strats)

let quality_level_part2 (b: blueprint) =
  let potential_strats = (turns 32 b [{ore = 0; clay = 0; obsidian = 0; geode = 0; ore_robot = 1; clay_robot = 0; obsidian_robot = 0; geode_robot = 0}]) in
  List.fold_left max 0 (List.map (fun elem -> elem.geode) potential_strats)

let _total_quality (input : blueprint array) =
  let tpl = Array.fold_left (fun acc elem -> ((fst acc) + 1, (snd acc) + (fst acc)*(_quality_level_part1 elem))) (1, 0) input in
  (snd tpl)

let () =
  Format.printf "%d\n" (quality_level_part2 (read_input ()).(0));;
