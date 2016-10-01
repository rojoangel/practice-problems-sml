use "pp1.sml";

(* fold_map tests *)
val test_fold_map = fold_map (fn x => x + 1) [1, 2, 3, 4 ,5] = [2, 3 ,4 ,5 ,6];

(* fold_filter tests *)
val test_fold_filter = fold_filter (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] = [2, 4];

(* unfold tests *)
val test_unfold_unfolds_numeric_list =
    unfold (fn x => if x > 3 then NONE else SOME (x + 1, x)) 0 = [0, 1, 2, 3];
val test_unfold_returns_empty_list_when_f_returns_NONE_for_initial_state =
    unfold (fn x => if x > 3 then NONE else SOME (x + 1, x)) 4 = [];
val test_unfold_works_with_strings_too =
    unfold (fn s => if String.size s < 9 then SOME (s ^ "Hoho", s) else NONE) "" = ["", "Hoho", "HohoHoho"];

(* factorial tests*)
val test_factorial_0 = factorial 0 = 1;
val test_factorial_1 = factorial 1 = 1;
val test_factorial_2 = factorial 2 = 2;
val test_factorial_9 = factorial 9 = 362880;

(* unfold_map tests *)
val test_unfold_map_using_addition =
    unfold_map (fn x => x + 1) [1, 2, 3, 4, 5] = [2, 3, 4, 5, 6];
val test_unfold_map_passing_empty_list =
    unfold_map (fn x => x + 1) [] = [];
val test_unfold_map_using_string_concatenation =
    unfold_map (fn x => x ^ "!") ["Hey", "Ho", "Let's go"] = ["Hey!", "Ho!", "Let's go!"]

(* do_until tests *)
val test_do_until_1 = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 48 = 3;
val test_do_until_with_predicate_false_for_the_firts_iteration =
    do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 49 = 49;
