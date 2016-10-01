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
