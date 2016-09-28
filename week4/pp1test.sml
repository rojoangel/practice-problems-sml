use "pp1.sml";

(* fold_map tests *)
val test_fold_map = fold_map (fn x => x + 1) [1, 2, 3, 4 ,5] = [2, 3 ,4 ,5 ,6];

(* fold_filter tests *)
val test_fold_filter = fold_filter (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] = [2, 4];
