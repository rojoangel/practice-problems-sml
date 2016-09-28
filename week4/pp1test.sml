use "pp1.sml";

(* fold_map tests *)
val test_fold_map = fold_map (fn x => x + 1) [1, 2, 3, 4 ,5] = [2, 3 ,4 ,5 ,6];
