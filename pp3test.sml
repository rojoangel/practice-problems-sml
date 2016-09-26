use "pp3.sml";

(* is_positive tests *)
val test_is_positive_returns_false_for_ZERO =
    is_positive ZERO = false;
val test_is_positive_returns_true_for_others =
    is_positive (SUCC ZERO) = true;
