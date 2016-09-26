use "pp3.sml";

(* is_positive tests *)
val test_is_positive_returns_false_for_ZERO =
    is_positive ZERO = false;
val test_is_positive_returns_true_for_others =
    is_positive (SUCC ZERO) = true;

(* pred tests *)
val test_pred_raises_Negative_for_ZERO =
    ((pred ZERO; false)
     handle Negative => true);
val test_pred_returns_predecessor_for_non_ZERO =
    pred (SUCC (SUCC (SUCC ZERO))) = (SUCC (SUCC ZERO));

(* nat_to_int tests*)
val test_nat_to_int_returns_0 =
    nat_to_int ZERO = 0;
val test_nat_to_int_returns_1 =
    nat_to_int (SUCC(ZERO)) = 1;
val test_nat_to_int_returns_2 =
    nat_to_int (SUCC(SUCC(ZERO))) = 2;
val test_nat_to_int_returns_5 =
    nat_to_int (SUCC(SUCC(SUCC(SUCC(SUCC(ZERO)))))) = 5;
