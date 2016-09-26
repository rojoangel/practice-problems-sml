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

(* int_to_nat tests *)
val test_int_to_nat_converts_0 =
    int_to_nat 0 = ZERO;
val test_int_to_nat_converts_1 =
    int_to_nat 1 = SUCC(ZERO);
val test_int_to_nat_converts_2 =
    int_to_nat 2 = SUCC(SUCC(ZERO));
val test_int_to_nat_converts_5 =
    int_to_nat 5 = SUCC(SUCC(SUCC(SUCC(SUCC(ZERO)))));

(* add tests *)
val test_add_ZERO_to_ZERO_returns_ZERO =
    add (ZERO, ZERO) = ZERO;
val test_add_ZERO_to_something_returns_something =
    add (ZERO, SUCC(ZERO)) = SUCC(ZERO);
val test_add_something_to_ZERO_returns_something =
    add (SUCC(SUCC(ZERO)), ZERO) = SUCC(SUCC(ZERO));
val test_add_adds_non_ZEROS =
    add (SUCC(SUCC(ZERO)), SUCC(SUCC(SUCC(SUCC(ZERO))))) = SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))));

(* sub tests *)
val test_sub_ZERO_from_ZERO_returns_ZERO =
    sub (ZERO, ZERO) = ZERO;
val test_sub_anything_from_ZERO_raises_Negative =
    ((sub (ZERO, SUCC(ZERO)); false)
         handle Negative => true)
val test_sub_ZERO_from_anything_returns_anything =
    sub(SUCC(SUCC(ZERO)), ZERO) = SUCC(SUCC(ZERO));
val test_sub_substracts_non_ZEROS =
    sub(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))), SUCC(SUCC(ZERO))) = SUCC(SUCC(SUCC(ZERO)));

(* mult tests *)
val test_mult_anything_by_ZERO_returns_ZERO =
    mult (SUCC(ZERO), ZERO) = ZERO;
val test_mult_ZERO_by_anything_returns_ZERO =
    mult (ZERO, SUCC(ZERO)) = ZERO;
val test_mult_multiplies_non_ZEROs =
    mult (SUCC(SUCC(SUCC(ZERO))), SUCC(SUCC(ZERO))) = SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))));
