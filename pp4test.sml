use "pp4.sml";

(* gcd_list tests *)
val test_gcd_list_raises_Empty_for_empty_list =
    ((gcd_list [] = 1; false)
     handle Empty => true);
val test_gcd_list_returns_element_for_single_element_list =
    gcd_list [7] = 7;
val test_gcd_list_returns_1_for_list_with_no_cd =
    gcd_list [3, 5, 7, 11, 13] = 1;
val test_gcd_list_returns_gcd =
    gcd_list [1000, 500, 250, 100, 50] = 50;

(* any_divisible_by tests *)
val test_any_divisible_by_returns_false_for_empty_list =
    any_divisible_by ([], 2) = false;
val test_any_divisible_by_returns_false_for_a_single_not_divisible_number =
    any_divisible_by ([5], 2) = false;
val test_any_divisible_by_returns_true_for_a_single__divisible_number =
    any_divisible_by ([6], 2) = true;
val test_any_divisible_by_returns_true =
    any_divisible_by ([1,3,5,7,9,11,10], 2) = true;
val test_any_divisible_by_returns_false =
    any_divisible_by ([1,3,5,7,9,11,10], 23) = false;

(* add_opt tests *)
val test_add_opt_returns_NONE_if_first_arg_is_NONE =
    add_opt(NONE, SOME 10) = NONE;
val test_add_opt_returns_NONE_if_second_arg_is_NONE =
    add_opt(SOME 7, NONE) = NONE;
val test_add_opt_returns_sum_if_no_arg_is_NONE =
    add_opt(SOME 7, SOME 10) = SOME 17;

(* add_all_opt tests *)
val test_add_all_opt_returns_NONE_for_empty_list =
    add_all_opt([]) = NONE;
val test_add_all_opt_returns_NONE_for_NONE_list =
    add_all_opt([NONE]) = NONE;
val test_add_all_opt_returns_NONE_for_multi_NONE_list =
    add_all_opt([NONE, NONE, NONE]) = NONE;
val test_add_all_opt_returns_sum_for_not_all_NONE_list =
    add_all_opt([NONE, SOME 1, SOME 2, NONE, SOME 99]) = SOME 102;
