use "pp4.sml";

(* gcd_list tests *)
val test_gcd_list_returns_1_for_empty_list =
    gcd_list [] = 1;
val test_gcd_list_returns_element_for_single_element_list =
    gcd_list [7] = 7;
val test_gcd_list_returns_1_for_list_with_no_cd =
    gcd_list [3, 5, 7, 11, 13] = 1;
val test_gcd_list_returns_gcd =
    gcd_list [1000, 500, 250, 100, 50] = 50;
