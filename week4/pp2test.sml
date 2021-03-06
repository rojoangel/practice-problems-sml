use "pp2.sml";

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

(* add_all_opt tests *)
val test_add_all_opt_returns_NONE_for_empty_list =
    add_all_opt([]) = NONE;
val test_add_all_opt_returns_NONE_for_NONE_list =
    add_all_opt([NONE]) = NONE;
val test_add_all_opt_returns_NONE_for_multi_NONE_list =
    add_all_opt([NONE, NONE, NONE]) = NONE;
val test_add_all_opt_returns_sum_for_not_all_NONE_list =
    add_all_opt([NONE, SOME 1, SOME 2, NONE, SOME 99]) = SOME 102;

(* alternate tests *)
val test_alternate_returns_0_for_empty_list =
    alternate [] = 0;
val test_alternate_returns_value_for_single_value_list =
    alternate [1] = 1;
val test_alternate_returns_sum_for_two_elements_list =
    alternate [1, 2] = ~1;
val test_alternate_alternates_for_three_elements_list =
    alternate [1, 2, 3] = 2;
val test_alternate_alternates_for_long_list =
    alternate [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = ~5;

(* min_max tests *)
val test_min_max_raises_empty_for_empty_list =
    ((min_max []; false)
     handle e => true);
val test_min_max_returns_value_for_single_value_list =
    min_max [4] = (4, 4);
val test_min_max_returns_min_max_for_two_value_list =
    min_max [99, 1] = (1, 99);
val test_min_max_returs_min_max_for_long_list =
    min_max [10, 56, 99, 78, 25, 19, 9, 76, 56, 23, 9, 77] = (9, 99);

(* unzip tests *)
val test_unzip_unzips_empty_list =
    unzip [] = ([], []);
val test_unzip_unzips_single_element_list =
    unzip [(1, 2)] = ([1], [2]);
val test_unzip_unzips_two_element_list =
    unzip [(1, 2), (3, 4)] = ([1, 3], [2, 4]);
val test_unzip_unzips_long_list =
    unzip [(1, 2), (3, 4), (5, 6), (7, 8)] = ([1, 3, 5, 7], [2, 4, 6, 8]);

(* zip tests *)
val test_zip_1 = zip ([1, 2, 3], [4, 6]) = [(1, 4), (2, 6)];
val test_zip_2 = zip ([], [4, 6]) = [];
val test_zip_3 = zip ([1, 2, 3], []) = [];
val test_zip_4 = zip ([], []) = [];
val test_zip_5 = zip ([1, 2], [4, 6, 8]) = [(1, 4), (2, 6)];
val test_zip_6 = zip ([1, 2, 3], [4, 6, 8]) = [(1, 4), (2, 6), (3, 8)];

(* repeats_list tests *)
val test_repeats_list_shortens_list_for_length_mismatch =
    repeats_list (["a"], []) = [];
val test_repeats_list_returns_empty_list_for_empty_lists =
    repeats_list ([], []) = [];
val test_repeats_list_repeats_according_to_second_parameter =
    repeats_list (["a", "b", "c", "d", "e", "f", "g"], [1 ,2, 3, 4, 3, 2, 1]) =
    ["a", "b", "b", "c", "c", "c", "d", "d", "d", "d", "e", "e", "e", "f", "f", "g"];

(* length_of_a_list tests *)
val test_length_of_a_list_1 = length_of_a_list [1] = 1;
val test_length_of_a_list_2 = length_of_a_list [] = 0;
val test_length_of_a_list_3 = length_of_a_list [[], [], [1, 2]] = 3;
val test_length_of_a_list_4 = length_of_a_list [(1, "hi"), (2, "there")] = 2;
val test_length_of_a_list_5 = length_of_a_list ["a", "quick", "brown", "fox"] = 4;
