use "pp1.sml";

(* pass_or_fail tests *)
val test_pass_or_fail_fails_for_NONE_grade =
    pass_or_fail {grade = NONE, id = 12} = fail;
val test_pass_or_fail_fails_for_SOME_grade_lesser_than_74 =
    pass_or_fail {grade = SOME 74, id = 12} = fail;
val test_pass_or_fail_passes_for_SOME_grade_equal_to_75 =
    pass_or_fail {grade = SOME 75, id = 12} = pass;
val test_pass_or_fail_passes_for_SOME_grade_greater_than_75 =
    pass_or_fail {grade = SOME 76, id = 12} = pass;

(* has_passed_ tests *)
val test_not_has_passed_for_NONE_grade =
    has_passed {grade = NONE, id = 12} = false;
val test_not_has_passed_for_SOME_grade_lesser_than_74 =
    has_passed {grade = SOME 74, id = 12} = false;
val test_has_passed_for_SOME_grade_equal_to_75 =
    has_passed {grade = SOME 75, id = 12} = true;
val test_has_passes_for_SOME_grade_greater_than_75 =
    has_passed {grade = SOME 76, id = 12} = true;

(* number_passed tests *)
val test_number_passed_returns_zero_for_empty_list =
    number_passed [] = 0;
val test_number_passed_returns_zero_when_no_passing_grades =
    number_passed [{grade = SOME 74, id = 11},
                   {grade = NONE, id = 12},
                   {grade = SOME 0, id = 13}] = 0;
val test_number_passed_returns_count_of_passing_grades =
    number_passed [{grade = SOME 74, id = 11},
                   {grade = SOME 75, id = 12},
                   {grade = NONE, id = 13},
                   {grade = SOME 76, id = 14},
                   {grade = SOME 0, id = 15},
                   {grade = SOME 80, id = 16}] = 3;
