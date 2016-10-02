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

(* factorial tests *)
val test_factorial_0 = factorial 0 = 1;
val test_factorial_1 = factorial 1 = 1;
val test_factorial_2 = factorial 2 = 2;
val test_factorial_9 = factorial 9 = 362880;

(* unfold_map tests *)
val test_unfold_map_using_addition =
    unfold_map (fn x => x + 1) [1, 2, 3, 4, 5] = [2, 3, 4, 5, 6];
val test_unfold_map_passing_empty_list =
    unfold_map (fn x => x + 1) [] = [];
val test_unfold_map_using_string_concatenation =
    unfold_map (fn x => x ^ "!") ["Hey", "Ho", "Let's go"] = ["Hey!", "Ho!", "Let's go!"]

(* do_until tests *)
val test_do_until_1 = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 48 = 3;
val test_do_until_with_predicate_false_for_the_fisrt_iteration =
    do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 49 = 49;

(* imp_factorial tests *)
val test_imp_factorial_0 = imp_factorial 0 = 1;
val test_imp_factorial_1 = imp_factorial 1 = 1;
val test_imp_factorial_2 = imp_factorial 2 = 2;
val test_imp_factorial_9 = imp_factorial 9 = 362880;

(* fixed_point tests *)
val test_fixed_point_0 = fixed_point (fn x => x div 2) 17 = 0;
val test_fixed_point_1 = fixed_point (fn x => x) 17 = 17;

(* my_sqrt tests *)
val test_my_sqrt_1 = abs (my_sqrt 1.0 - Math.sqrt 1.0) < 0.01;
val test_my_sqrt_2 = abs (my_sqrt 2.0 - Math.sqrt 2.0) < 0.01;
val test_my_sqrt_7 = abs (my_sqrt 7.0 - Math.sqrt 7.0) < 0.01;
val test_my_sqrt_99 = abs (my_sqrt 99.0 - Math.sqrt 99.0) < 0.01;

(* tree_fold tests *)
val test_tree_fold_1 =
    tree_fold (fn (l,v,r) => l ^ v ^ r) "!" (node { value = "foo",
                                                    left = node { value = "bar",
                                                                  left = leaf,
                                                                  right = leaf},
                                                    right = node { value = "baz",
                                                                   left = leaf,
                                                                   right = leaf}})
    = "!bar!foo!baz!";
val test_tree_fold_2 =
    tree_fold (fn (l,v,r) => l * v * r) 1 leaf = 1;
val test_tree_fold_3 =
    tree_fold (fn (l, v, r) => v - l - r) 1 (node { value = 10,
                                                    left = node { value = 5,
                                                                  left = leaf,
                                                                  right = leaf },
                                                    right = node { value = 3,
                                                                   left = leaf,
                                                                   right = leaf }})
    = 6;

(* tree_unfold tests *)
val test_tree_unfold_1 =
    tree_unfold (fn x => if x = 0 then NONE else SOME (x - 1, x, x - 1)) 2 =
    node { value = 2,
           left = node { value = 1, left = leaf, right = leaf },
           right = node { value = 1, left = leaf, right = leaf }};
val test_tree_unfold_2 =
    tree_unfold (fn x => NONE) NONE = leaf;
val test_tree_unfold_3 =
    tree_unfold (fn x => if x = 0 then NONE else SOME (x div 2, x, x div 3)) 6 =
    node { value = 6,
           left = node { value = 3,
                         left = node { value = 1,
                                       left = leaf,
                                       right = leaf },
                         right = node { value = 1,
                                        left = leaf,
                                        right = leaf } },
           right = node { value = 2,
                          left = node { value = 1,
                                        left = leaf,
                                        right = leaf },
                          right = leaf }};

(* infer_type tests *)
val test_infer_type_1 =
    infer_type (conditional (
                     literal_bool,
                     literal_int,
                     binary_int_op (literal_int, literal_int))) = type_int;
val test_infer_type_2 =
    infer_type literal_int = type_int;
val test_infer_type_3 =
    infer_type literal_bool = type_bool;
val test_infer_type_4 =
    (infer_type (conditional (
                      literal_bool,
                      literal_int,
                      binary_int_op (
                          literal_bool,
                          literal_int))); false) handle TypeError => true = true;
val test_infer_type_5 =
    (infer_type (conditional (
                      literal_bool,
                      literal_bool,
                      binary_int_op (
                          literal_int,
                          literal_int))); false) handle TypeError => true = true;
val test_infer_type_6 =
    (infer_type (conditional (
                      literal_bool,
                      literal_int,
                      comparison (
                          literal_int,
                          literal_int))); false) handle TypeError => true = true;
val test_infer_type_7 =
    infer_type (conditional (
                     literal_bool,
                     literal_bool,
                     comparison (literal_int, literal_int))) = type_bool;
val test_infer_type_8 =
    infer_type (conditional (
                     literal_bool,
                     conditional (
                         literal_bool,
                         literal_bool,
                         comparison (
                             literal_int,
                             binary_int_op (
                                 literal_int, literal_int))),
                     comparison (literal_int, literal_int))) = type_bool;
val test_infer_type_9 =
    infer_type (conditional (
                     literal_bool,
                     binary_bool_op (
                         literal_bool,
                         comparison (
                             literal_int,
                             literal_int)),
                     comparison (literal_int, literal_int))) = type_bool;
val test_infer_type_10 =
    infer_type (binary_int_op (literal_int, literal_int)) = type_int;
val test_infer_type_11 =
    (infer_type (binary_int_op (literal_int, literal_bool)); false)
    handle TypeError => true = true;
val test_infer_type_12 =
    (infer_type (binary_bool_op (literal_int, literal_bool)); false)
    handle TypeError => true = true;
val test_infer_type_13 =
    (infer_type (comparison (literal_int, literal_bool)); false)
    handle TypeError => true = true;
val test_infer_type_14 =
    infer_type (conditional (
                     binary_bool_op (
                         literal_bool,
                         literal_bool),
                     literal_bool,
                     comparison (
                         literal_int,
                         literal_int))) = type_bool;
val test_infer_type_15 =
    infer_type (conditional (
                     comparison (
                         literal_int, literal_int),
                     literal_bool, comparison (
                         literal_int, literal_int))) = type_bool;
val test_infer_type_16 =
    (infer_type (conditional (
                      binary_int_op (
                          literal_int,
                          literal_int),
                      literal_bool,
                      comparison (
                          literal_int,
                          literal_int))); false)
    handle TypeError => true = true;
