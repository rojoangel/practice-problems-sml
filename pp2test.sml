use "pp2.sml";

(* tree_height tests *)
val test_tree_height_returns_zero_for_a_leaf =
    tree_height leaf = 0;
val test_tree_height_returns_one_for_a_one_level_tree =
    tree_height (node {value = 1, left = leaf, right = leaf}) = 1;
val test_tree_height_returns_two_for_a_two_levels_tree =
    tree_height (node {value = 1, left = leaf, right =
                (node {value = 2, left = leaf, right = leaf})}) = 2;
