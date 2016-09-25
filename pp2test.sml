use "pp2.sml";

(* tree_height tests *)
val test_tree_height_returns_zero_for_a_leaf =
    tree_height leaf = 0;
val test_tree_height_returns_one_for_a_one_node_tree =
    tree_height (node {value = 1, left = leaf, right = leaf}) = 1;
val test_tree_height_returns_two_for_a_two_node_tree =
    tree_height (node {value = 1, left = leaf, right =
                (node {value = 2, left = leaf, right = leaf})}) = 2;

(** sum_tree tests *)
val test_sum_tree_returns_zero_for_a_leaf =
    sum_tree leaf = 0;
val test_sum_tree_returns_value_for_a_single_node_tree =
    sum_tree (node {value = 99, left = leaf, right = leaf}) = 99;
val test_sum_tree_returns_sum_of_values_for_a_two_node_tree =
    sum_tree (node {value = 25, left = leaf, right =
             (node {value = 50, left = leaf, right = leaf})}) = 75;

(* gardener tests *)
val test_gardener_returns_leaf_for_a_leaf =
    gardener leaf = leaf;
val test_gardener_respects_leave_me_alone_node =
    gardener (node {value = leave_me_alone, left = leaf, right = leaf}) =
    (node {value = leave_me_alone, left = leaf, right = leaf});
val test_gardener_prunes_prune_me_node =
    gardener (node {value = prune_me, left = leaf, right = leaf}) = leaf;
val test_gardener_prunes_deep_prune_me_nodes =
    gardener (node {value = leave_me_alone,
                    left = (node {value = leave_me_alone,
                                  left = (node { value = prune_me,
                                                 left = leaf,
                                                 right = leaf}),
                                  right = leaf}),
                    right = leaf}) = (node {value = leave_me_alone,
                                            left = (node {value = leave_me_alone,
                                                          left = leaf,
                                                          right = leaf}),
                                            right = leaf});
