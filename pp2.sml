datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* Write a function tree_height that accepts an 'a tree and evaluates to a
 height of this tree. The height of a tree is the length of the longest path
 to a leaf. Thus the height of a leaf is 0. *)
fun tree_height t =
  case t of
      leaf => 0
    | node {value = _, left = tl, right = tr} => 1 + let
          val left_height = tree_height tl
          val right_height = tree_height tr
      in
          if (left_height > right_height)
          then left_height
          else right_height
      end

(* Write a function that takes an and evaluates to the sum of all values in the nodes. *)
fun sum_tree t =
  case t of
      leaf => 0
    | node {value = i, right = tr, left = tl} => i + sum_tree tr + sum_tree tl 

(* Write a function gardener of type flag tree -> flag tree such that its structure is
 identical to the original tree except all nodes of the input containing prune_me are
(along with all their descendants) replaced with a leaf. *)
fun gardener t =
  case t of
      node {value = leave_me_alone, right = tr, left = tl} => node {value = leave_me_alone,
                                                                    right = gardener tr,
                                                                    left = gardener tl}
    | _ => leaf
