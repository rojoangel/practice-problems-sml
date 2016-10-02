(* Write a function 𝚐𝚌𝚍_𝚕𝚒𝚜𝚝 following the specification from week 2's
 "Greated Common Divisor -- Continued" problem. Use folds.
 Use the following implementation of 𝚐𝚌𝚍 as a helper function:
 *)
(* provided code *)
fun gcd (a : int, b : int) =
  if a = b
  then a
  else
      if a < b
      then gcd (a, b - a)
      else gcd (a - b, b)

fun gcd_list xs = List.foldl gcd (hd xs) (tl xs)

(* Write a function 𝚊𝚗𝚢_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 following the specification from week 2's
 "Element Of A List" problem. Use folds or other higher-order list functions.
 Use the following implementation of 𝚒𝚜_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 as a helper function:

𝚏𝚞𝚗 𝚒𝚜_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 (𝚊 : 𝚒𝚗𝚝, 𝚋 : 𝚒𝚗𝚝) = 𝚊 𝚖𝚘𝚍 𝚋 = 𝟶 *)
(* provided code *)
fun is_divisible_by (a : int, b : int) = a mod b = 0
fun any_divisible_by (xs, i) = List.exists (fn x => is_divisible_by (x, i)) xs
(* another solution using foldl                        
fun any_divisible_by (xs, i) = List.foldl
                                   (fn (x, acc) => acc orelse is_divisible_by (x, i))
                                   false
                                   xs
*)

(* Write a function 𝚊𝚍𝚍_𝚊𝚕𝚕_𝚘𝚙𝚝 following the specification from week 2's
 "Quirky Addition -- Continued" problem. Use folds. *)
(* provided code *)
val add_all_opt =
    let
        fun add_opt param =
          case param of
              (SOME x, SOME acc) => SOME (x + acc)
            | (NONE, acc) => acc
            | (x, NONE) => x 
    in
        List.foldl add_opt NONE
    end
