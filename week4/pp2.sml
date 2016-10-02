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

(* Write a function 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 following the specification from week 2's
 "Flip Flop" problem. Use folds. *)
val alternate =
      #2 o List.foldl (fn (x, (factor, acc)) => (~1 * factor, x * factor + acc)) (1,0)

(* Write a function 𝚖𝚒𝚗_𝚖𝚊𝚡 following the specification from week 2's
 "Minimum/Maximum" problem. Use folds. *)
fun min_max (x::xs) =
    List.foldl (fn (x, (acc_min, acc_max)) => (
                    if x < acc_min then x else acc_min,
                    if x > acc_max then x else acc_max)) (x,x) xs

(* Write a function 𝚞𝚗𝚣𝚒𝚙 following the specification from week 2's
 "Lists And Tuples, Oh My!" problem. Use folds. *)
fun unzip xs = List.foldr (fn ((x,y), (acc_x, acc_y)) => (x::acc_x, y::acc_y)) ([], []) xs

(* Write a function 𝚣𝚒𝚙 following the specification from week 2's
 "Lists And Tuples, Oh My! -- Continued (1)" problem.
 Use 𝚞𝚗𝚏𝚘𝚕𝚍 that you wrote in "The Evil Twin" problem. *)
fun zip xs =
  let
      fun unfold f state =
        case f state of
            NONE => []
          | SOME (state', x) => x :: unfold f state'
      fun aux param =
        case param of
            (x::xs, y::ys) => SOME ((xs, ys), (x, y))
          | _ => NONE
  in
      unfold aux xs
  end

(* Write a function 𝚛𝚎𝚙𝚎𝚊𝚝𝚜_𝚕𝚒𝚜𝚝 following the specification from week 2's
 "BananaBanana -- Continued (Again)" problem. Use folds. *)
fun repeats_list xs =
  let
      fun unfold f state =
        case f state of
            NONE => []
          | SOME (state', x) => x :: unfold f state'
      fun aux param =
        case param of
            (_::ss, 0::is) => aux (ss, is)
          | (s::ss, i::is) => SOME ((s::ss, (i-1)::is), s)
          | _ => NONE 
  in
      unfold aux xs
  end
