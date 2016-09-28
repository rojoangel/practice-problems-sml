(* provided *)
fun gcd (a : int, b : int) =
  if a = b
  then a
  else
      if a < b
      then gcd (a, b - a)
      else gcd (a - b, b)

(* Write a function 𝚐𝚌𝚍_𝚕𝚒𝚜𝚝 following the specification from Week 2's
"Greatest Common Divisor -- Continued" problem. Use pattern matching instead
of list functions. You may assume that the list is non-empty and all the numbers
on the list are positive *)
fun gcd_list xs =
  case xs of
      x::[] => x
    | x::xs' => gcd(x, gcd_list xs')
    | _ => raise Empty

(* Write a function 𝚊𝚗𝚢_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 that takes a list of integers and a divisor
(an integer number) and evaluates to either 𝚝𝚛𝚞𝚎 or 𝚏𝚊𝚕𝚜𝚎. The function should
evaluate to 𝚝𝚛𝚞𝚎 if and only if there exists an element of the list that is
divisible by the function's second argument. *)
(* provided helper function *)
fun is_divisible_by (a : int, b : int) = a mod b = 0

fun any_divisible_by (xs, i) =
  case xs of
      [] => false
    | x::xs' => is_divisible_by (x, i) orelse any_divisible_by (xs', i)

(* Write a function 𝚊𝚍𝚍_𝚘𝚙𝚝 that given two "optional" integers, adds themif they are
both present, or evaluates to 𝙽𝙾𝙽𝙴 if at least one of the two arguments is 𝙽𝙾𝙽𝙴. *)
fun add_opt (o1, o2) =
  case (o1, o2) of 
      (SOME i1, SOME i2) => SOME(i1 + i2)
    | (_, _)  => NONE

(* Write a function 𝚊𝚍𝚍_𝚊𝚕𝚕_𝚘𝚙𝚝 that given a list of "optional" integers, adds those
integers that are there (i.e. adds all the 𝚂𝙾𝙼𝙴 𝚒). If the list does not contain any
𝚂𝙾𝙼𝙴 in it, i.e. they are all 𝙽𝙾𝙽𝙴 or the list is empty, the function should evaluate to NONE. *)
fun add_all_opt xs =
  let fun f param =
      case param of
          ([], acc) => acc
        | (SOME x :: xs, SOME y) => f(xs, SOME (x+y))
        | (NONE :: xs, SOME y) => f(xs, SOME y)
        | (x::xs, _) =>  f(xs, x)
  in
      f(xs, NONE)
  end

(* Write a function 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 that takes a list of numbers and adds them with alternating sign. The result of applying this function to [𝟷, 𝟸, 𝟹, 𝟺] should be 𝟷 - 𝟸 + 𝟹 - 𝟺 = ~𝟸. *)
fun alternate xs =
  case xs of
      [] => 0
    | x::[] => x
    | x::x'::xs' => x - x' + alternate xs'

(* alternative solution *)
(*
fun alternate xs =
  let
      fun helper (factor, xs) =
        case xs of
            [] => 0
          | x::xs' => x * factor + helper (~1 * factor, xs')
  in
      helper (1, xs)
  end
*)

(* Write a function 𝚖𝚒𝚗_𝚖𝚊𝚡 that takes a non-empty list of numbers, and evaluates to a tuple (𝚖𝚒𝚗, 𝚖𝚊𝚡) of the minimum and maximum of the numbers in the list. *)
fun min_max xs =
  case xs of
      [] => raise Empty
    | x::[] => (x, x)
    | x::xs' => case min_max xs' of
                      (tail_min, tail_max) => (if x < tail_min then x else tail_min,
                                               if x > tail_max then x else tail_max)

(* alternative solution using helper function and tail recursion *)
(*
fun min_max xs =
  let fun accumulate_min_max ((min, max), xs) =
        case xs of
            [] => (min, max)
          | x'::xs' => accumulate_min_max (
                         (if x' < min then x' else min,
                          if x' > max then x' else max),
                         xs')
  in
      case xs of
          [] => raise Empty
        | x::xs => accumulate_min_max ((x,x), xs)
  end
*)

(* Write a function 𝚞𝚗𝚣𝚒𝚙 that takes an (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 and evaluates to 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 such that the first element of the resulting tuple is a list consisting of all first elements of the argument (in order), and the second element of the result consists of all second elements of the tuples in the original list. *)
fun unzip xs =
  case xs of
      [] => ([], [])
    | (i1, i2)::xs' => case unzip xs' of
                           (i1s, i2s) => (i1::i1s, i2::i2s)

(* Write a function 𝚛𝚎𝚙𝚎𝚊𝚝𝚜_𝚕𝚒𝚜𝚝 that given a list of strings and a list of nonnegative integers, repeats the strings in the first list according to the numbers indicated by the second list. You may assume that both lists have the same length.*)
exception ListLengthMismatch
fun repeats_list param =
  case param of
      ([], []) => []
    | (_::ss, 0::is) => repeats_list (ss, is)
    | (s::ss, i::is) => s :: repeats_list (s::ss, i-1::is)
    | _  => raise ListLengthMismatch 
