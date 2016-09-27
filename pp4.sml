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
