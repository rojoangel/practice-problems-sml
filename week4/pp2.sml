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
