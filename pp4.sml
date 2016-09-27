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
on the list are positive*)
fun gcd_list xs =
  case xs of
      [] => 1
    | x::[] => x
    | x::xs' => gcd(x, gcd_list xs')
