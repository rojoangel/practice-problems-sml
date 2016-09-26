(* provided *)
datatype nat = ZERO | SUCC of nat

(* Write 𝚒𝚜_𝚙𝚘𝚜𝚒𝚝𝚒𝚟𝚎 : 𝚗𝚊𝚝 -> 𝚋𝚘𝚘𝚕, which given a "natural number" returns whether
that number is positive (i.e. not zero). *)
fun is_positive n =
  case n of
      ZERO => false
    | _ => true

(* Write pred: nat -> nat, which given a "natural number" returns its predecessor.
Since 0 does not have a predecessor in the natural numbers, throw an exception
Negative (will need to be defined first) *)
exception Negative
fun pred n =
  case n of
      ZERO => raise Negative
    | SUCC n' => n'
