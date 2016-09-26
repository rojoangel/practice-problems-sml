(* provided *)
datatype nat = ZERO | SUCC of nat

(* Write 𝚒𝚜_𝚙𝚘𝚜𝚒𝚝𝚒𝚟𝚎 : 𝚗𝚊𝚝 -> 𝚋𝚘𝚘𝚕, which given a "natural number" returns whether that number is positive (i.e. not zero). *)
fun is_positive n =
  case n of
      ZERO => false
    | _ => true
               
