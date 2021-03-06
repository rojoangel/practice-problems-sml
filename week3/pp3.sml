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

(* Write nat_to_int: nat -> int, which given a "natural number" returns the
 corresponding int. *)
fun nat_to_int n =
  case n of
      ZERO => 0
   | SUCC n' => 1 + nat_to_int n'

(* Write int_to_nat : int -> nat which given an integer retturns a "natural number"
 representation for it, or throws a Negative exception if the integer was negative *)
fun int_to_nat i =
  if i < 0
  then raise Negative
  else if i = 0 then ZERO
  else SUCC (int_to_nat (i-1))

(* Write add: nat * nat -> nat to perform addition *)
fun add (n1,n2) =
  case (n1,n2) of
      (ZERO, n2) => n2
    | (n1, ZERO) => n1
    | (SUCC n1', SUCC n2') => SUCC(SUCC(add (n1', n2')))

(* Write sub : nat * nat -> nat to perform substraction. (Hint: Use pred.) *)
fun sub (n1,n2) =
  case (n1,n2) of
      (n1, ZERO) => n1
    | (n1, n2) => sub (pred n1, pred n2) 

(* Write mult : nat * nat -> nat to perform multiplication (Hint: use add.) *)
fun mult (n1,n2) =
  case (n1,n2) of
      (ZERO, _) => ZERO
    | (_, ZERO) => ZERO
    | (n1, n2) => add(n1, mult(n1, pred n2))

(* Write less_than : nat * nat -> bool to return true when the firt argument is less
than the second *)
fun less_than (n1,n2) =
  case (n1,n2) of
      (_, ZERO) => false
    | (ZERO, _) => true
    | (n1, n2) => less_than (pred n1, pred n2)
