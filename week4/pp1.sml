(*Write functions 𝚏𝚘𝚕𝚍_𝚖𝚊𝚙 and 𝚏𝚘𝚕𝚍_𝚏𝚒𝚕𝚝𝚎𝚛 that have the same signatures and
behavior as 𝙻𝚒𝚜𝚝.𝚖𝚊𝚙 and 𝙻𝚒𝚜𝚝.𝚏𝚒𝚕𝚝𝚎𝚛 correspondingly.
Use 𝙻𝚒𝚜𝚝.𝚏𝚘𝚕𝚍𝚛. Do not use pattern matching or any other list functions. *)
fun fold_map f = List.foldr (fn (x, acc) => f x :: acc ) [] 
fun fold_filter f = List.foldr (fn (x, acc) => if f x then x :: acc else acc) []

(* Write a function 𝚞𝚗𝚏𝚘𝚕𝚍 that takes a state transition function and an initial
 state and produces a list. On each step the current state is fed into the state
 transition function, which evaluates either to 𝙽𝙾𝙽𝙴, indicating that the result
 should contain no more elements, or to 𝚂𝙾𝙼𝙴 pair, where pair contains the next
state and the next list element. *)
fun unfold f state =
  case f state of
      NONE => []
    | SOME (state', x) => x :: unfold f state' 

(* Write a function 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚊𝚕 that takes an integer number n and evaluates to n!. Your function should be a composition of 𝚞𝚗𝚏𝚘𝚕𝚍 and 𝙻𝚒𝚜𝚝.𝚏𝚘𝚕𝚍𝚕. You should not use any other list functions, recursion or pattern matching. *)
val factorial = (List.foldl (fn (x, acc) => x * acc) 1) o (unfold (fn x => if x > 0 then SOME (x-1 , x) else NONE))
