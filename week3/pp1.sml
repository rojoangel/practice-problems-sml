type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = {id : student_id, grade : grade option}
datatype pass_fail = pass | fail

(* pass_or_fail of type {grade: int option, id: a'} -> pass_fail that takes
a final_grade (or, as the type indicates a more general type) and returns
pass if the grade field contains SOME i for a i >= 75 (else fail). *)
fun pass_or_fail a_grade =
  case a_grade of
      {id = _, grade = SOME i} => if i >= 75
                                then pass
                                else fail
    | _ => fail

(* Using pass_or_fail as a helper function, write a function has_passed of type
 {grade : int option, id: a'} -> bool that returns true if and only if the grade
field contains SOME i for and i >= 75 *)
fun has_passed a_grade =
  pass_or_fail a_grade = pass
(* a more convoluted approach to has_passed *)
(*
fun has_passed a_grade =
  case pass_or_fail a_grade of
      pass => true
    | fail => false
*)

(* Using has_passed as a helper function, write a function number_passed that takes
a list of type final_grade (or a more general type) and returns how many list elements
have passing grades *)
(* solution without tail recursion *)
(*
fun number_passed gs =
  case gs of
      [] => 0
    | g::gs' => if has_passed g
                then 1 + number_passed gs'
                else number_passed gs'
*)
(* reimplemented using tail-recursion *)
fun number_passed gs =
  let fun aux(gs, acc) =
        case gs of
            [] => acc
          | g::gs' => if has_passed g
                     then aux(gs', acc + 1)
                     else aux(gs', acc)
  in
      aux (gs,0)
  end

(* write a function number_misgraded of type (pass_fail * final_grade) list -> int
 that indicates how many list elements are 'mislabeled' where mislabeling means a
 pair (pass, x) where has_passed x is false or (fail, x) where has_passed x is true *)
fun number_misgraded xs =
  case xs of
      [] => 0
    | (pass, g)::xs' => number_misgraded xs' + (if has_passed g then 0 else 1)
    | (fail, g)::xs' => number_misgraded xs' + (if has_passed g then 1 else 0)
