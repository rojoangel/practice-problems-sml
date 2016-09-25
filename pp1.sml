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

