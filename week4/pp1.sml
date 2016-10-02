(* Write functions 𝚏𝚘𝚕𝚍_𝚖𝚊𝚙 and 𝚏𝚘𝚕𝚍_𝚏𝚒𝚕𝚝𝚎𝚛 that have the same signatures and
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

(* Write a function 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚊𝚕 that takes an integer number n and evaluates to n!.
 Your function should be a composition of 𝚞𝚗𝚏𝚘𝚕𝚍 and 𝙻𝚒𝚜𝚝.𝚏𝚘𝚕𝚍𝚕.
 You should not use any other list functions, recursion or pattern matching. *)
val factorial = (List.foldl (fn (x, acc) => x * acc) 1) o (unfold (fn x => if x > 0 then SOME (x-1 , x) else NONE))

(* Write a function 𝚞𝚗𝚏𝚘𝚕𝚍_𝚖𝚊𝚙, that behaves exactly as 𝙻𝚒𝚜𝚝.𝚖𝚊𝚙 and 𝚏𝚘𝚕𝚍_𝚖𝚊𝚙, but that
would be implemented in terms of 𝚞𝚗𝚏𝚘𝚕𝚍.*)
fun unfold_map f =
  let
      fun helper param =
        case param of
            [] => NONE
          | x :: xs => SOME (xs, f x)
  in
      unfold helper
  end

(* Write a function 𝚍𝚘_𝚞𝚗𝚝𝚒𝚕 that takes three arguments, 𝚏, 𝚙 and 𝚡, and keeps applying
𝚏 to 𝚡 until 𝚙 𝚡 evaluates to 𝚝𝚛𝚞𝚎. Upon reaching that condition, 𝚏 (𝚏 (𝚏 ... (𝚏 𝚡) ...))
is returned. *)
fun do_until f p x =
  if p x
  then x
  else do_until f p (f x) 

(* Write a function 𝚒𝚖𝚙_𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚊𝚕 that has the same behavior as the 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚊𝚕 function
 described above, but is defined in terms of 𝚍𝚘_𝚞𝚗𝚝𝚒𝚕. *)
fun imp_factorial n = #1 (do_until (fn (acc, x) => (acc * x, x - 1)) (fn (_, x) => x = 0) (1, n))

(* Write a function 𝚏𝚒𝚡𝚎𝚍_𝚙𝚘𝚒𝚗𝚝 that accepts some function 𝚏 and an initial value 𝚡,
and keeps applying 𝚏 to 𝚡 until an 𝚡 is found such that 𝚏 𝚡 = 𝚡.
Note that the function must have the same domain and codomain, and that the values must
 be comparable for equality. *)
fun fixed_point f = do_until f (fn x => f x = x)

(* Square root of a real number n is a fixed point of function fn(x)=1/2(x+n/x).
 Unfortunately, for reasons rooted in the arcane art of numerical analysis,
 𝚛𝚎𝚊𝚕s are not comparable for equality in Standard ML.
 Write a function 𝚖𝚢_𝚜𝚚𝚛𝚝 that takes a real number and evaluates to an approximation of
 its square root. You will probably need to write a version of 𝚏𝚒𝚡𝚎𝚍_𝚙𝚘𝚒𝚗𝚝 that uses
 "difference in absolute value less than ϵ" as a test for equality.
 Use ϵ=0.0001. Use the number itself as an initial guess. *)
fun my_sqrt n =
  let
      fun fixed_point f = do_until f (fn x => abs (x - f x) < 0.0001)
  in
      fixed_point (fn x => 0.5 * (x + n / x)) n
  end

(* Let's reuse the binary tree data structure from practice problems for Section 2:

 𝚍𝚊𝚝𝚊𝚝𝚢𝚙𝚎 '𝚊 𝚝𝚛𝚎𝚎 = 𝚕𝚎𝚊𝚏 | 𝚗𝚘𝚍𝚎 𝚘𝚏 { 𝚟𝚊𝚕𝚞𝚎 : '𝚊, 𝚕𝚎𝚏𝚝 : '𝚊 𝚝𝚛𝚎𝚎, 𝚛𝚒𝚐𝚑𝚝 : '𝚊 𝚝𝚛𝚎𝚎 }

 Write functions 𝚝𝚛𝚎𝚎_𝚏𝚘𝚕𝚍 and 𝚝𝚛𝚎𝚎_𝚞𝚗𝚏𝚘𝚕𝚍 that would serve as equivalents of
 𝚏𝚘𝚕𝚍 and 𝚞𝚗𝚏𝚘𝚕𝚍 on lists for this data structure.

HINT: This is a hard problem, but consider this: the initial value for 𝚏𝚘𝚕𝚍 corresponds
 to the base case of recursion on lists (i.e., matching []), while the function passed
 to the 𝚏𝚘𝚕𝚍 corresponds to the case when we match on ::. [] and :: correspond to 𝚕𝚎𝚊𝚏
 and 𝚗𝚘𝚍𝚎 data constructors. Similar reasoning applies to 𝚞𝚗𝚏𝚘𝚕𝚍. You might also want to
 meditate over the signatures below if this does not provide sufficient insight. *)
datatype 'a tree = leaf
                 | node of { value : 'a, left: 'a tree, right: 'a tree }

fun tree_fold f base t =
  case t of
      leaf => base
    | node {value = v, left = l, right = r} => f (tree_fold f base l, v, tree_fold f base r)

fun tree_unfold f base =
  case f base of
      NONE => leaf
    | SOME (lstate, value, rstate) => node ({left = tree_unfold f lstate,
                                            value = value,
                                            right = tree_unfold f rstate})

(* Let's try to write a simple type inference algorithm for a very simple expression language.
 We won't deal with functions, variables or polymorphism.

 The expressions will be represented by the following data type:

 𝚍𝚊𝚝𝚊𝚝𝚢𝚙𝚎 𝚎𝚡𝚙𝚛 = 𝚕𝚒𝚝𝚎𝚛𝚊𝚕_𝚋𝚘𝚘𝚕
             | 𝚕𝚒𝚝𝚎𝚛𝚊𝚕_𝚒𝚗𝚝
             | 𝚋𝚒𝚗𝚊𝚛𝚢_𝚋𝚘𝚘𝚕_𝚘𝚙 𝚘𝚏 𝚎𝚡𝚙𝚛 * 𝚎𝚡𝚙𝚛
             | 𝚋𝚒𝚗𝚊𝚛𝚢_𝚒𝚗𝚝_𝚘𝚙 𝚘𝚏 𝚎𝚡𝚙𝚛 * 𝚎𝚡𝚙𝚛
             | 𝚌𝚘𝚖𝚙𝚊𝚛𝚒𝚜𝚘𝚗 𝚘𝚏 𝚎𝚡𝚙𝚛 * 𝚎𝚡𝚙𝚛
             | 𝚌𝚘𝚗𝚍𝚒𝚝𝚒𝚘𝚗𝚊𝚕 𝚘𝚏 𝚎𝚡𝚙𝚛 * 𝚎𝚡𝚙𝚛 * 𝚎𝚡𝚙𝚛

 The data constructors represent literal booleans, literal integers, binary operators on booleans,
 binary operators on integers, comparison operators and conditionals. Since we're only interested
 in types, and not in actually evaluating our expressions, we're omitting immaterial details, such
 as whether a literal boolean is "true" or "false", or whether an operator on integers is addition,
 subtraction or something else entirely.

 The types will be represented by the following simple datatype:

 𝚍𝚊𝚝𝚊𝚝𝚢𝚙𝚎 𝚎𝚡𝚙𝚛_𝚝𝚢𝚙𝚎 = 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕 | 𝚝𝚢𝚙𝚎_𝚒𝚗𝚝

 The typing rules for our expression language are simple:

 Literal booleans are of type 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕.
 Literal integers have type 𝚝𝚢𝚙𝚎_𝚒𝚗𝚝.
 Boolean operators have type 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕 provided that both of their operands also have type 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕.
 Integer operators have type 𝚝𝚢𝚙𝚎_𝚒𝚗𝚝 provided that both operands also have type 𝚝𝚢𝚙𝚎_𝚒𝚗𝚝.
 Comparison operators have type 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕 provided that both operands have type 𝚝𝚢𝚙𝚎_𝚒𝚗𝚝.
 Conditionals have the same type as the first branch, provided that the second branch has the same type,
 and the condition has type 𝚝𝚢𝚙𝚎_𝚋𝚘𝚘𝚕.
 Write a function 𝚒𝚗𝚏𝚎𝚛_𝚝𝚢𝚙𝚎 that accepts an 𝚎𝚡𝚙𝚛 and evaluates to the type of the given expression.
 If the type cannot be determined according to the rules above, raise 𝚃𝚢𝚙𝚎𝙴𝚛𝚛𝚘𝚛 exception. *)
datatype expr = literal_bool | literal_int
                | binary_bool_op of expr * expr | binary_int_op of expr * expr
                | comparison of expr * expr
                | conditional of expr * expr * expr

datatype expr_type = type_bool | type_int

exception TypeError

fun infer_type expression =
  case expression of
      literal_bool => type_bool
    | literal_int => type_int
    | binary_bool_op (x1, x2) => if infer_type x1 = type_bool
                                    andalso
                                    infer_type x2 = type_bool
                                 then type_bool
                                 else raise TypeError
    | binary_int_op (x1, x2) => if infer_type x1 = type_int
                                   andalso
                                   infer_type x2 = type_int
                                then type_int
                                else raise TypeError
    | comparison (x1, x2) => if infer_type x1 = type_int
                                andalso
                                infer_type x2 = type_int
                             then type_bool
                             else raise TypeError
    | conditional (x1, x2, x3) => let val t2 = infer_type x2
                                      val t3 = infer_type x3
                                  in
                                      if infer_type x1 = type_bool andalso t2 = t3
                                      then t2
                                      else raise TypeError
                                  end
