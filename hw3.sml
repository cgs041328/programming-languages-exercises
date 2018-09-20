exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl (fn (x, y) => if String.size x > String.size y then x else y) ""

val longest_string2 = List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f = List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* ******************* *)

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case (f x) of
                      NONE => first_answer f xs'
                    | SOME v => v

fun all_answers f xs =
    let fun helper acc xs =
            case xs of
                [] => SOME acc
              | x :: xs' => case (f x) of
                                  NONE => NONE
                                | SOME v => helper (acc @ v) xs'
    in
        helper [] xs
    end

(* Part 2 *)
val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) String.size

fun count_some_var (s, p) = g (fn x => 0) (fn x => if x = s then 1 else 0) p

val check_pat =
    let fun get_all_vars p =
            case p of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, acc) => (get_all_vars p) @ acc) [] ps
              | _ => []
        fun has_repeats vs =
            case vs of
                [] => false
              | v::vs' => List.exists (fn x => x = v) vs' orelse has_repeats vs'
    in
        not o has_repeats o get_all_vars
end

fun match (va, pa) =
    case (va, pa) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
                                 then let val x = ListPair.zip(vs, ps)
                                      in all_answers match x
                                      end
                                 else NONE
      | (Constructor (s,v), ConstructorP (s',p)) => if s = s' then match(v,p) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE


