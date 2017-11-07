(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(* Problem 1*)
val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

(*Problem 2*)
val longest_string1 = foldl (fn (new,prev) => if String.size new > String.size prev then new else prev) ""

(*Problem 3*)
val longest_string2 = foldl (fn (new,prev) => if String.size new >= String.size prev then new else prev) ""

(*Problem 4*)

fun longest_string_helper predicate l =
    foldl (fn (new,prev) => if predicate (String.size new, String.size prev) then new else prev) "" l

val longest_string3 = longest_string_helper (fn (a,b) => a > b)

val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

(*Problem 5*)
val longest_capitalized = longest_string3 o only_capitals

(*Problem 6*)
val rev_string = implode o rev o explode

(*Problem 7*)
fun first_answer f l =
    case l of
        [] => raise NoAnswer
      | x::xs => case f x of
                    NONE => first_answer f xs
                  | SOME ans => ans

(*Problem 8*)
fun all_answers f l =
    let
      fun aux (x,acc) =
          case acc of
              NONE => NONE
            | SOME xs => case f x of
                            NONE => NONE
                          | SOME res => SOME (res @ xs)
    in
      foldl aux (SOME []) l
    end

(*Problem 9*)
fun count_wildcards pattern = g (fn x => 1) (fn x => 0) pattern

fun count_wild_and_variable_lengths pattern = g (fn x => 1) String.size pattern

fun count_some_var (name,pat) = g (fn x => 0) (fn var => if var = name then 1 else 0) pat

(*Problem 10*)
fun check_pat pat =
    let
        fun take_varnames pat =
            case pat of
                Variable n => [n]
              | ConstructorP (_, pat) => take_varnames pat
              | TupleP pats => foldl (fn (x,y) => (take_varnames x) @ y ) [] pats
              | _ => []
        fun exist_doubles varnames =
            case varnames of
                 [] => false
               | x::xs => List.exists (fn s => s = x) xs orelse exist_doubles xs
    in
        not (exist_doubles (take_varnames pat))
    end


(*Problem 11*)
fun match exp =
    case exp of
        (_, Wildcard) => SOME []
      | (value, Variable var) => SOME [(var, value)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vals, TupleP pats) => all_answers match (ListPair.zip (vals, pats))
      | (Constructor (name,value), ConstructorP(nameP,pat)) =>
         if name = nameP then match (value,pat) else NONE
      | _ => NONE

(*Problem 12*)
fun first_match value pats =
    SOME (first_answer (fn p => match (value,p)) pats)
    handle NoAnswer => NONE


