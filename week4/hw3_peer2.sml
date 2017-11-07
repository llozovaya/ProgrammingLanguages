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

fun only_capitals strings = List.filter(fn x => Char.isUpper(String.sub(x,0))) (strings)

fun longest_string1 ss = List.foldl (fn(x,y) => if (String.size(x) > String.size(y)) then x else y) "" ss
fun longest_string2 ss = List.foldl (fn(x,y) => if (String.size(x) < String.size(y)) then y else x) "" ss

fun longest_string_helper f = List.foldl (fn(x,y) => if f(String.size(x), String.size (y)) then x else y) ""
val longest_string3 = longest_string_helper ( fn(x,y) => x >  y)
val longest_string4 = longest_string_helper ( fn(x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f ls=
  case ls of
      [] => raise NoAnswer
    | x::xs => case f(x) of
                   NONE => first_answer f xs
		 | SOME v => v

fun all_answers f lst =
  let fun helper (ls, acc) =
	case ls of
            [] => SOME acc
          | x::xs => case f(x) of
                         NONE => NONE
                       | SOME v => helper (xs, v@acc)
  in helper (lst, [])
  end

val count_wildcards = g (fn _ => 1)(fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1)(fn s => String.size s)

fun count_some_var (s, p) = g (fn _ => 0)(fn x => if s = x then 1 else 0) p

fun check_pat p =
  let fun helper1 ls =
        case ls of
            Variable x => [x]
           | TupleP ps  => List.foldl (fn (r,i) => helper1(r)@i) [] ps
           | _ => []
      fun test lst =
       case lst of
        [] => true
        | x::xs => if (List.exists(fn y: string => x = y ) xs) then false else test xs
  in test ( helper1 p)
  end

fun match (v: valu, p: pattern) =
  case (v,p) of
      (_, Variable x) => SOME [(x, v)]
    | (_, Wildcard) => SOME []
    | (Unit, UnitP) => SOME []
    | (_, UnitP) => NONE
    | (Const(v), ConstP k) => if k = v then SOME [] else NONE
    | (_, ConstP k) => NONE
    | (Tuple vs, TupleP ps) =>
      if List.length vs = List.length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
    | (_, TupleP ps) => NONE
    | (Constructor(s2,vv), ConstructorP(s1,pp)) => if s1 = s2 then match(vv,pp) else NONE
    | (_, ConstructorP(_,_)) => NONE

fun first_match v ps =
  SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE

(*
ConstructorP ("hi",TupleP[Variable "x",Variable "x"])
ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])
check_pat

check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",Variable "x"]), should have gotten: false but your function returned otherwise. [incorrect answer] check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])]), should have gotten: false but your function returned otherwise. [incorrect answer] prob13 tests failed to run (most likely caused by an incorrect function signature in the submission)
*)
