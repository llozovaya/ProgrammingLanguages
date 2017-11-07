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

fun only_capitals(x: string list) =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) x

fun longest_string1(x: string list) =
  case x of
       [] => ""
     | x::xs => foldl (fn (acc, el) => if (String.size el) >= (String.size acc) then el else acc) x xs

fun longest_string2(x: string list) =
  case x of
       [] => ""
     | x::xs => foldl (fn (acc, el) => if (String.size el) > (String.size acc) then el else acc) x xs

fun longest_string_helper (comparator: int * int -> bool) x =
  case x of
       [] => ""
     | x::xs => foldl (fn(acc, el) => if comparator(String.size el, String.size acc) then el else acc) x xs

fun longest_string3(x: string list) =
  longest_string_helper (fn (el, acc) => el >= acc) x

fun longest_string4(x: string list) =
  longest_string_helper (fn (el, acc) => el > acc) x

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
  case ((f o hd) lst, lst) of
       (_, []) => raise NoAnswer
     | (SOME(x), _) => x
     | (NONE, x::xs) => first_answer f xs

fun all_answers f x =
let
  fun all_answers(f, x, acc) =
    case (x, acc) of
         ([], []) => NONE
       | ([], _) => SOME(acc)
       | (x::xs, _) => let
                    val y = f x
                  in
                    if y = NONE
                    then NONE
                    else all_answers(f, xs, acc @ (valOf y))
                  end
in
  all_answers(f, x, [])
end

