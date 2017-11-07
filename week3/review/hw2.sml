
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* problem (a) return NONE if string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it*)
fun all_except_option(str, slist) = 
  case slist of [] => NONE
	      | s::rlist => let val ans = all_except_option(str, rlist) in
				if same_string(s, str) then
				    SOME rlist
				else case ans of NONE => NONE 
					      | SOME res => SOME(s::res)
			end;

		
(* problem (b) *)
(* get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") *)
(* answer: ["Fredrick","Freddie","F"] *)

fun get_substitutions1(sslist, s) =
  case sslist of [] => []
	      | slist::rlist => let val ans = get_substitutions1(rlist, s) in
case all_except_option(s, slist) of NONE => ans 
				 | SOME tlist => tlist@ans
end;

fun get_substitutions2(sslist, s) = 
let fun inner(sslist, s, acc) = 
      case sslist of [] => acc
			| slist::rlist => case all_except_option(s, slist) of
					      NONE => inner(rlist, s, acc)
						   | SOME tlist => inner(rlist, s, acc@tlist)
in
    inner(sslist, s, [])
end;

fun similar_names(subs, fullName) = let
    fun get_full_name(first_name) = case fullName of {first = _, middle = m, last = l} => {first = first_name, middle = m, last = l}
    val slist = case fullName of {first = first_name, middle = _, last = _} => get_substitutions2(subs, first_name);
    val first_name = case fullName of {first = first_name, middle=_, last=_} => first_name;
in
    (map(get_full_name))(first_name::slist)
end;
  

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 


exception IllegalMove

(* problem 2 *)
(* problem (a) *)
fun card_color(mCard) = case mCard of (Spades, _) => Black
				   | (Clubs, _) => Black
				   | _ => Red

(* problem (b) *)
fun card_value(mCard) = case mCard of (_, Num i) => i
				   | (_, Ace) => 11
				   | (_, _) => 10
(* problem (c) *)
fun remove_card(cards, rcard, e) = case cards of [] => raise e
					      | fcard::rest => if fcard = rcard then rest else fcard::remove_card(rest, rcard, e);

(* problem (d) *)
fun all_same_color(cards) = case cards of [] => true
				       | c1::c2::rest => card_color(c1)=card_color(c2) andalso all_same_color(c2::rest)
				       | c::[] => true;

(* problem (e) *)
fun sum_cards(cards) = let
    fun sum(lst) = case lst of [] => 0
				  | el::rest => el + sum(rest);
in
    sum(map(card_value)(cards))
end;

(* problem (f) *)
fun score(cards, goal) = let
    val s = sum_cards(cards);
    val rs = if s > goal then 3*(s-goal) else goal-s
in
    if all_same_color(cards) then rs div 2 else rs
end;

fun officiate(cards, moves, goal) = let
    fun inner(cards, helds, moves, goal, sum) = 
      if sum < goal then
      case moves of [] => helds
		       | Discard dcard::mrest => inner(cards, remove_card(helds, dcard, IllegalMove), mrest, goal, sum-card_value(dcard))
					      | Draw::mrest => case cards of fcard::crest => inner(crest, fcard::helds, mrest, goal, sum+card_value(fcard))
| [] => helds
											   else helds
in
    score(inner(cards, [], moves, goal, 0), goal)
end;

