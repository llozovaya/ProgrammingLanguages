(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* (a) Write a function all_except_option , which takes a string and a string list.
   Return NONE if the string is not in the list, else return SOME lst where lst is
   identical to the argument list except the string is not in it. *)
fun all_except_option (str, strs) =
    let fun remove_fst (prevs, nexts) =
        case nexts of
              [] => (prevs, false)
            | x::xs =>
                if same_string(str,x)
                then (prevs @ xs, true)
                else
                    remove_fst(prevs @ (x::[]), xs)
        val (strs', contains) = remove_fst([], strs)
    in
        if contains
        then SOME strs'
        else NONE
    end


(* (b) Write a function get_substitutions1 , which takes a string list list
   (a list of list of strings, the substitutions) and a string s and returns
   a string list .  The result has all the strings that are in some list in
   substitutions that also has s , but s itself should not be in the result. *)
fun get_substitutions1 (subs, str) =
    case subs of
         [] => []
      | sub::subs' =>
            case all_except_option(str, sub) of
                 NONE => get_substitutions1(subs', str)
               | SOME names => names @ get_substitutions1(subs', str)


(* (c) Write a function get_substitutions2 , which is like get_substitutions1
   except it uses a tail-recursive local helper function. *)
fun get_substitutions2(subs, str)=
  case subs of
        [] => []
      | _ =>
          let fun aux(ss, acc) =
                  case ss of
                      [] => acc
                    | s::ss' =>
                        case all_except_option(str, s) of
                             NONE => aux(ss', acc)
                           | SOME names => aux(ss', acc @ names)
          in
              aux(subs, [])
          end


(* (d) Write a function similar_names , which takes a string list list of substitutions
   (as in parts (b) and (c)) and a full  name of type {first:string,middle:string,last:string}
   and returns a list of full names (type {first:string,middle:string,last:string} list).
   The result is all the full names you can produce by substituting for the  rst name (and
   only the  rst name) *)
fun similar_names(subs, fullname) =
    let val {first, middle, last} = fullname
        fun getnames fsts =
            case fsts of
                 [] => []
               | f::fs => {first=f, middle=middle, last=last}::(getnames fs)
    in
        fullname::getnames(get_substitutions2(subs, first))
    end


(*PART 2*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) Write  a  function card_color ,  which  takes  a  card  and  returns  its  color
   (spades  and  clubs  are  black, diamonds and hearts are red). *)
fun card_color(st, _) =
    case st of
          Clubs => Black
        | Diamonds => Red
        | Hearts => Red
        | Spades => Black


(* (b) rite  a  function card_value ,  which  takes  a  card  and  returns  its  value
   (numbered  cards  have  their number as the value, aces are 11, everything else is 10)*)
fun card_value(_, rnk) =
    case rnk of
        Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num num => num


(*  (c) Write a function remove_card , which takes a list of cards cs , a card c
    , and an exception e .  It returns a list that has all the elements of cs except
    c .  If c is in the list more than once, remove only the  rst one.  If c
    is not in the list, raise the exception e .  *)
fun remove_card (cs, c, e) =
            let fun aux(prev, next) =
                  case next of
                      [] => (prev, false)
                    | card::cards =>
                        if c = card
                        then (prev @ cards, true)
                        else aux(prev @ (c::[]), cards)
                val (cards, contains) = aux([], cs)
            in
                if contains
                then cards
                else raise e
            end


(* (d) Write a function all_same_color ,  which takes a list of cards
    and returns true if all the cards in the list are the same color. *)
fun all_same_color(cs) =
    case cs of
         c1::c2::cs' =>
            card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')
       | _ => true

(* (e) Write a function all_same_color , which takes a list of cards and returns
   true if all the cards in the list are the same color. *)
fun sum_cards (cs) =
    let fun aux(cs, acc) =
            case cs of
                 [] => acc
               | c::cs' => aux(cs', card_value(c)+acc)
    in
        aux(cs, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and
    an int (the goal) and computes the score as described above. *)
fun score (cs, goal) =
    let val sum = sum_cards(cs)
        val prel = if sum > goal
                   then 3 * (sum - goal)
                   else goal - sum
    in
        if all_same_color(cs)
        then prel div 2
        else prel
    end


(* (g) Write a function officiate , which \runs a game."  It takes a card list
   (the card-list) a move list (what the player \does" at each point), and an
   int (the goal) and returns the score at the end of the game after processing
   (some or all of) the moves in the move list in order.  *)
fun officiate (cards, moves, goal) =
    let fun perform_move(cards, held_cards, moves) =
        case moves of
             [] => score(held_cards, goal)
           | (Discard c)::ms =>
               perform_move(cards, remove_card(held_cards, c, IllegalMove), ms)
           | Draw::ms =>
               case cards of
                    [] => score(held_cards, goal)
                  | c::cs =>
                      let val new_held = c:: held_cards
                      in
                        if sum_cards(new_held) > goal
                        then score(new_held, goal)
                        else perform_move(cs, new_held, ms)
                      end
    in
        perform_move(cards, [], moves)
    end
