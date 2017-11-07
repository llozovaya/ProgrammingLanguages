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
       | Datatype of string;

(**** you can put all your code here ****)
(* 1 ------------------------------------------------------------------ *)
fun only_capitals(strings : string list) =
  List.filter (fn ss => Char.isUpper(String.sub(ss,0))) strings;

(* 2 ------------------------------------------------------------------ *)
fun longest_string1(strings) =
  foldl (fn (s1,s2) => if(String.size(s1) > String.size(s2)) then s1 else s2 ) "" strings;

(* 3 ------------------------------------------------------------------ *)
fun longest_string2(strings) =
  foldl (fn (s1,s2) => if(String.size(s1) >= String.size(s2)) then s1 else s2 ) "" strings;

(* 4 ------------------------------------------------------------------ *)
fun longest_string_helper f strings =
  foldl ( fn (s1,s2) => if( f(String.size(s1),String.size(s2)) ) then s1 else s2) "" strings;
val longest_string3 = fn strings => longest_string_helper (op >) strings;
val longest_string4 = fn strings => longest_string_helper (op >=) strings;

(* 5 ------------------------------------------------------------------ *)
fun longest_capitalized strings = (longest_string1 o only_capitals) strings

(* 6 ------------------------------------------------------------------ *)
fun rev_string string = (String.implode o rev o String.explode) string;

(* 7 ------------------------------------------------------------------ *)
exception NoAnswer;
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f(x) of
                   NONE => first_answer f xs'
                 | SOME i => i;

(* 8 ------------------------------------------------------------------ *)
fun all_answers pred xs =
  let fun aux(pred,xss,res) =
        case xss of
            [] => SOME res
          | x::xss' => case pred(x) of
                          NONE => NONE
                        | SOME i => aux(pred,xss',res @ i)
  in
      aux(pred,xs,[])
  end;

(* 9 ------------------------------------------------------------------ *)
(* a *)
fun count_wildcards p =
  g (fn x => 1) (fn x => 0) p;

(* b *)
fun count_wild_and_variable_lengths p =
  g (fn x => 1) (fn s => String.size s) p

(* c *)
fun count_some_var (str,ps) =
  let val cmp = fn s => if s = str then 1 else 0 in
      g (fn x => 0) cmp ps
  end;

(* 10 ------------------------------------------------------------------ *)
fun check_pat p =
  let fun get_strings(pat,acc) =
        case pat of
            Variable x => [x]
          | TupleP ps => List.foldl (fn (p',acc') =>  acc' @ get_strings(p',acc')  ) [] ps
          | ConstructorP(_,ps) => get_strings(ps,acc)
          | _ => acc;
      fun check_repeats(xs) =
        case xs of
            [] => true
          | x::xs' => if List.exists (fn s1 => s1 = x) xs' then false
                     else check_repeats(xs')
  in
      check_repeats(get_strings(p,[]))
  end;

(* 11 ------------------------------------------------------------------ *)
fun match (v,p) =
  case (v,p) of
      (_,Wildcard) => SOME []
    | (_,Variable s) => SOME [(s,v)]
    | (_,UnitP) => if v = Unit then SOME [] else NONE
    | (Const iv,ConstP ip) => if ip = iv then SOME [] else NONE
    | (Tuple vs,TupleP ps) => if List.length ps = List.length vs then
                                 all_answers match (ListPair.zip(vs,ps))
                             else NONE
    | (Constructor(s1,v'),ConstructorP(s2,p')) => if s1 = s2 then match(v',p') else NONE
    |  _ => NONE;

(* 12 ------------------------------------------------------------------ *)
fun first_match v ps =
  SOME (first_answer (fn p => match (v,p)) ps)
  handle NoAnswer => NONE
