(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["A","b"] = ["A"]
val test1_3 = only_capitals ["a","B","c"] = ["B"]
val test1_4 = only_capitals ["Aa","bB","cc"] = ["Aa"]
val test1_5 = only_capitals ["Aa","bB","cC", "Dd"] = ["Aa", "Dd"]

val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 [] = ""
val test2_3 = longest_string1 ["A","abc","bc","def","C"] = "abc"

val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 [] = ""
val test3_3 = longest_string2 ["A","abc","bc","def","C"] = "def"

val test4a_1 = longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string3 [] = ""
val test4a_3 = longest_string3 ["A","abc","bc","def","C"] = "abc"

val test4b_1 = longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string4 [] = ""
val test4b_3 = longest_string4 ["A","abc","bc","def","C"] = "def"

val test5_1 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["A","bc","C","Dd"] = "Dd"
val test5_3 = longest_capitalized ["A","Bb","bc","C","Dd"] = "Bb"

val test6_1 = rev_string "abc" = "cba"
val test6_2 = rev_string "" = ""
val test6_3 = rev_string "vfg" = "gfv"

val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = first_answer (fn x => SOME x) [] handle NoAnswer => "No answer" = "No answer"

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x >= 2 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [7,6,5,4,3,2]
val test8_3 = all_answers (fn x => if x >= 3 then SOME [x] else NONE) [10,2,3,4,5,6,7] = NONE
val test8_4 = all_answers (fn x => if x >= 1 then SOME [x,x+1] else NONE) [1,5,7,4] = SOME [4,5,7,8,5,6,1,2]
val test8_5 = all_answers (fn _ => SOME [] ) [1,5,7,4] = SOME []

val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (ConstructorP ("some", Wildcard)) = 1
val test9a_3 = count_wildcards (ConstructorP ("some", Variable "x")) = 0
val test9a_4 = count_wildcards (TupleP [Wildcard, UnitP, ConstructorP ("some", Wildcard)]) = 2

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test9b_3 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("abcd")]) = 5
val test9b_4 = count_wild_and_variable_lengths (ConstructorP ("some", Variable "xc")) = 2

val test9c_1 = count_some_var ("x", Variable("x")) = 1
val test9c_2 = count_some_var ("x", Wildcard) = 0
val test9c_3 = count_some_var ("x", TupleP [Wildcard, Variable "z", ConstructorP ("x", Variable "t"), ConstructorP ("t", Variable "x")]) = 1

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP [Variable("x"), Variable("x")]) = false
val test10_2 = check_pat (TupleP [ConstructorP ("SOME", Variable "x"), Variable("x")]) = false
val test10_4 = check_pat (TupleP [Variable("x"), UnitP]) = true

val test11_1 = match (Const(1), ConstP(2)) = NONE
val test11_2 = match (Const(1), UnitP) = NONE
val test11_3 = match (Const(1), ConstP(1)) = SOME []
val test11_4 =
    match (Tuple [Unit,Const 17, Constructor ("some",(Const 1))], TupleP [UnitP,
    ConstP 17, Variable "x"]) = SOME [("x",Constructor ("some",(Const 1)))]
val test11_5 = match (Const(1), UnitP) = NONE

val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match Unit [ConstP 19, UnitP, ConstructorP ("some", ConstP 1), UnitP] = SOME []
val test12_3 = first_match (Const 17) [UnitP, Variable "x", Wildcard] = SOME [("x", Const 17)]
val test12_4 = first_match Unit [UnitP] = SOME []

