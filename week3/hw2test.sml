(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_1 = all_except_option ("string", ["string"]) = SOME []

val test1_2 = all_except_option ("string", []) = NONE

val test1_4 = all_except_option ("string", ["notstring"]) = NONE

val test1_5 = all_except_option ("string", ["string", "notstring"]) = SOME ["notstring"]

val test1_6 = all_except_option ("string", ["notstring", "string"]) = SOME ["notstring"]

val test1_7 = all_except_option ("string", ["notstring", "king", "string", "thing", "sing"]) =
  SOME ["notstring", "king", "thing", "sing"]


val test2_1 = get_substitutions1 ([], "foo") = []

val test2_2 = get_substitutions1 ([["foo"],[]], "foo") = []

val test2_3 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test2_4 = get_substitutions1 ([["bar"],["there"]], "foo") = []

val test2_5 = get_substitutions1 ([["foo", "bar"],["there"]], "foo") = ["bar"]

val test2_6 = get_substitutions1 ([["foo", "bar"], ["there", "foo"]], "foo") = ["bar", "there"]

val test2_7 = get_substitutions1 ([["foo", "bar"], ["there", "foo", "bar"]], "foo") = ["bar", "there", "bar"]


val test3_1 = get_substitutions2 ([], "foo") = []

val test3_2 = get_substitutions2 ([["foo"],[]], "foo") = []

val test3_3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test3_4 = get_substitutions2 ([["bar"],["there"]], "foo") = []

val test3_5 = get_substitutions2 ([["foo", "bar"],["there"]], "foo") = ["bar"]

val test3_6 = get_substitutions2 ([["foo", "bar"], ["there", "foo"]], "foo") = ["bar", "there"]

val test3_7 = get_substitutions2 ([["foo", "bar"], ["there", "foo", "bar"]], "foo") = ["bar", "there", "bar"]

val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
        [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []


val test8_1 = all_same_color [(Hearts, Ace)] = true

val test8_2 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test8_3 = all_same_color [(Hearts, Ace), (Hearts, King), (Clubs, Ace)] = false

val test8_4 = all_same_color [(Hearts, Ace), (Hearts, King), (Clubs, Ace), (Hearts, King)] = false

val test9_1 = sum_cards [] = 0

val test9_2 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test9_3 = sum_cards [(Clubs, King)] = 10

val test9_4 = sum_cards [(Clubs, Ace), (Clubs, Num 9), (Clubs, Num 5)] = 25


val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test10_2 = score ([(Hearts, Num 9),(Clubs, Num 4)],10) = 9

val test10_4 = score ([(Hearts, King), (Diamonds, Num 4)],10) = 6

val test10_3 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)


