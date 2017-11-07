(* Leah Morrison *)
(* Homework 1 *)
(* Programming Languages Part A *)


(* 1.) Write a function is_older that takes two dates and returns a bool *)
(*     returns true if the first argument comes before the second  *)

fun is_older(d1: (int*int*int), d2: (int*int*int)) =
  if #1 d1 < #1 d2
  then true
  else (if #2 d1 < #2 d2
	then true
	else (if #3 d1 < #3 d2
		  then true
		  else false)
       )

(* 2.) Write a function number_in_month that takes in a list of dates 
       and a month and returns how many days in the list are in the month *)

fun number_in_month(d1:(int*int*int) list, d2:int) =
  if null d1
  then 0
  else (if #2(hd d1) = d2
        then 1 + number_in_month(tl d1, d2)
	else number_in_month(tl d1, d2)
       )

(* 3.) Function number_in_months that takes a list of dates and list of months
	   and returns the number of dates in the list of dates that are in
	   any of the months in the list of months  *)
fun number_in_months(d1:(int*int*int) list, d2: int list) =
  if null d2
  then 0
  else number_in_month(d1, hd d2) + number_in_months(d1, tl d2)


(* 4.) Function dates_in_month takes a list of dates and a month, returns
      a list holding the dates from the argument list of dates that are in
      the month (in the order that they are given)  *)
fun dates_in_month(d1:(int*int*int)list, d2: int) =
  if null d1
  then []
  else (if #2 (hd d1) = d2
	then hd d1::dates_in_month(tl d1, d2)
	else dates_in_month(tl d1, d2)
       )
	   

 (* 5. Function dates_in_months that takes a list of dates and a list of months
   and returns a list holding dates from the arg list of dates that are in any
   of the months in the list of months *)
fun dates_in_months(d1: (int*int*int)list, d2: int list) =
  if null d2
  then []
	   else dates_in_month(d1, hd d2)@dates_in_months(d1, tl d2)

(* 6. Function get_nth that takes a list of strings and an int n and returns the
  nth element of the list where the head of the list is 1st. *)
fun get_nth(str: string list, n: int) =
  if n=1
  then hd str
  else get_nth(tl str, n-1)
	      

 (* 7.) Function date_to_string takes a date and returns a string  *)
fun date_to_string(d1: int*int*int) =
  let
      val months=["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 d1)^ " " ^ Int.toString(#3 d1) ^", " ^ Int.toString(#1 d1)
  end


 (* 8.) Function number_before_reaching_sum, takes an int called sum and 
int list, and returns an int n such that the first n elements in the list
add up to less than sum (n+1 adds up to sum or more than sum) *)
fun number_before_reaching_sum(sum:int, l:int list) =
  let
      fun count(c: int, cur_sum: int, l:int list) =
      if hd l + cur_sum >= sum
      then c
      else count(c + 1, cur_sum + hd l, tl l)
  in
      count(0, 0, l)
  end
      


(* 9.) Function what_month takes a day of year and returns what month *)
fun what_month (day: int) =
  let
      val month = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
      number_before_reaching_sum(day, month)
  end
      

(* 10.) Function month_range takes two days of the year and returns an int list
       [m1, m2...mn] where m1 is the month of day 1, m2 is the month of day1+1
       and mn is the month of day2   *)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1):: month_range(day1+1, day2)
				     
