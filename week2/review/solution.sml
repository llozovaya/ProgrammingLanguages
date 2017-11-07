(*1*)
fun is_older(first : int*int*int, second : int*int*int) =
  (* check years *)
  if #1 first < #1 second
  then true
  else if #1 first > #1 second
  then false
  (* check months *)
  else if #2 first < #2 second
  then true
  else if #2 first > #2 second
  then false
  (* check days *)
  else if #3 first < #3 second
  then true
  else false;

(*2*)
fun number_in_month(datesList : (int*int*int) list, month : int)  =
  if null datesList
  then 0
  else
      let
	  val actualMonth = #2 (hd datesList);
	  val result = if actualMonth = month then 1 else 0;
      in
	  result + number_in_month(tl datesList, month)
      end;

(*3*)
fun number_in_months(datesList : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(datesList, hd months) + number_in_months(datesList, tl months);

(*4*)
fun dates_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else let
      val result = dates_in_month(tl dates, month);
      val date = hd dates;
      val isIn = (#2 date = month);
  in
      if isIn
      then date::result
      else result
  end;

(*5*)
fun append(list1 : (int*int*int) list, list2 : (int*int*int) list) =
  if null list1
  then list2
  else hd list1 :: append(tl list1, list2);

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else let
      val tailResult = dates_in_months(dates, tl months);
      val headResult = dates_in_month(dates, hd months);
  in
      append(headResult, tailResult)
  end;

(*6*)
fun get_nth(list : string list, index : int) =
  if index = 1
  then hd list
  else get_nth(tl list, index - 1);

(*7*)
fun date_to_string(date : int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
      val month = get_nth(months, #2 date);
      val day = #3 date;
      val year = #1 date;
  in
      month ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end;

(*8*)
fun number_before_reaching_sum(sum : int, elements : int list) =
  let
      val first = hd elements;
      val result = sum - first;
  in
      if result <= 0
      then 0
      else 1 + number_before_reaching_sum(result, tl elements)
  end;

(*9*)
fun what_month(day : int) =
  let
      val daysInMonth = [31,28,31,30,31,30,31,31,30,31,30,31];
  in
      number_before_reaching_sum(day, daysInMonth) + 1
  end;

(*10*)
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else let
      val tailResult = month_range(day1 + 1, day2);
  in
      what_month(day1) :: tailResult
  end;

(*11*)
fun oldest(dates : (int*int*int) list) =
  if null dates
  then NONE
  else let
      val prevOldest = oldest(tl dates);
      val actual = hd dates;
  in
      if isSome prevOldest
      then if is_older(actual, valOf prevOldest)
	   then SOME actual
	   else prevOldest
      else SOME actual
  end;

(*12*)
fun isDuplicated(duplicatedList : int list, element : int) =
  if null duplicatedList
  then false
  else (hd duplicatedList = element) orelse isDuplicated(tl duplicatedList, element);

fun removeDuplicates(duplicatedList : int list) =
  if null duplicatedList
  then []
  else
      if isDuplicated(tl duplicatedList, hd duplicatedList)
      then removeDuplicates(tl duplicatedList)
      else (hd duplicatedList) :: removeDuplicates(tl duplicatedList);

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
  let
      val monthsWithoutDuplicates = removeDuplicates(months);
  in
      number_in_months(dates, monthsWithoutDuplicates)
  end;

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
  let
      val monthsWithoutDuplicates = removeDuplicates(months);
  in
      dates_in_months(dates, monthsWithoutDuplicates)
  end;

(*13*)
fun get(list : int list, index : int) =
  if index = 1
  then hd list
  else get(tl list, index - 1);

fun reasonable_date(date : int*int*int) =
  let
      val year = #1 date;
      val month = #2 date;
      val day = #3 date;
      val leapYear = (year mod 4 = 0) andalso ((year mod 100 <> 0) orelse (year mod 400 = 0));
      val februaryLength = if leapYear then 29 else 28;
      val monthsLength = [31,februaryLength,31,30,31,30,31,31,30,31,30,31];
      val monthLength = get(monthsLength,month);
  in
      if year <= 0
      then false
      else if month < 1 orelse month > 12
      then false
      else if day <= 0 orelse day > monthLength
      then false
      else true
  end;
