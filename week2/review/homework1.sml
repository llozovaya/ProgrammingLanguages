(* date is a triple int*int*int (y,m,d) *)
fun is_older(first: int * int * int, second: int * int * int) =
  let
    val y1 = #1 first
    val m1 = #2 first
    val d1 = #3 first
    val y2 = #1 second
    val m2 = #2 second
    val d2 = #3 second
  in
    y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
  end;

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates then 0
  else let
      val first_month_count = if #2 (hd dates) = month then 1 else 0
    in
      first_month_count + number_in_month(tl dates, month)
    end

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates then []
  else
    let
      val first_date = hd dates
      val remaining_dates = tl dates
    in
      if (#2 first_date) = month then first_date :: dates_in_month(remaining_dates, month)
      else dates_in_month(remaining_dates, month)
    end

fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(dates: string list, n: int) = if n = 1 then hd dates else get_nth(tl dates, n - 1)

fun date_to_string(date: int * int * int) =
  let
    val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    val y = #1 date
    val m = #2 date
    val d = #3 date
    val month = get_nth(month_names, m)
  in
    month ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
  end

fun number_before_reaching_sum(sum: int, xs: int list) =
  if sum <= (hd xs) then 0
  else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)

fun what_month(day: int) =
  let
    val month_lengths = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, month_lengths)
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2 then [] else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  let
    fun oldest_in_nonempty(dates: (int * int * int) list) =
      if null (tl dates) then hd dates
      else let
        val oldest_in_tail = oldest_in_nonempty(tl dates)
        fun older_of(d1, d2) = if is_older(d1, d2) then d1 else d2
      in
        older_of(hd dates, oldest_in_tail)
      end
  in
    if null dates then NONE
    else
      SOME(oldest_in_nonempty(dates))
  end
