fun is_older (date1 : int * int * int, date2 : int * int * int) =
    #1 date1 < #1 date2 orelse
    #1 date1 = #1 date2 andalso #2 date1 < #2 date2 orelse
    #2 date1 = #2 date2 andalso #3 date1 < #3 date2

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
        let val date = hd dates
            val mth = #2 date
            val in_month = if mth = month then 1 else 0
        in
            in_month + number_in_month(tl dates, month)
        end

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
        let val date = hd dates
            val tail = tl dates
        in
          if #2 date = month
          then date::dates_in_month(tail, month)
          else dates_in_month(tail, month)
        end

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings : string list, n : int) =
    if n <= 1
    then hd strings
    else get_nth (tl strings, n-1)

fun date_to_string (date : int*int*int) =
    let val month_names = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, numbers: int list) =
    if null numbers then 0
    else
        if hd numbers >= sum then 0
        else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month(day : int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(day, days) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun oldest_nonempty(dates : (int*int*int) list) =
                if null (tl dates)
                then hd dates
                else let val old = oldest_nonempty(tl dates)
                         val head = hd dates
                     in
                        if is_older(head, old)
                        then head
                        else old
                     end
        in
          SOME (oldest_nonempty dates)
        end

fun remove_duplicates (numbers: int list) =
    if null numbers
    then []
    else
        let fun contains (nums : int list, n : int) =
                if null nums
                then false
                else if n = hd nums
                    then true
                    else contains(tl nums, n)
            val head = hd numbers
            val tail = tl numbers
        in
            if contains(tail, head)
            then remove_duplicates(tail)
            else head::remove_duplicates(tail)
        end


fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : int*int*int) =
    let val year = #1 date
        fun is_leap(year : int) =
            (year mod 4 = 0) andalso (year mod 100 <> 0) orelse (year mod 400 = 0)
        val days = [31,(if is_leap(year) then 29 else 28),31,30,31,30,31,31,30,31,30,31]
        fun get_nth (nums : int list, n : int) =
            if n <= 1
            then hd nums
            else get_nth (tl nums, n-1)
        val month = #2 date
    in
        year > 0 andalso month > 1 andalso month < 12 andalso #3 date <= get_nth(days, month)
    end

