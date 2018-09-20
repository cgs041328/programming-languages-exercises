fun is_older(first : int*int*int, second : int*int*int) =
    if #1 first <> #1 second then #1 first < #1 second
    else
        if #2 first <> #2 second then #2 first < #2 second
        else
            if #3 first <> #3 second then #3 first < #3 second
            else false

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates then 0
    else
        let
            val acc = (if #2 (hd dates) = month then 1 else 0)
        in
            number_in_month(tl dates, month) + acc
        end

fun number_in_months(dates : (int*int*int) list, months : int list) = 
    if null months then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates then []
    else
        if #2 (hd dates) = month then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

fun date_to_string (date:int*int*int) =
    let val months = ["January",
                     "February",
                     "March",
                     "April",
                     "May",
                     "June",
                     "July",
                     "August",
                     "September",
                     "October",
                     "November",
                     "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum - hd xs <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month (n : int) =
    number_before_reaching_sum(n, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range(day1 : int, day2 : int) = 
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(xs :  (int * int * int) list) = 
    if null xs then NONE
    else let fun oldest'(dates : (int * int * int) list) =
                 if null (tl dates) then hd dates
                 else let val restOldest = oldest'(tl dates)
                      in  if is_older(hd dates,restOldest)
                          then hd dates
                          else restOldest
                      end
         in SOME(oldest'(xs))
         end

fun elem (x : int, xs : int list) =
    if null xs
    then false
    else if (x = hd xs)
    then true
    else elem (x, tl xs)

fun remove_duplicates (xs : int list) =
    if null xs
    then []
    else if elem(hd xs, tl xs)
    then remove_duplicates(tl xs)
    else hd xs :: remove_duplicates(tl xs)

fun number_in_months_challenge (dates: (int*int*int) list, mons: int list) =
    number_in_months(dates, remove_duplicates(mons))

fun dates_in_months_challenge (dates: (int*int*int) list, mons: int list) = 
    dates_in_months(dates, remove_duplicates(mons))

fun reasonable_date (date : int*int*int) = 
    let fun is_leap (year : int) =
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        fun get_nth (xs : int list, n : int) =
            if n = 1
            then hd xs
            else get_nth(tl xs, n-1)
        val y = #1 date
        val m = #2 date
        val d = #3 date
        val month_dates = [31, if is_leap(y) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        y > 0 andalso m >= 1 andalso m <= 12 andalso d >= 1 andalso d <= get_nth(month_dates, m)
    end