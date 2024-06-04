(* 1. *)
fun is_older(date1: int * int * int, date2: int * int * int) =
    if (#1 date1) > (#1 date2)
       orelse ((#1 date1) = (#1 date2) andalso ((#2 date1) > (#2 date2)))
       orelse ((#1 date1) = (#1 date2) andalso ((#2 date1) = (#2 date2)) andalso ((#3 date1 > #3 date2)))
       orelse ((#1 date1) = (#1 date2) andalso ((#2 date1) = (#2 date2)) andalso ((#3 date1 = #3 date2)))
    then false
    else true

(* 2. *)
fun number_in_month(dates: (int * int * int) list, month: int) =
    if null(dates)
    then 0
    else
	if #2 (hd dates) = month
	then number_in_month(tl dates, month) + 1
	else number_in_month(tl dates, month);
	    
(* 3. *)
fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null(months)
    then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months);
	     
(* 4. *)
fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null(dates)
    then []
    else
	if #2 (hd dates) = month
	then
	    hd dates::dates_in_month(tl dates, month)
	else
	    dates_in_month(tl dates, month);

(* 5. *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null(months)
    then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);

(* 6. *)
fun get_nth(items: string list, nth_element: int) =
    if nth_element = 1
    then hd items
    else 
	get_nth(tl items, nth_element - 1);

(* 7. *)
fun date_to_string(date: (int * int * int)) =
    let
	val months_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* 8. *)
fun number_before_reaching_sum(sum: int, number_list: int list) =
    if sum <= hd number_list
    then 0
    else if (sum > hd number_list andalso sum <= (hd number_list) + hd (tl number_list))
    then
	1
    else
	1 + number_before_reaching_sum(sum - hd number_list, tl number_list);

(* 9. *)
fun what_month(year_day: int) =
    let
	val days_each_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(year_day, days_each_month)
    end

(* 10. *)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then
	[]
    else
	what_month(day1)::month_range(day1 + 1, day2);

(* 11. *)
fun oldest(dates:(int * int * int) list) =
    if null(dates)
    then NONE
    else
	if null(tl dates)
	then SOME(hd dates)
	else
	    let
		fun is_current_eval_date_older(current_eval_date: (int * int * int), date_compared_to: (int * int * int)) =
		    is_older(current_eval_date, date_compared_to)
	    in
		if (is_current_eval_date_older(hd dates, hd (tl dates)))
		then
		    oldest((hd dates)::(tl (tl dates)))
		else
		    oldest(tl dates)
	    end
