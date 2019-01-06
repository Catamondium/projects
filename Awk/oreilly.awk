#!/usr/bin/awk -f
# GNU Awk 4.1.3
BEGIN { FS = "\n"; RS = "" } # Init, lines are fields, no records

NF >= 3 { # Main, at least 3 fields?
    for (i = 3; i <= NF; ++i) {
	sv = split($i, order, " ") # split into title,copies,price
	if (sv == 3) {
	    # Bind vars
	    title = order[1] # Indexed at 1
	    copies = order[2]
	    price = order[3]
	    # Calc sub-tot
	    amount = copies * price
	    # Accumulate sub-tot
	    vol[title] += copies
	    amt[title] += amount
	    # Accumulate tot
	    total_vol += copies
	    total_amt += amount
	} else
	    print "Incomplete Record"
    }
}

END { # Post-main
    printf "%5s\t%10s\t%6s\n\n", "TITLE", "COPIES SOLD", "TOTAL" #header
    for (title in vol) #body
	printf "%5s\t%10d\t£%7.2f\n", title, vol[title], amt[title]
    printf "%s\n", "-------------" #totals
    printf "\t%s%4d\t£%7.2f\n", "Total ", total_vol, total_amt
}
