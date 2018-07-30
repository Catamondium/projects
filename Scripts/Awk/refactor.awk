#!/usr/bin/awk -f
# GNU Awk 4.1.3

(/^#([0-9]*)/) { # main, $1=title, $2=copies, $3=price
    if (NF == 3) {
	# Calc sub-tot
	amount = $2 * $3
	# Accumulate sub-tot
	vol[$1] += $2
	amt[$1] += amount
	#accumulate tot
	total_vol += $2
	total_amt += amount
    } else
	printf "Malformed record: %s, line %s\n", FILENAME, NR
}

END { # Post-main
    printf "%5s\t%10s\t%8s\n\n", "TITLE", "COPIES SOLD", "TOTAL" #header
    for (title in vol) #body
	printf "%5s\t%11d\t£%7.2f\n", title, vol[title], amt[title]
    printf "%s\n", "-------------" #footer
    printf "%s\t%11d\t£%7.2f\n", "Total: ", total_vol, total_amt
}
