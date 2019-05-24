import kotlin.system.exitProcess

class Time(val hrs: Int, val mins: Int) {
    val abs = hrs*60 + mins
    
    constructor(s: String): this(s.split(':'))
    constructor(s: List<String>) : this(s.getOrNull(0)?.toInt() ?: 0, s.getOrNull(1)?.toInt() ?: 0)
    constructor(m: Int): this((m/60).toInt(), m % 60)
    
    override fun toString() : String {
      return "%02d:%02d".format(hrs, mins)
    }

}

operator fun Time.plus(mins_in: Int) : Time {
    val tot = this.abs + mins_in
    return Time(tot)
}

operator fun Time.plus(other: Time) : Time {
    val tot = this.abs + other.abs
    return Time(tot)
}

fun usage() {
    println("""
    |Usage: addtime [-qh] hh:mm mins_elapse
    |Note: if mins_elapse is negative, precede it with '--'
    |Options:
    |        -q quietly output end time
    |        -h print this message and exit
    """.trimMargin())
    exitProcess(1)
}

fun fromString(str: String) : Time {
    if (':' in str) {
        return Time(str)
    } else {
        return Time(str.toInt())
    }
}

fun main(args: Array<String>) {
    val re = "^-[a-zA-Z]+".toRegex()
    val (opts, free) = args.partition{thing -> re.matches(thing)}

    if ("-h" in opts || free.size < 2) {
        usage()
    }

    val (str_start, str_elapse) = free
    val start = fromString(str_start)
    val elapse = fromString(str_elapse)

    if (opts.contains("-q")) {
        println("${start + elapse}")
    } else {
        println("Start:\t$start\t${elapse.abs}\nEnd:\t${start + elapse}")
    }
}
