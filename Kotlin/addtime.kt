import kotlin.system.exitProcess
import kotlin.KotlinVersion

class Time(val hrs: Int, val mins: Int) {
    val abs = hrs * 60 + mins

    constructor(m: Int) : this((m / 60).toInt(), m % 60)

    override fun toString(): String {
        return "%02d:%02d".format(hrs, mins)
    }

}

operator fun Time.plus(mins_in: Int): Time {
    val tot = this.abs + mins_in
    return Time(tot)
}

operator fun Time.plus(other: Time): Time {
    val tot = this.abs + other.abs
    return Time(tot)
}

fun usage() {
    println("""
    |Usage: addtime [-qhl] HH:MM HH:MM|mins_elapse
    |Note: if mins_elapse is negative, precede it with '--'
    |Options:
    |        -q quietly output end time
    |        -h print this message and exit
    |        -l print version information
    """.trimMargin())
    exitProcess(1)
}

fun String.toTime(): Time {
    if (':' in this) {
        val (hrs, mins) = this.split(':')
        return Time(hrs.toInt(), mins.toInt())
    } else {
        return Time(this.toInt())
    }
}

fun main(args: Array<String>) {
    val re = "^-[a-zA-Z]+".toRegex()
    val (opts, free) = args.partition { thing -> re.matches(thing) }

    if ("-l" in opts) {
        println("Kotlin: %s".format(KotlinVersion.CURRENT))
        exitProcess(0)
    }
    else if ("-h" in opts || free.size < 2) {
        usage()
    }

    val (str_start, str_elapse) = free
    val start = str_start.toTime()
    val elapse = str_elapse.toTime()

    if ("-q" in opts) {
        println("${start + elapse}")
    } else {
        println("Start:\t%s\t%+d".format(start, elapse.abs))
        println("End:\t%s".format(start + elapse))
    }
}
