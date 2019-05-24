class Time(val hrs: Int, val mins: Int) {
    constructor(s: String): this(s.split(':')) // must delegate?
    // Guess we're supporting List<String> too
    constructor(s: List<String>): this(s.getOrNull(0)?.toInt() ?: 0, s.getOrNull(1)?.toInt() ?: 0)
    override fun toString(): String {
      return "%02d:%02d".format(hrs, mins)
    }

    fun abs(): Int {
        return hrs*60 + mins
    }
}
operator fun Time.plus(mins_in: Int): Time {
    val tot = this.abs() + mins_in
    return Time((tot/60).toInt(), tot % 60)
}

fun main(args: Array<String>) {
    val t = Time(1, 30) + 30
    println(t)
    println(Time("1"))
}