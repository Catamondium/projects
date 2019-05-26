/*
commands:
	fd num ==> move forward by num pixels,
	bk num ==> move back by num pixels,

	rt num ==> turn right by num degrees
	lt num ==> turn left by num degrees

	pu ==> pen = false
	pd ==> pen = true

	ht ==> hide turtle
	st ==> show turtle
	rst ==> pen = true, show turtle

	do num block ==> repeat 'block' num times
*/

enum class TokenType {PAREN, WORD, INTEGER, WHITESPACE}
data class Token(val type: TokenType, var value: String)

typealias RulePair = Pair<Int, Token?>
typealias Predicate = (ch: Char) -> Boolean

interface Rule {
    operator fun invoke(input: String) : RulePair
}

class CharRule(val type: TokenType, val ch: Char) : Rule {
    override operator fun invoke(input: String) : RulePair {
        return if (ch == input[0]) Pair(1, Token(type, ch.toString())) else Pair(0, null)
    }
}

class PatternRule(val type: TokenType, val pred: Predicate) : Rule {
    override operator fun invoke(input: String) : RulePair {
        var ch = input[0]
        if(pred(ch)) {
            val out = input.takeWhile(pred)
            return Pair(out.length, Token(type, out))
        }
        return Pair(0, null)
    }
}


val rules =  listOf(
    CharRule(TokenType.PAREN, '{'),
    CharRule(TokenType.PAREN, '}'),
    PatternRule(TokenType.WORD, {it in 'a'..'z' || it in 'A'..'Z'}),
    PatternRule(TokenType.INTEGER, {"-?[0-9]+".toRegex().matches(it.toString())}),
    PatternRule(TokenType.WHITESPACE, Char::isWhitespace)
    )

fun tokenize(input: String) : List<Token> {
    val ret = mutableListOf<Token>()
    var local = input.toLowerCase()
    while (0 != local.length) {
        var tokenized = false
        rules.forEach { fn ->
            if (tokenized) { return@forEach }
            val (consumed, tok) = fn(local)
            if (consumed != 0) {
                tokenized = true
                local = local.drop(consumed)
            }

            if (tok != null && tok.type != TokenType.WHITESPACE) {
                ret.add(tok)
            }
        }

        if (!tokenized) {
            throw Error("Bad character: ${local[0]}")
        }
    }

    return ret
}

fun main() {
    val sample = "do 10 { fd 30 rt 36 }"
    val toks = tokenize(sample)
    println(toks.joinToString(separator=",\n"))
}