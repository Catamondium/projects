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
package compile.lex

enum class TokenType {
    PAREN,
    VERB,
    INTEGER,
    WHITESPACE
    }

data class Token(val type: TokenType, val value: String)

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

class PredRule(val type: TokenType, val pred: Predicate) : Rule {
    override operator fun invoke(input: String) : RulePair {
        val out = input.takeWhile(pred)
        return Pair(
            out.length,
            if (out.length != 0) 
                Token(type, out)
                else null)
    }
}


val rules =  listOf(
    CharRule(TokenType.PAREN, '{'),
    CharRule(TokenType.PAREN, '}'),
    PredRule(TokenType.VERB, Char::isLetter),
    PredRule(TokenType.INTEGER, {it == '-' || it.isDigit()}),
    PredRule(TokenType.WHITESPACE, Char::isWhitespace)
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