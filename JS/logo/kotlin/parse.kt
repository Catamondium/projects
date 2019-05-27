package compile.parse
import compile.lex.

enum class NodeType {INTEGER, FUNCTION, ROOT}
data class Node(
    val type: NodeType,
    val value: String, 
    val children: List<Node>)
typealias ParsePair = Pair<Int, Node?>

interface Parse {
    operator fun invoke(input: List<Token>) : ParsePair
}

class Literal(type: NodeType, tok: TokenType) : Parse {
    override operator fun invoke(input: List<Token>) : ParsePair {
        if (input[0].type == tok) {
            return Pair(
                1,
                Node(
                    type,
                    input[0].value,
                    emptyList()))
        } else {
            return Pair(0, null)
        }
    }
}

val integral = Literal(NodeType.INTEGER, TokenType.INTEGER)

class Function(val verb: String, val operands: Int) : Parse {
    override operator fun invoke(input: List<Token>) : ParsePair {
        //////////////
    }
}

class AST : Parse {
    val children : MutableListOf<Node>()
    override operator fun invoke(input: List<Token>) : ParsePair
}

fun main() {
    val sample = "do 10 { fd 30 rt 36 }"
    val toks = tokenize(sample)
    println(toks.joinToString(separator=",\n"))
}