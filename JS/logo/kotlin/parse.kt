package compile.parse
import compile.lex.*

// maybe we should let this be for later

fun main() {
    val sample = "do 10 { fd 30 rt 36 }"
    val toks = tokenize(sample)
    println(toks.joinToString(separator=",\n"))
}

/*
AST
~\
   do - 10
    \BLOCK
    fd - 30
    |
    rt - 36
 */