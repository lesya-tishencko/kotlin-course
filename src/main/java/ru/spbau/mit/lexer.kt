package ru.spbau.mit

abstract class Token

class LiteralToken(val int: Int) : Token()
class IdToken(val string: String) : Token()

enum class KeyWord  {
    LBRACE, RBRACE, COMMA, FUN, VAR, LPAREN, RPAREN, WHILE, IF, ELSE, ASSIGN, RETURN, ADD, SUB, MUL, DIV, MOD,
    GT, LT, GE, LE, EQ, NEQ, OR, AND
}

data class KeyWordToken(val tok: KeyWord) : Token()

val strToTokenMap = mapOf(
        "{" to KeyWord.LBRACE,
        "}" to KeyWord.RBRACE,
        "," to KeyWord.COMMA,
        "fun" to KeyWord.FUN,
        "var" to KeyWord.VAR,
        "(" to KeyWord.LPAREN,
        ")" to KeyWord.RPAREN,
        "while" to KeyWord.WHILE,
        "if" to KeyWord.IF,
        "else" to KeyWord.ELSE,
        "=" to KeyWord.ASSIGN,
        "return" to KeyWord.RETURN,
        "+" to KeyWord.ADD,
        "-" to KeyWord.SUB,
        "*" to KeyWord.MUL,
        "/" to KeyWord.DIV,
        "%" to KeyWord.MOD,
        ">" to KeyWord.GT,
        "<" to KeyWord.LT,
        ">=" to KeyWord.LE,
        "<=" to KeyWord.LE,
        "==" to KeyWord.EQ,
        "!=" to KeyWord.NEQ,
        "||" to KeyWord.OR,
        "&&" to KeyWord.AND
)

class Lexer(private val programText: List<String>) {
    val programTokens: MutableList<MutableList<Token>> = mutableListOf()

    fun setSource() = programText.forEach {
        line -> programTokens.add(getNextLine(line))
    }

    private fun getNextLine(line: String): MutableList<Token> {
        val tokenList: MutableList<Token> = mutableListOf()
        var pos = 0;
        fun getNextWord(): String {
            while (pos < line.length && line[pos].isWhitespace()) pos += 1
            if (pos >= line.length) return ""
            var result = ""
            while (pos < line.length && !line[pos].isWhitespace()) {
                result += when (line[pos]) {
                    '/' -> if (result.last() == '/') return "" else line[pos++]
                    '(', ')', ',' -> if (result.isEmpty()) return line[pos++].toString() else return result
                    else -> line[pos++]
                }
            }
            return result
        }

        while (true) {
            val tok = getNextWord()
            if (tok.isEmpty()) break
            when {
                strToTokenMap.containsKey(tok) -> tokenList.add(KeyWordToken(strToTokenMap.getValue(tok)))
                tok.toIntOrNull() != null -> tokenList.add(LiteralToken(tok.toInt()))
                else -> tokenList.add(IdToken(tok))
            }
        }
        return tokenList
    }

}
