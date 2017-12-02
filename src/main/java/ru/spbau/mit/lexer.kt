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
    private val programTokens: MutableList<MutableList<Token>> = mutableListOf()

    private var lineNumber = 0
    private var positionInLine = 0

    fun setSource() = programText.forEach {
        line -> programTokens.add(getNextLine(line))
    }

    fun isEndOfProgram() = programTokens.size == lineNumber

    fun isEndOfLine() = positionInLine == 0

    fun getLineNumber() = lineNumber

    fun getCurrentToken() = programTokens[lineNumber][positionInLine]

    fun showNextToken() = programTokens[lineNumber][positionInLine + 1]

    fun tryIncrementPosition() {
        when {
            programTokens[lineNumber].size == positionInLine + 1 -> {
                lineNumber++
                while(lineNumber < programTokens.size && programTokens[lineNumber].isEmpty()) lineNumber++
                positionInLine = 0
            }
            else -> positionInLine++
        }
    }

    fun incrementPosition(count: Int = 1) {
        (1..count).forEach { _ ->
            tryIncrementPosition()
            if (lineNumber == programTokens.size) throw ParserError("Unexpected end of file in line$lineNumber")
        }
    }

    fun decrementPosition(count: Int = 1) {
        (1..count).forEach { _ ->
            when (positionInLine) {
                0 -> {
                    lineNumber--
                    while (programTokens[lineNumber].isEmpty()) lineNumber--
                    positionInLine = programTokens[lineNumber].size - 1
                }
                else -> positionInLine--
            }
        }
    }

    private fun getNextLine(line: String): MutableList<Token> {
        val tokenList: MutableList<Token> = mutableListOf()
        var pos = 0
        fun getNextWord(): String {
            while (pos < line.length && line[pos].isWhitespace()) pos += 1
            if (pos >= line.length) return ""
            var result = ""
            while (pos < line.length && !line[pos].isWhitespace()) {
                result += when (line[pos]) {
                    '/' -> if (result.last() == '/') return "" else line[pos++]
                    '(', ')', ',' -> return if (result.isEmpty()) line[pos++].toString() else result
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
