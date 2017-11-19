package ru.spbau.mit

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

fun main(args: Array<String>) {
    val mainPath = Paths.get(args[0])
    val programText = mutableListOf<String>()
    Files.lines(mainPath, StandardCharsets.UTF_8).forEach { elem -> programText.add(elem) }
    val lexer = Lexer(programText)
    lexer.setSource()
    val parser = Parser(lexer.programTokens)

    val mainNode = parser.parseBlockNode(Scope()).component1()
    val visitor = Executor()
    mainNode.visit(visitor)
}
