package ru.spbau.mit

import kotlin.test.assertEquals
import org.junit.Test

class TestMain {
    @Test
    fun testFirstExample() {
        val programText = listOf("fun foo(n) {", "    fun bar(m) {", "        return m + n",
                "    }", "", "    return bar(1)", "}", "", "return foo(41)")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer.programTokens)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals(42, visitor.stack.last())
    }

    @Test
    fun testSecondExample() {
        val programText = listOf("fun fib(n) {", "    if (n <= 1) {", "        return 1", "    }",
                "    return fib(n - 1) + fib(n - 2)", "}", "", "        var i = 1", "while (i <= 5) {",
                "    println(i, fib(i))",  "    i = i + 1",  "}", "return fib(5)")

        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer.programTokens)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals("RETURN STATEMENT", visitor.stack.last())
        visitor.stack.remove(visitor.stack.last())
        assertEquals(8, visitor.stack.last())
    }

    @Test
    fun testThirdExample() {
        val programText = listOf("var a = 10", "var b = 20", "if (a > b) {", "    return 1",
                "} else {", "    return 0", "}")

        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer.programTokens)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals("RETURN STATEMENT", visitor.stack.last())
        visitor.stack.remove(visitor.stack.last())
        assertEquals(0, visitor.stack.last())
    }
}
