package ru.spbau.mit
import kotlin.test.assertEquals
import org.junit.Test
import kotlin.test.assertNotNull

class TestParser {
    @Test
    fun testFirstExample() {
        val programText = listOf("fun foo(n) {", "    fun bar(m) {", "        return m + n",
                "    }", "", "    return bar(1)", "}", "", "println(foo(41))")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        assert(mainNode.statementList[0] is FunctionNode)
        assert(mainNode.statementList[1] is FunctionCallNode)

        var funNode = mainNode.statementList[0] as FunctionNode
        assertEquals(IdentifierNode("foo"), funNode.id)
        assert(funNode.body.block.statementList[0] is FunctionNode)
        assert(funNode.body.block.statementList[1] is ReturnNode)

        val returnNode = (funNode.body.block.statementList[1] as ReturnNode)!!
        val expectedReturnNode = ReturnNode(FunctionCallNode(IdentifierNode("bar"), ArgumentsNode(mutableListOf(LiteralNode(1)))))
        assertEquals(expectedReturnNode, returnNode)

        funNode = funNode.body.block.statementList[0] as FunctionNode
        assertEquals(IdentifierNode("bar"), funNode.id)
        val body = ((funNode.body.block.statementList[0] as ReturnNode).expr as BinaryExpressionNode)
        assertEquals(BinaryExpressionNode(IdentifierNode("m"), IdentifierNode("n"), KeyWord.ADD), body)
    }

    @Test
    fun testSecondExample() {
        val programText = listOf("fun fib(n) {", "    if (n <= 1) {", "        return 1", "    }",
            "    return fib(n - 1) + fib(n - 2)", "}", "", "        var i = 1", "while (i <= 5) {",
            "    println(i, fib(i))",  "    i = i + 1",  "}")

        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        assert(mainNode.statementList[0] is FunctionNode)
        assert(mainNode.statementList[1] is VariableNode)
        assert(mainNode.statementList[2] is WhileNode)

        val ifNode = ((mainNode.statementList[0] as FunctionNode).body.block.statementList[0] as IfNode)!!
        assertEquals(BinaryExpressionNode(IdentifierNode("n"), LiteralNode(1), KeyWord.LE), ifNode.expr)
        assertEquals(ReturnNode(LiteralNode(1)), ifNode.thenBlock.block.statementList[0])

        val valNode = mainNode.statementList[1] as VariableNode
        assertEquals(VariableNode(IdentifierNode("i"), LiteralNode(1)), valNode)

        val whileNode = mainNode.statementList[2] as WhileNode
        assertEquals(BinaryExpressionNode(IdentifierNode("i"), LiteralNode(5), KeyWord.LE), whileNode.expr)
        val printlnNode = (whileNode.body.block.statementList[0] as FunctionCallNode)!!
        assertEquals(IdentifierNode("println"), printlnNode.id)
        assertEquals(2, printlnNode.arguments!!.expressions.size)
        assert(printlnNode.arguments!!.expressions[1] is FunctionCallNode)
    }

    @Test
    fun testThirdExample() {
        val programText = listOf("var a = 10", "var b = 20", "if (a > b) {", "    println(1)",
            "} else {", "    println(0)", "}")

        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        assert(mainNode.statementList[0] is VariableNode)
        assert(mainNode.statementList[1] is VariableNode)
        assert(mainNode.statementList[2] is IfNode)

        val valNode = mainNode.statementList[0] as VariableNode
        assertEquals(VariableNode(IdentifierNode("a"), LiteralNode(10)), valNode)

        val ifNode = mainNode.statementList[2] as IfNode
        assertEquals(BinaryExpressionNode(IdentifierNode("a"), IdentifierNode("b"), KeyWord.GT), ifNode.expr)
        assertNotNull(ifNode.elseBlock)
        val elseExpr = (ifNode.elseBlock!!.block.statementList[0] as FunctionCallNode)!!
        assertEquals(FunctionCallNode(IdentifierNode("println"), ArgumentsNode(mutableListOf(LiteralNode(0)))), elseExpr)
    }

    @Test(expected = ParserError::class)
    fun testRandomSequence() {
        val programText = listOf("eiuytf iuyg 1234 = 234")
        var lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        parser.parseBlockNode(Scope()).component1()
    }

    @Test(expected = ParserError::class)
    fun testErrorNotFoundBraces() {
        val programText = listOf("var a = 4", "if (a > 5) a + 1")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        parser.parseBlockNode(Scope()).component1()
    }

    @Test(expected = ParserError::class)
    fun testErrorNotFoundParen() {
        val programText = listOf("var a = 4", "if a > 5 { a + 1 }")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)
        parser.parseBlockNode(Scope()).component1()
    }

    @Test(expected = ParserError::class)
    fun testErrorNotDeclaredVar() {
        val programText = listOf("if (a > 5)", "{ a + 1 }")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        parser.parseBlockNode(Scope()).component1()
    }

    @Test(expected = ParserError::class)
    fun testErrorNotDeclaredFun() {
        val programText = listOf("var a = 4", "if (a > 5)", "{ foo(2) }")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        parser.parseBlockNode(Scope()).component1()
    }

    @Test
    fun testPrintln() {
        val programText = listOf("var a = 4", "if (a > 5)", "{ println(2) }")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        assert(mainNode.statementList[1] is IfNode)
    }
}
