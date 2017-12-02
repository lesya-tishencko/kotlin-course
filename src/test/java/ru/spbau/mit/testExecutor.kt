package ru.spbau.mit

import kotlin.test.assertEquals
import org.junit.Test

class TestExecutor {
    @Test
    fun testFunction() {
        val a = IdentifierNode("a")
        val foo = IdentifierNode("foo")
        val scopeA = Scope(mutableMapOf(a to VariableNode(a, null)))
        val function = FunctionNode(foo, ParameterNamesNode(mutableListOf(IdentifierNode("a"))),
                BlockWithBracesNode(BlockNode(mutableListOf(ReturnNode(
                        BinaryExpressionNode(IdentifierNode("a"), LiteralNode(1), KeyWord.ADD))),
                        scopeA
                        )))
        val scopeFun = Scope(mutableMapOf(foo to function))
        val returnNode = ReturnNode(FunctionCallNode(foo, ArgumentsNode(mutableListOf(LiteralNode(20)))))
        val mainNode = BlockNode(mutableListOf(function, returnNode), scopeFun)

        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals("RETURN STATEMENT", visitor.stack.last())
        visitor.stack.remove(visitor.stack.last())
        assertEquals(21, visitor.stack.last())
    }

    @Test
    fun testIf() {
        val expr = BinaryExpressionNode(LiteralNode(5), LiteralNode(0), KeyWord.EQ)
        val thenBlock = BlockWithBracesNode(BlockNode(mutableListOf(ReturnNode(LiteralNode(1))), Scope()))
        val elseBlock = BlockWithBracesNode(BlockNode(mutableListOf(ReturnNode(LiteralNode(0))), Scope()))

        val mainNode = IfNode(expr, thenBlock, elseBlock)

        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals("RETURN STATEMENT", visitor.stack.last())
        visitor.stack.remove(visitor.stack.last())
        assertEquals(0, visitor.stack.last())
    }

    @Test
    fun testWhile() {
        val id = IdentifierNode("a")
        val valueNode = VariableNode(id, LiteralNode(3))
        val expr = BinaryExpressionNode(id, LiteralNode(5), KeyWord.LT)
        val scope = Scope(mutableMapOf(id to valueNode))
        val body = AssignmentNode(id, BinaryExpressionNode(id, LiteralNode(1), KeyWord.ADD))
        val block = BlockWithBracesNode(BlockNode(mutableListOf(body), scope))
        val whileNode = WhileNode(expr, block)
        val returnNode = ReturnNode(id)

        val mainNode = BlockNode(mutableListOf(valueNode, whileNode, returnNode), scope)
        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals("RETURN STATEMENT", visitor.stack.last())
        visitor.stack.remove(visitor.stack.last())
        assertEquals(5, visitor.stack.last())
    }

    @Test
    fun testAutoReturn() {
        val programText = listOf("fun foo() { println(0) }", "foo()")
        val lexer = Lexer(programText)
        lexer.setSource()
        val parser = Parser(lexer)

        val mainNode = parser.parseBlockNode(Scope()).component1()
        val visitor = Executor()
        mainNode.visit(visitor)
        assertEquals(0, visitor.stack.last())
    }
}
