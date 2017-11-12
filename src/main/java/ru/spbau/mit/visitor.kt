package ru.spbau.mit

abstract class Visitor {
    abstract fun visitIdentifierNode(node: IdentifierNode)
    abstract fun visitLiteralNode(node: LiteralNode)
    abstract fun visitBinaryExpressionNode(node: BinaryExpressionNode)
    abstract fun visitFunctionCallNode(node: FunctionCallNode)
    abstract fun visitArgumentsNode(node: ArgumentsNode)
    abstract fun visitAssigmentNode(node: AssigmentNode)
    abstract fun visitWhileNode(node: WhileNode)
    abstract fun visitBlockNode(node: BlockNode)
    abstract fun visitVariableNode(node: VariableNode)
    abstract fun visitParameterNamesNode(node: ParameterNamesNode)
    abstract fun visitBlockWithBracesNode(node: BlockWithBracesNode)
    abstract fun visitIfNode(node: IfNode)
    abstract fun visitReturnNode(node: ReturnNode)
    abstract fun visitFunctionNode(node: FunctionNode)
    abstract fun visitExpressionNode(node: ExpressionNode)
    abstract fun visitStatementNode(node: StatementNode)
}

class Executor: Visitor() {
    val stack = mutableListOf<Any>()
    val scopesList = mutableListOf<Scope>()

    override fun visitIdentifierNode(node: IdentifierNode) {
        val variable = scopesList.last().get(node) ?: throw ExecutorError("Uninitialized variable ${node.name}")
        visitExpressionNode((variable as VariableNode).expr!!)
    }

    override fun visitLiteralNode(node: LiteralNode) {
        stack.add(node.value)
    }

    override fun visitBinaryExpressionNode(node: BinaryExpressionNode) {
        visitExpressionNode(node.left)
        visitExpressionNode(node.right)
        val right = stack.last()
        stack.remove(stack.last())
        val left = stack.last()
        stack.remove(stack.last())
        stack.add(getOpByToken(node.op)(left, right))
    }

    override fun visitFunctionCallNode(node: FunctionCallNode) {
        if (node.id == IdentifierNode("println")) {
            for (arg in node.arguments?.expressions!!) {
                visitExpressionNode(arg)
                print(stack.last())
                print(" ")
                stack.remove(stack.last())
            }
            println()
        } else {
            val function = scopesList.last().get(node.id) as FunctionNode
            var index = 0
            for (arg in function?.arguments?.idList!!) {
                val value = function.body.block.scope.get(arg) as VariableNode
                node.arguments?.expressions?.get(index)?.let { value?.setExpression(it) }
                index++
            }
            visitBlockWithBracesNode(function.body)
            if (checkReturnStatement()) stack.remove(stack.last())
        }
    }

    override fun visitArgumentsNode(node: ArgumentsNode) {}

    override fun visitAssigmentNode(node: AssigmentNode) {
        val variable = scopesList.last().get(node.id) as VariableNode
        variable?.setExpression(node.expr)
    }

    override fun visitWhileNode(node: WhileNode) {
        while (true) {
            visitExpressionNode(node.expr)
            val condition = stack.last() as Boolean
            stack.remove(stack.last())
            if (condition != null && !condition) break
            visitBlockWithBracesNode(node.body)
            if (checkReturnStatement()) break
        }
    }

    override fun visitBlockNode(node: BlockNode) {
        scopesList.add(node.scope)
        for (statement in node.statementList) {
            visitStatementNode(statement)
            if (checkReturnStatement()) break
        }
        scopesList.remove(scopesList.last())
    }

    override fun visitVariableNode(node: VariableNode) {

    }

    override fun visitParameterNamesNode(node: ParameterNamesNode) {

    }

    override fun visitBlockWithBracesNode(node: BlockWithBracesNode) {
        visitBlockNode(node.block)
    }

    override fun visitIfNode(node: IfNode) {
        visitExpressionNode(node.expr)
        val condition = stack.last() as Boolean
        stack.remove(stack.last())
        if (condition != null && condition) {
            visitBlockWithBracesNode(node.thenBlock)
        } else if (node.elseBlock != null) {
            visitBlockWithBracesNode(node.elseBlock)
        }
    }

    override fun visitReturnNode(node: ReturnNode) {
        visitExpressionNode(node.expr)
        stack.add("RETURN STATEMENT")
    }

    override fun visitFunctionNode(node: FunctionNode) {

    }

    override fun visitExpressionNode(node: ExpressionNode) {
        node.visit(this)
    }

    override fun visitStatementNode(node: StatementNode) {
        node.visit(this)
    }

    private fun getOpByToken(token: KeyWord) = when(token) {
        KeyWord.MOD -> { a: Any, b: Any -> a as Int % b as Int }
        KeyWord.LE -> { a: Any, b: Any -> a as Int <= b as Int }
        KeyWord.ADD -> { a: Any, b: Any -> a as Int + b as Int }
        KeyWord.DIV -> { a: Any, b: Any -> a as Int / b as Int }
        KeyWord.EQ -> { a: Any, b: Any -> a == b }
        KeyWord.GE -> { a: Any, b: Any -> a as Int >= b as Int }
        KeyWord.GT -> { a: Any, b: Any -> a as Int > b as Int }
        KeyWord.LT -> { a: Any, b: Any -> b as Int > a as Int }
        KeyWord.SUB -> { a: Any, b: Any -> a as Int - b as Int }
        KeyWord.MUL -> { a: Any, b: Any -> a as Int * b as Int }
        KeyWord.NEQ -> { a: Any, b: Any -> a != b }
        KeyWord.AND -> { a: Any, b: Any -> a as Boolean && b as Boolean }
        KeyWord.OR -> { a: Any, b: Any -> a as Boolean || b as Boolean }
        else -> { a: Any, b: Any -> 0 }
    }

    private fun checkReturnStatement() = when {
        stack.size == 0 -> false
        stack.last() !is String -> false
        else -> stack.last() == "RETURN STATEMENT"
    }
}

open class ExecutorError(message: String? = null) : Throwable()