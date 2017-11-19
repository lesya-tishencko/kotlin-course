package ru.spbau.mit

abstract class Visitor {
    abstract fun visitIdentifierNode(node: IdentifierNode)
    abstract fun visitLiteralNode(node: LiteralNode)
    abstract fun visitBinaryExpressionNode(node: BinaryExpressionNode)
    abstract fun visitFunctionCallNode(node: FunctionCallNode)
    abstract fun visitArgumentsNode(node: ArgumentsNode)
    abstract fun visitAssignmentNode(node: AssignmentNode)
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
    private var scopesList = mutableListOf<Pair<IdentifierNode, StatementNode>>()

    private fun find(entity: IdentifierNode): StatementNode {
        return scopesList.reversed()
                .firstOrNull { (key, _) -> key == entity }
                ?.let { (_, value) -> value }
                ?: StatementNode();
    }

    override fun visitIdentifierNode(node: IdentifierNode) {
        val variable = find(node) as? VariableNode ?: throw ExecutorError("Using uninitialized variables")
        variable.expr?.let { visitExpressionNode(it) }
    }

    override fun visitLiteralNode(node: LiteralNode) {
        stack.add(node.value)
    }

    override fun visitBinaryExpressionNode(node: BinaryExpressionNode) {
        visitExpressionNode(node.left)
        visitExpressionNode(node.right)
        val right = stack.last()
        stack.removeAt(stack.lastIndex)
        val left = stack.last()
        stack.removeAt(stack.lastIndex)
        stack.add(getOpByToken(node.op)(left, right))
    }

    override fun visitFunctionCallNode(node: FunctionCallNode) {
        when {
            node.id == IdentifierNode("println") -> {
                node.arguments?.expressions!!.forEach { arg ->
                    visitExpressionNode(arg)
                    print(stack.last())
                    print(" ")
                    stack.removeAt(stack.lastIndex)
                }
                println()
            }
            else -> {
                val function = find(node.id) as FunctionNode
                function.body.block.scope.variables.forEach { (key, _) ->
                    val valueFromScope = find(key)
                    when (valueFromScope) {
                        is VariableNode -> function.body.block.scope.add(key, valueFromScope)
                    }
                }
                when {
                    function.arguments != null -> function.arguments.idList.withIndex().forEach { (index, arg) ->
                        val value = (function.body.block.scope.get(arg) as VariableNode).copy()
                        node.arguments?.expressions?.get(index)?.let { visitExpressionNode(it) }
                        when {
                            stack.size == 0 || stack.last() !is Int -> throw ExecutorError("Incorrect expression in function call ${function.id.name}")
                            else -> {
                                value.setExpression(LiteralNode(stack.last() as Int))
                                function.body.block.scope.add(value.id, value)
                                stack.removeAt(stack.lastIndex)
                            }
                        }
                    }
                }
                visitBlockWithBracesNode(function.body)
                if (checkReturnStatement()) stack.removeAt(stack.lastIndex)
            }
        }
    }

    override fun visitArgumentsNode(node: ArgumentsNode) {}

    override fun visitAssignmentNode(node: AssignmentNode) {
        val variable = find(node.id) as VariableNode
        visitExpressionNode(node.expr)
        if (stack.size == 0 || stack.last() !is Int) throw ExecutorError("Incorrect assignment")
        variable.setExpression(LiteralNode(stack.last() as Int))
        stack.removeAt(stack.lastIndex)
    }

    override fun visitWhileNode(node: WhileNode) {
        while (true) {
            visitExpressionNode(node.expr)
            val condition = stack.last() as Boolean
            stack.removeAt(stack.lastIndex)
            if (!condition) break
            visitBlockWithBracesNode(node.body)
            if (checkReturnStatement()) break
        }
    }

    override fun visitBlockNode(node: BlockNode) {
        val newScopeStatementsCount = node.scope.variables.size
        node.scope.variables.forEach { key, value -> scopesList.add(Pair(key, value)) }
        for (statement in node.statementList) {
            visitStatementNode(statement)
            if (checkReturnStatement()) break
        }
        scopesList = scopesList.take(scopesList.size - newScopeStatementsCount).toMutableList()
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
        stack.removeAt(stack.lastIndex)
        when {
            condition -> visitBlockWithBracesNode(node.thenBlock)
            node.elseBlock != null -> visitBlockWithBracesNode(node.elseBlock)
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
        else -> { _: Any, _: Any -> 0 }
    }

    private fun checkReturnStatement() = when {
        stack.size == 0 -> false
        stack.last() !is String -> false
        else -> stack.last() == "RETURN STATEMENT"
    }
}

open class ExecutorError(message: String? = null) : Throwable()