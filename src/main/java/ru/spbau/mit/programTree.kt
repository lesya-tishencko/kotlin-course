package ru.spbau.mit

abstract class Node {
    abstract fun visit(visitor: Visitor)
}

abstract class ExpressionNode: StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitExpressionNode(this)
}

data class IdentifierNode(val name: String): ExpressionNode() {
    override fun visit(visitor: Visitor) = visitor.visitIdentifierNode(this)
}

data class LiteralNode(val value: Int): ExpressionNode() {
    override fun visit(visitor: Visitor) = visitor.visitLiteralNode(this)
}

data class BinaryExpressionNode(val left: ExpressionNode, val right: ExpressionNode, val op: KeyWord): ExpressionNode() {
    override fun visit(visitor: Visitor) = visitor.visitBinaryExpressionNode(this)
}

data class FunctionCallNode(val id: IdentifierNode, val arguments: ArgumentsNode?): ExpressionNode() {
    override fun visit(visitor: Visitor) = visitor.visitFunctionCallNode(this)
}

data class ArgumentsNode(val expressions: MutableList<ExpressionNode>): ExpressionNode() {
    override fun visit(visitor: Visitor) = visitor.visitArgumentsNode(this)
}

open class StatementNode : Node() {
    override fun visit(visitor: Visitor) = visitor.visitStatementNode(this)
}

data class AssignmentNode(val id: IdentifierNode, val expr: ExpressionNode): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitAssignmentNode(this)
}

data class WhileNode(val expr: ExpressionNode, val body: BlockWithBracesNode): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitWhileNode(this)
}

data class BlockNode(val statementList: MutableList<StatementNode>, val scope: Scope): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitBlockNode(this)
}

data class VariableNode(val id: IdentifierNode, var expr: ExpressionNode?): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitVariableNode(this)
}

data class ParameterNamesNode(val idList: MutableList<IdentifierNode>): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitParameterNamesNode(this)
}

data class BlockWithBracesNode(val block: BlockNode): Node() {
    override fun visit(visitor: Visitor) = visitor.visitBlockWithBracesNode(this)
}

data class IfNode(val expr: ExpressionNode, val thenBlock: BlockWithBracesNode, val elseBlock: BlockWithBracesNode?): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitIfNode(this)
}

data class ReturnNode(val expr: ExpressionNode): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitReturnNode(this)
}

data class FunctionNode(val id: IdentifierNode, val arguments: ParameterNamesNode?, val body: BlockWithBracesNode): StatementNode() {
    override fun visit(visitor: Visitor) = visitor.visitFunctionNode(this)
}

class Scope(val variables: MutableMap<IdentifierNode, StatementNode> = mutableMapOf(),
            private val notInitializedId: MutableSet<IdentifierNode> = mutableSetOf()) {

    fun add(id: IdentifierNode, variable: StatementNode) = variables.put(id, variable)
    fun add(id: IdentifierNode) = notInitializedId.add(id)
    fun get(id: IdentifierNode) = variables[id]
    fun contains(id: IdentifierNode) = variables.containsKey(id) || notInitializedId.contains(id)
    fun copy(): Scope {
        val newVariables = mutableMapOf<IdentifierNode, StatementNode>()
        variables.forEach { id, variable -> newVariables.put(id, variable) }
        val newIds = mutableSetOf<IdentifierNode>()
        notInitializedId.forEach { id -> newIds.add(id) }
        return Scope(newVariables, newIds)
    }
}