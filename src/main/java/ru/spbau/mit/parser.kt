package ru.spbau.mit

class Parser(private val lexer: Lexer) {

    fun parseBlockNode(scope: Scope): Pair<BlockNode, Scope> {
        val statements: MutableList<StatementNode> = mutableListOf()
        var newScope = scope.copy()
        do {
            val (expr, nextScope) = parseStatementNode(newScope)
            statements.add(expr)
            newScope = nextScope
            lexer.tryIncrementPosition()
            if (lexer.isEndOfProgram()) break
            val nextTok = lexer.getCurrentToken() as? KeyWordToken
        } while (nextTok == null || nextTok.tok != KeyWord.RBRACE)
        return BlockNode(statements, newScope) to newScope
    }

    private fun parseStatementNode(scope: Scope): Pair<StatementNode, Scope> = when(lexer.getCurrentToken()) {
        KeyWordToken(KeyWord.FUN) -> parseFunctionNode(scope)
        KeyWordToken(KeyWord.VAR) -> parseVariableNode(scope)
        KeyWordToken(KeyWord.WHILE) -> parseWhileNode(scope)
        KeyWordToken(KeyWord.IF) -> parseIfNode(scope)
        KeyWordToken(KeyWord.RETURN) -> parseReturnNode(scope)
        else -> if (lexer.showNextToken() == KeyWordToken(KeyWord.ASSIGN)) parseAssignmentNode(scope)
        else parseExpressionNode(scope)
    }

    private fun parseFunctionCallNode(scope: Scope): Pair<FunctionCallNode, Scope> {
        val (id, _) = parseIdentifierNode(scope)
        if (id.name != "println" && !scope.contains(id))
            throw ParserError("Not found declaration ${id.name} in line${lexer.getLineNumber()}")
        lexer.incrementPosition()
        var paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN)
            throw ParserError("Expected ( not found in line${lexer.getLineNumber()}")
        lexer.incrementPosition()
        var arguments: ArgumentsNode? = null
        paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null) {
            arguments = parseArgumentsNode(scope).component1()
            lexer.incrementPosition()
            paren = lexer.getCurrentToken() as? KeyWordToken
        }
        if (paren == null || paren.tok != KeyWord.RPAREN)
            throw ParserError("Expected ) not found in line${lexer.getLineNumber()}")
        return FunctionCallNode(id, arguments) to scope
    }

    private fun parseArgumentsNode(scope: Scope): Pair<ArgumentsNode, Scope> {
        val expressionList = mutableListOf<ExpressionNode>()
        lexer.decrementPosition()
        do {
            lexer.incrementPosition()
            expressionList.add(parseExpressionNode(scope).component1())
            lexer.incrementPosition()
            val comma = lexer.getCurrentToken() as? KeyWordToken
        } while (comma != null && comma.tok == KeyWord.COMMA)
        lexer.decrementPosition()
        return ArgumentsNode(expressionList) to scope
    }

    private fun parseAssignmentNode(scope: Scope): Pair<AssignmentNode, Scope> {
        val (id, _) = parseIdentifierNode(scope)
        lexer.incrementPosition(2)
        val (expression, _) = parseExpressionNode(scope)
        return AssignmentNode(id, expression) to scope
    }

    private fun parseReturnNode(scope: Scope): Pair<ReturnNode, Scope> {
        lexer.incrementPosition()
        return ReturnNode(parseExpressionNode(scope).component1()) to scope
    }

    private fun parseIfNode(scope: Scope): Pair<IfNode, Scope> {
        lexer.incrementPosition()
        var paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN)
            throw ParserError("Expected ( not found in line${lexer.getLineNumber()}")
        lexer.incrementPosition()
        val (expression, _) = parseExpressionNode(scope)
        lexer.incrementPosition()
        paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.RPAREN)
            throw ParserError("Expected ) not found in line${lexer.getLineNumber()}")
        val (blockThen, _) = parseBlockWithBracesNode(scope)

        var blockElse: BlockWithBracesNode? = null
        lexer.tryIncrementPosition()
        if (!lexer.isEndOfProgram()) {
            val nextTok = lexer.getCurrentToken() as? KeyWordToken
            if (nextTok != null && nextTok.tok == KeyWord.ELSE) blockElse = parseBlockWithBracesNode(scope).component1()
            else lexer.decrementPosition()
        }
        else lexer.decrementPosition()
        return IfNode(expression, blockThen, blockElse) to scope
    }

    private fun parseWhileNode(scope: Scope): Pair<WhileNode, Scope> {
        lexer.incrementPosition()
        var paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN)
            throw ParserError("Expected ( not found in line${lexer.getLineNumber()}")
        lexer.incrementPosition()
        val (expression, _) = parseExpressionNode(scope)
        lexer.incrementPosition()
        paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.RPAREN)
            throw ParserError("Expected ) not found in line${lexer.getLineNumber()}")
        val (block, _) = parseBlockWithBracesNode(scope)
        return WhileNode(expression, block) to scope
    }

    private fun parseVariableNode(scope: Scope): Pair<VariableNode, Scope> {
        lexer.incrementPosition()
        val (id, _) = parseIdentifierNode(scope)

        var expression: ExpressionNode? = null
        lexer.tryIncrementPosition()
        if (!lexer.isEndOfProgram()) {
            val nextTok = lexer.getCurrentToken() as? KeyWordToken
            if (nextTok != null && nextTok.tok == KeyWord.ASSIGN) {
                lexer.incrementPosition()
                expression = parseExpressionNode(scope).component1()
            }
            else lexer.decrementPosition()
        }
        val variable = VariableNode(id, expression)
        val newScope = Scope(scope.variables.toMutableMap())
        newScope.add(id, variable)
        return variable to newScope
    }

    private fun parseFunctionNode(scope: Scope): Pair<FunctionNode, Scope> {
        lexer.incrementPosition()
        val (id, _) = parseIdentifierNode(scope)

        lexer.incrementPosition()
        var paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN)
            throw ParserError("Expected ( not found in line${lexer.getLineNumber()}")
        lexer.incrementPosition()

        var parameters: ParameterNamesNode? = null
        var newScope = scope.copy()
        paren = lexer.getCurrentToken() as? KeyWordToken
        if (paren == null) {
            val parsed = parseParametersNode(scope)
            parameters = parsed.component1()
            newScope = parsed.component2()
            lexer.incrementPosition()
            paren = lexer.getCurrentToken() as? KeyWordToken
        }
        if (paren == null || paren.tok != KeyWord.RPAREN)
            throw ParserError("Expected ) not found in line${lexer.getLineNumber()}")
        newScope.add(id)
        val (block, _) = parseBlockWithBracesNode(newScope)
        if (block.block.statementList.last() !is ReturnNode) block.block.statementList.add(ReturnNode(LiteralNode(0)))
        val function = FunctionNode(id, parameters, block)
        newScope = scope.copy()
        newScope.add(id, function)
        return function to newScope
    }

    private fun parseParametersNode(scope: Scope): Pair<ParameterNamesNode, Scope> {
        val parametersList = mutableListOf<IdentifierNode>()
        val newScope = scope.copy()
        do {
            val (id, _) = parseIdentifierNode(scope)
            parametersList.add(id)
            newScope.add(id, VariableNode(id, null))
            lexer.incrementPosition()
            val comma = lexer.getCurrentToken() as? KeyWordToken
        } while (comma != null && comma.tok == KeyWord.COMMA)
        lexer.decrementPosition()
        return ParameterNamesNode(parametersList) to newScope
    }

    private fun parseBlockWithBracesNode(scope: Scope): Pair<BlockWithBracesNode, Scope> {
        lexer.incrementPosition()
        var brace = lexer.getCurrentToken() as? KeyWordToken
        if (brace == null || brace.tok != KeyWord.LBRACE)
            throw ParserError("Expected { not found in line${lexer.getLineNumber()}")
        lexer.incrementPosition()
        val (block, _) = parseBlockNode(scope)
        brace = lexer.getCurrentToken() as? KeyWordToken
        if (brace == null || brace.tok != KeyWord.RBRACE)
            throw ParserError("Expected } not found in line${lexer.getLineNumber()}")
        return BlockWithBracesNode(block) to scope
    }

    private fun parseExpressionNode(scope: Scope, stopToken: KeyWordToken? = null): Pair<ExpressionNode, Scope> {
        val expression = parseBinaryOperationNode(scope, stopToken).component1()
        return expression to scope
    }

    private fun parseBinaryOperationNode(scope: Scope, stopToken: KeyWordToken?): Pair<ExpressionNode, Scope> {
        fun endOfExpression(): Boolean {
            if (lexer.isEndOfProgram()) return true
            val tok = lexer.getCurrentToken() as? KeyWordToken
            return lexer.isEndOfLine() || (stopToken != null && tok == stopToken) || (tok != null &&
                    (tok.tok == KeyWord.RPAREN || tok.tok == KeyWord.RBRACE || tok.tok == KeyWord.COMMA))
        }

        var firstArgument: ExpressionNode = IdentifierNode("fun")
        var nextTok = lexer.getCurrentToken() as? KeyWordToken
        if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
            lexer.incrementPosition()
            firstArgument = parseExpressionNode(scope).component1()
            lexer.incrementPosition()
            val paren = lexer.getCurrentToken() as? KeyWordToken
            if (paren == null || paren.tok != KeyWord.RPAREN)
                throw ParserError("Expected ) not found in line${lexer.getLineNumber()}")
        }

        if (firstArgument == IdentifierNode("fun")) {
            lexer.incrementPosition()
            nextTok = lexer.getCurrentToken() as? KeyWordToken
            lexer.decrementPosition()
            if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
                firstArgument = parseFunctionCallNode(scope).component1()
            }
        }

        if (firstArgument == IdentifierNode("fun")) firstArgument =
                if (lexer.getCurrentToken() is LiteralToken) parseLiteralNode(scope).component1()
                else parseIdentifierNode(scope).component1()

        lexer.tryIncrementPosition()
        if (endOfExpression()) {
            lexer.decrementPosition()
            return firstArgument to scope
        }
        else {
            while (!endOfExpression()) {
                val op = lexer.getCurrentToken() as? KeyWordToken ?:
                        throw ParserError("Expected binary operator not found in line${lexer.getLineNumber()}")

                var step = -1
                while (true) {
                    lexer.incrementPosition()
                    step++
                    nextTok = lexer.getCurrentToken() as? KeyWordToken
                    if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
                        do {
                            step++
                            lexer.incrementPosition()
                            nextTok = lexer.getCurrentToken() as? KeyWordToken
                        } while (nextTok == null || nextTok.tok != KeyWord.RPAREN)
                        lexer.incrementPosition()
                        step++
                    }
                    if (endOfExpression() ||
                            (nextTok != null && isOpToken(nextTok) && getPriority(nextTok.tok) <= getPriority(op.tok)))
                        break
                }

                val stopOp = lexer.getCurrentToken() as KeyWordToken
                lexer.decrementPosition(step)

                val (secondArgument, _) = parseExpressionNode(scope, stopOp)
                if (firstArgument is IdentifierNode && !scope.contains(firstArgument))
                    throw ParserError("Not found declaration ${firstArgument.name} in line${lexer.getLineNumber()}")
                if (secondArgument is IdentifierNode && !scope.contains(secondArgument))
                    throw ParserError("Not found declaration ${secondArgument.name} in line${lexer.getLineNumber()}")
                firstArgument = BinaryExpressionNode(firstArgument, secondArgument, op.tok)
                lexer.incrementPosition()
            }
            lexer.decrementPosition()
            return firstArgument to scope
        }

    }

    private fun parseLiteralNode(scope: Scope): Pair<LiteralNode, Scope> {
        val value = lexer.getCurrentToken() as? LiteralToken ?:
                throw ParserError("Unknown symbol in line${lexer.getLineNumber()}")
        return LiteralNode(value.int) to scope
    }

    private fun parseIdentifierNode(scope: Scope): Pair<IdentifierNode, Scope> {
        val id = lexer.getCurrentToken() as? IdToken ?:
                throw ParserError("Unknown symbol in line${lexer.getLineNumber()}")
        return IdentifierNode(id.string) to scope
    }

    private fun getPriority(op: KeyWord) = when(op) {
        KeyWord.OR -> 4
        KeyWord.AND -> 5
        KeyWord.EQ, KeyWord.NEQ -> 9
        KeyWord.GT, KeyWord.GE, KeyWord.LT, KeyWord.LE -> 10
        KeyWord.ADD, KeyWord.SUB -> 12
        KeyWord.MUL, KeyWord.DIV, KeyWord.MOD -> 13
        else -> 0
    }

    private fun isOpToken(op: KeyWordToken) = when(op.tok) {
        KeyWord.OR, KeyWord.AND, KeyWord.EQ, KeyWord.NEQ, KeyWord.GT, KeyWord.GE, KeyWord.LT, KeyWord.LE,
        KeyWord.ADD, KeyWord.SUB, KeyWord.MUL, KeyWord.DIV, KeyWord.MOD -> true
        else -> false
    }
}

open class ParserError(message: String? = null) : Throwable()
