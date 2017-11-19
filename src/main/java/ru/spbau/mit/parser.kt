package ru.spbau.mit

class Parser(val tokenList: List<List<Token>>) {
    var lineNumber = 0;
    var positionInLine = 0;

    fun parseBlockNode(scope: Scope): Pair<BlockNode, Scope> {
        val statements: MutableList<StatementNode> = mutableListOf()
        var newScope = scope.copy()
        do {
            val (expr, nextScope) = parseStatementNode(newScope)
            statements.add(expr)
            newScope = nextScope
            tryIncrementPosition()
            if (tokenList.size == lineNumber) break
            val nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
        } while (nextTok == null || nextTok.tok != KeyWord.RBRACE)
        return BlockNode(statements, newScope) to newScope
    }

    private fun tryIncrementPosition() {
        if (tokenList[lineNumber].size == positionInLine + 1) {
            lineNumber++
            while(lineNumber < tokenList.size && tokenList[lineNumber].isEmpty()) lineNumber++
            positionInLine = 0
        } else {
            positionInLine++
        }
    }

    private fun parseStatementNode(scope: Scope): Pair<StatementNode, Scope> = when(tokenList[lineNumber][positionInLine]) {
        KeyWordToken(KeyWord.FUN) -> parseFunctionNode(scope)
        KeyWordToken(KeyWord.VAR) -> parseVariableNode(scope)
        KeyWordToken(KeyWord.WHILE) -> parseWhileNode(scope)
        KeyWordToken(KeyWord.IF) -> parseIfNode(scope)
        KeyWordToken(KeyWord.RETURN) -> parseReturnNode(scope)
        else -> if (tokenList[lineNumber][positionInLine + 1] == KeyWordToken(KeyWord.ASSIGN)) parseAssigmentNode(scope)
                else parseExpressionNode(scope)
    }

    private fun parseFunctionCallNode(scope: Scope): Pair<FunctionCallNode, Scope> {
        val (id, _) = parseIdentifierNode(scope)
        if (id.name != "println" && !scope.contains(id)) throw ParserError("Not found declaration ${id.name} in line$lineNumber")
        incrementPosition()
        var paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN) throw ParserError("Expected ( not found in line$lineNumber")
        incrementPosition()
        var arguments: ArgumentsNode? = null
        paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null) {
            arguments = parseArgumentsNode(scope).component1()
            incrementPosition()
            paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        }
        if (paren == null || paren.tok != KeyWord.RPAREN) throw ParserError("Expected ) not found in line$lineNumber")
        return FunctionCallNode(id, arguments) to scope
    }

    private fun incrementPosition(count: Int = 1) {
        for (i in 1..count) {
            tryIncrementPosition()
            if (lineNumber == tokenList.size) throw ParserError("Unexpected end of file in line$lineNumber")
        }
    }

    private fun parseArgumentsNode(scope: Scope): Pair<ArgumentsNode, Scope> {
        val expressionList = mutableListOf<ExpressionNode>()
        decrementPosition()
        do {
            incrementPosition()
            expressionList.add(parseExpressionNode(scope).component1())
            incrementPosition()
            val comma = tokenList[lineNumber][positionInLine] as? KeyWordToken
        } while (comma != null && comma.tok == KeyWord.COMMA)
        decrementPosition()
        return ArgumentsNode(expressionList) to scope
    }

    private fun decrementPosition(count: Int = 1) {
        for (i in 1..count) {
            if (positionInLine == 0) {
                lineNumber--
                while (tokenList[lineNumber].isEmpty()) lineNumber--
                positionInLine = tokenList[lineNumber].size - 1
            } else {
                positionInLine--
            }
        }
    }

    private fun parseAssigmentNode(scope: Scope): Pair<AssignmentNode, Scope> {
        val (id, _) = parseIdentifierNode(scope)
        incrementPosition(2)
        val (expression, _) = parseExpressionNode(scope)
        return AssignmentNode(id, expression) to scope
    }

    private fun parseReturnNode(scope: Scope): Pair<ReturnNode, Scope> {
        incrementPosition()
        return ReturnNode(parseExpressionNode(scope).component1()) to scope
    }

    private fun parseIfNode(scope: Scope): Pair<IfNode, Scope> {
        incrementPosition()
        var paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN) throw ParserError("Expected ( not found in line$lineNumber")
        incrementPosition()
        val (expression, _) = parseExpressionNode(scope)
        incrementPosition()
        paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.RPAREN) throw ParserError("Expected ) not found in line$lineNumber")
        val (blockThen, _) = parseBlockWithBracesNode(scope)

        var blockElse: BlockWithBracesNode? = null
        tryIncrementPosition()
        if (lineNumber != tokenList.size) {
            val nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
            if (nextTok != null && nextTok.tok == KeyWord.ELSE) {
                blockElse = parseBlockWithBracesNode(scope).component1()
            } else {
                decrementPosition()
            }
        }
        return IfNode(expression, blockThen, blockElse) to scope
    }

    private fun parseWhileNode(scope: Scope): Pair<WhileNode, Scope> {
        incrementPosition()
        var paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN) throw ParserError("Expected ( not found in line$lineNumber")
        incrementPosition()
        val (expression, _) = parseExpressionNode(scope)
        incrementPosition()
        paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.RPAREN) throw ParserError("Expected ) not found in line$lineNumber")
        val (block, _) = parseBlockWithBracesNode(scope)
        return WhileNode(expression, block) to scope
    }

    private fun parseVariableNode(scope: Scope): Pair<VariableNode, Scope> {
        incrementPosition()
        val (id, _) = parseIdentifierNode(scope)

        var expression: ExpressionNode? = null
        tryIncrementPosition()
        if (lineNumber != tokenList.size) {
            val nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
            if (nextTok != null && nextTok.tok == KeyWord.ASSIGN) {
                incrementPosition()
                expression = parseExpressionNode(scope).component1()
            } else {
                decrementPosition()
            }
        }
        val variable = VariableNode(id, expression)
        val newScope = Scope(scope.variables.toMutableMap())
        newScope.add(id, variable)
        return variable to newScope
    }

    private fun parseFunctionNode(scope: Scope): Pair<FunctionNode, Scope> {
        incrementPosition()
        val (id, _) = parseIdentifierNode(scope)

        incrementPosition()
        var paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null || paren.tok != KeyWord.LPAREN) throw ParserError("Expected ( not found in line$lineNumber")
        incrementPosition()

        var parameters: ParameterNamesNode? = null
        var newScope = scope.copy()
        paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (paren == null) {
            val parsed = parseParametersNode(scope)
            parameters = parsed.component1()
            newScope = parsed.component2()
            incrementPosition()
            paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
        }
        if (paren == null || paren.tok != KeyWord.RPAREN) throw ParserError("Expected ) not found in line$lineNumber")
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
            incrementPosition()
            val comma = tokenList[lineNumber][positionInLine] as? KeyWordToken
        } while (comma != null && comma.tok == KeyWord.COMMA)
        decrementPosition()
        return ParameterNamesNode(parametersList) to newScope
    }

    private fun parseBlockWithBracesNode(scope: Scope): Pair<BlockWithBracesNode, Scope> {
        incrementPosition()
        var brace = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (brace == null || brace.tok != KeyWord.LBRACE) throw ParserError("Expected { not found in line$lineNumber")
        incrementPosition()
        val (block, _) = parseBlockNode(scope)
        brace = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (brace == null || brace.tok != KeyWord.RBRACE) throw ParserError("Expected } not found in line$lineNumber")
        return BlockWithBracesNode(block) to scope
    }

    private fun parseExpressionNode(scope: Scope, stopToken: KeyWordToken? = null): Pair<ExpressionNode, Scope> {
        val expression = parseBinaryOperationNode(scope, stopToken).component1()
        return expression to scope;
    }

    private fun parseBinaryOperationNode(scope: Scope, stopToken: KeyWordToken?): Pair<ExpressionNode, Scope> {
        fun endOfExpression(): Boolean {
            if (lineNumber == tokenList.size) return true
            val tok = tokenList[lineNumber][positionInLine] as? KeyWordToken
            return positionInLine == 0 || (stopToken != null && tok == stopToken) || (tok != null &&
                    (tok.tok == KeyWord.RPAREN || tok.tok == KeyWord.RBRACE || tok.tok == KeyWord.COMMA))
        }

        var arg1: ExpressionNode = IdentifierNode("fun");
        var nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
        if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
            incrementPosition()
            arg1 = parseExpressionNode(scope).component1()
            incrementPosition()
            var paren = tokenList[lineNumber][positionInLine] as? KeyWordToken
            if (paren == null || paren.tok != KeyWord.RPAREN) throw ParserError("Expected ) not found in line$lineNumber")
        }

        if (arg1 == IdentifierNode("fun")) {
            incrementPosition()
            nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
            decrementPosition()
            if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
                arg1 = parseFunctionCallNode(scope).component1()
            }
        }

        if (arg1 == IdentifierNode("fun")) {
            if (tokenList[lineNumber][positionInLine] is LiteralToken)
                arg1 = parseLiteralNode(scope).component1()
            else
                arg1 = parseIdentifierNode(scope).component1()
        }

        tryIncrementPosition()
        if (endOfExpression()) {
            decrementPosition()
            return arg1 to scope
        }

        while (!endOfExpression()) {
            val op = tokenList[lineNumber][positionInLine] as KeyWordToken

            var step = -1
            while (true) {
                incrementPosition()
                step++
                var nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
                if (nextTok != null && nextTok.tok == KeyWord.LPAREN) {
                    do {
                        step++
                        incrementPosition()
                        nextTok = tokenList[lineNumber][positionInLine] as? KeyWordToken
                    } while (nextTok == null || nextTok.tok != KeyWord.RPAREN)
                    incrementPosition()
                    step++
                }
                if (endOfExpression() || (nextTok != null && isOpToken(nextTok) && getPriority(nextTok.tok) <= getPriority(op.tok)))
                    break
            }

            val stopOp = tokenList[lineNumber][positionInLine] as KeyWordToken
            decrementPosition(step)

            val (arg2, _) = parseExpressionNode(scope, stopOp)
            if (arg1 is IdentifierNode && !scope.contains(arg1)) throw ParserError("Not found declaration ${arg1.name} in line$lineNumber")
            if (arg2 is IdentifierNode && !scope.contains(arg2)) throw ParserError("Not found declaration ${arg2.name} in line$lineNumber")
            arg1 = BinaryExpressionNode(arg1, arg2, op.tok)
            incrementPosition()
        }
        decrementPosition()
        return arg1 to scope;
    }

    private fun parseLiteralNode(scope: Scope): Pair<LiteralNode, Scope> {
        val value = tokenList[lineNumber][positionInLine]
        if (value !is LiteralToken) throw ParserError("Unknown symbol in line$lineNumber")
        return LiteralNode((value as LiteralToken).int) to scope
    }

    private fun parseIdentifierNode(scope: Scope): Pair<IdentifierNode, Scope> {
        val id = tokenList[lineNumber][positionInLine] as? IdToken ?: throw ParserError("Unknown symbol in line$lineNumber")
        return IdentifierNode((id as IdToken).string) to scope
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
