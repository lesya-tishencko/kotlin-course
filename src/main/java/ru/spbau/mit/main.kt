package ru.spbau.mit

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.Reader
import java.util.*

class parser(val content: String) {
    private var position = 0

    private fun readTag(tag: String) = when {
        content.length <= position -> false
        content.substring(position).startsWith(tag) -> {
            position += tag.length
            true
        }
        else -> false
    }

    fun openTable() = readTag("<table>")
    fun closeTable() = readTag("</table>")
    fun openRow() = readTag("<tr>")
    fun closeRow() = readTag("</tr>")
    fun openCell() = readTag("<td>")
    fun closeCell() = readTag("</td>")
}

class reader(val parser: parser) {
    private val answers = mutableListOf<Int>()

    fun readTable(): Unit {
        var sum = 0
        parser.openTable()
        while (!parser.closeTable()) {
            sum += readRow()
        }
        answers.add(sum)
    }

    fun getAnswer(): String {
        answers.sort()
        return answers.joinToString(separator = " ")
    }

    private fun  readRow(): Int {
        var sum = 0
        parser.openRow()
        while (!parser.closeRow()) {
            sum += readCell()
        }
        return sum
    }

    private fun  readCell(): Int {
        parser.openCell()
        if (!parser.closeCell()) {
            readTable()
        }
        parser.closeCell()
        return 1
    }
}

fun main(args: Array<String>) {
    var input = StringBuilder()
    val scanner = InputStreamReader(System.`in`)
    scanner.forEachLine { input.append(it) }
    val parser = parser(input.toString())
    val reader = reader(parser)
    reader.readTable()
    print(reader.getAnswer())
}
