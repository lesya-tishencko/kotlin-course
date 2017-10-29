package ru.spbau.mit
import kotlin.test.assertEquals
import org.junit.Test

class TestSource {
    @Test
    fun testFirstExample() {
        val parser = parser("<table><tr><td></td></tr></table>")
        val reader = reader(parser)
        reader.readTable()
        assertEquals("1", reader.getAnswer())
    }

    @Test
    fun testSecondExample() {
        val parser = parser(
                "<table>" +
                "<tr>" +
                "<td>" +
                "<table><tr><td></td></tr><tr><td><" +
                "/td" +
                "></tr><tr><td></td></tr><tr><td></td></tr></table>" +
                "</td>" +
                "</tr>" +
                "</table>"
        )
        val reader = reader(parser)
        reader.readTable()
        assertEquals("1 4", reader.getAnswer())
    }

    @Test
    fun testThirdExample() {
        val parser = parser(
                "<table><tr><td>" +
                "<table><tr><td>" +
                "<table><tr><td>" +
                "<table><tr><td></td><td></td>" +
                "</tr><tr><td></td></tr></table>" +
                "</td></tr></table>" +
                "</td></tr></table>" +
                "</td></tr></table>"
        )
        val reader = reader(parser)
        reader.readTable()
        assertEquals("1 1 1 3", reader.getAnswer())
    }
}
