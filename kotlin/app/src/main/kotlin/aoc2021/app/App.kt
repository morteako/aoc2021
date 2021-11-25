package aoc2021.app

import aoc2021.app.days.day1.solve
import aoc2021.app.days.day1.solve20
import java.io.File
import kotlin.system.measureTimeMillis

val solutions = mapOf(
    1 to ::solve,
    2 to ::solve20
)

fun main(args: Array<String>) {
    val day = args.firstOrNull()?.toIntOrNull() ?: 20
    println("Running day : $day")
//    val input = File("../inputs/$day").readText()
//    solutions[day]
    println(measureTimeMillis { solve20() })
}
