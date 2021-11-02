package aoc2021.app

import aoc2021.app.days.day1.solve
import java.io.File

val solutions = mapOf(
    1 to ::solve
)

fun main(args: Array<String>) {
    val day = args.firstOrNull()?.toIntOrNull() ?: 1
    println("Running day : $day")
    val input = File("../inputs/$day").readText()
    solutions[day]?.let{ it(input)} ?: throw Error("Missing day $day")
}