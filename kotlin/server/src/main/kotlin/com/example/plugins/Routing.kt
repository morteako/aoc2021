package com.example.plugins

import io.ktor.routing.*
import io.ktor.http.*
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.request.*
import kotlin.system.measureTimeMillis

import aoc2021.app.Testy

fun Application.configureRouting() {

    // Starting point for a Ktor app:
    routing {
        get("/{day}") {
            val day = call.parameters["day"]
                ?.toIntOrNull()
            day?.let {
                val res = measureDay(day)
                call.respondText("$res")
            } ?: call.respondText(status = HttpStatusCode.NotAcceptable, provider = {"Invalid day"})
        }
    }
}

fun measureDay(day:Int):Long {
    Testy.test()
    return day * measureTimeMillis {
        println(day)
    }
}