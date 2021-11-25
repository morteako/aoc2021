package aoc2021.app.days.day1

fun solve20() {
    val lim = 36000000
    val houses = Array<Int>(lim+1, {0})
    for (elf in 1..lim) {
        for(e in (elf .. lim step elf).take(50)) {
            houses[e] += elf*11
        }
        if(houses[elf] >= lim) {
            println(elf)
            break
        }
    }
}

fun solve20_a() {
    val lim = 36000000
    val houses = IntArray(lim+1)
    for (elf in 1..lim) {
        for(e in elf .. lim step elf) {
            houses[e] += elf*11
        }
        if(houses[elf] >= lim) {
            println(elf)
            break
        }
    }
}
