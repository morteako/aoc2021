package aoc2021.app.days.day1

fun solve(input:String) {
    val nums = input
        .trim()
        .lines()
        .map{it.toInt()}

    val target = 2020
    val m = nums.associateBy { target - it }
    val res = nums.firstNotNullOf { n -> m[n]?.let {
            n to it
        }
    }
    println(res.first * res.second)

    nums.firstNotNullOf { n ->
        val m = nums.associateBy { target - it - n }
        val res2 = nums.firstNotNullOfOrNull { n ->
            m[n]?.let {
                n to it
            }
        }
        res2?.let { (a,b) ->
            a*b*n
        }
    }.also {
        println(it)
    }

}