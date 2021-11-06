#!/bin/bash
case "$1" in
    r|R|"")
    ghcid --command="stack repl aoc2021:lib :app" -r
    ;;
    t|T)
    case "$2" in
        "")
        ghcid -c="stack ghci aoc2021:test aoc2021:lib" -T=':main'
        ;;
        *)
        ghcid -c="stack ghci aoc2021:test aoc2021:lib" -T=':main --match "/Day'$2'/"'
        ;;
    esac
    ;;
    *)
    args=${@:1}
    ghcid --command='stack repl :app' -T=":main $args"
    ;;

esac