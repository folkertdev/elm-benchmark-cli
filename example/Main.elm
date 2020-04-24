port module Main exposing (main)

import Benchmark
import Benchmark.Runner.Node exposing (BenchmarkProgram, run)
import Json.Encode exposing (Value)


main : BenchmarkProgram
main =
    let
        reverse =
            Benchmark.benchmark "String.reverse" (\_ -> String.reverse "foobarbaz")

        id =
            Benchmark.benchmark "identity" (\_ -> identity "foobarbaz")

        three =
            Benchmark.compare "foo"
                "String.reverse"
                (\_ -> String.reverse "foobarbaz")
                "identity"
                (\_ -> identity "foobarbaz")
    in
    run emit three


port emit : Value -> Cmd msg
