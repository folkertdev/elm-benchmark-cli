port module Main exposing (main)

import Array
import Benchmark
import Benchmark.Runner.Node exposing (BenchmarkProgram, run)
import Json.Encode exposing (Value)


main : BenchmarkProgram
main =
    let
        n =
            1000

        list =
            List.range 0 (n - 1)

        array =
            Array.initialize n identity

        benchmark =
            Benchmark.describe "folds"
                [ Benchmark.compare "foldl"
                    "List.foldl"
                    (\_ -> List.foldl (+) 0 list)
                    "Array.foldl"
                    (\_ -> Array.foldl (+) 0 array)
                , Benchmark.compare "foldr"
                    "List.foldr"
                    (\_ -> List.foldr (+) 0 list)
                    "Array.foldr"
                    (\_ -> Array.foldr (+) 0 array)
                ]
    in
    run emit benchmark


port emit : Value -> Cmd msg
