module Benchmark.Runner.Node exposing (Benchmark, BenchmarkProgram, run)

import Benchmark
import Benchmark.LowLevel
import Benchmark.Reporting as Reporting
import Benchmark.Status as Status
import Console
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Platform exposing (worker)
import Process
import Task exposing (Task)
import Trend.Linear as Trend exposing (Trend)
import Trend.Math


fromLowLevel : Benchmark.Benchmark -> Benchmark
fromLowLevel lowlevel =
    case Reporting.fromBenchmark lowlevel of
        Reporting.Single name _ ->
            Wrapped name lowlevel

        Reporting.Series name _ ->
            Wrapped name lowlevel

        Reporting.Group name _ ->
            Wrapped name lowlevel


type Benchmark
    = Compare Benchmark.Benchmark (List Benchmark.Benchmark)
    | Group String (List Benchmark)
    | Series String (Dict String Benchmark)
    | Wrapped String Benchmark.Benchmark


isDone : Benchmark -> Bool
isDone benchmark =
    case benchmark of
        Wrapped _ lowlevel ->
            Benchmark.done lowlevel

        Compare base other ->
            Benchmark.done base && List.all Benchmark.done other

        Group _ other ->
            List.all isDone other

        Series _ samples ->
            samples
                |> Dict.values
                |> List.all isDone


traverse :
    (a -> Task e a)
    -> List a
    -> Task e (List a)
traverse toTask list =
    list
        |> List.map toTask
        |> Task.sequence


step : Benchmark -> Task Never Benchmark
step structure =
    case structure of
        Wrapped name bench ->
            Task.map (Wrapped name) (Benchmark.step bench)

        Compare baseline cases ->
            case ( Benchmark.step baseline, traverse Benchmark.step cases ) of
                ( left, right ) ->
                    Task.map2 Compare left right

        Group name entries ->
            Task.map (Group name) (traverse step entries)

        Series name entries ->
            let
                tasks =
                    Dict.toList entries
                        |> List.map
                            (\( key, entry ) ->
                                entry
                                    |> step
                                    |> Task.map (Tuple.pair key)
                            )
            in
            case tasks of
                [] ->
                    Task.succeed (Series name Dict.empty)

                _ ->
                    List.foldr
                        (Task.map2
                            (\( key, value ) dict ->
                                Dict.insert key value dict
                            )
                        )
                        (Task.succeed entries)
                        tasks
                        |> Task.map (Series name)


type alias Model =
    { benchmarks : Benchmark
    , emit : Value -> Cmd Msg
    }


type alias BenchmarkProgram =
    Program () Model Msg


type Msg
    = Update Benchmark


init : (Value -> Cmd Msg) -> Benchmark -> ( Model, Cmd Msg )
init emit benchmarks =
    ( { emit = emit
      , benchmarks = benchmarks
      }
    , Cmd.batch
        [ if isDone benchmarks then
            Cmd.none

          else
            stepCmd benchmarks
        , emit <| running benchmarks
        , emit <| start benchmarks
        ]
    )


breakForRender : Task x a -> Task x a
breakForRender task =
    Process.sleep 0 |> Task.andThen (always task)


stepCmd : Benchmark -> Cmd Msg
stepCmd benchmark =
    step benchmark
        |> breakForRender
        |> Task.perform Update


update : Msg -> Model -> ( Model, Cmd Msg )
update (Update benchmark) ({ emit } as model) =
    if isDone benchmark then
        ( { model | benchmarks = benchmark }, Cmd.batch [ emit <| done benchmark ] )

    else
        ( { model | benchmarks = benchmark }, Cmd.batch [ stepCmd benchmark, emit <| running benchmark ] )


type Progress
    = Sizing
    | InProgress ProgressStats
    | Invalid


type alias ProgressStats =
    { progress : Float, errors : Int }


combine :
    Progress
    -> Progress
    -> Progress
combine left right =
    case ( left, right ) of
        ( InProgress a, InProgress b ) ->
            InProgress
                { progress = max a.progress b.progress
                , errors = a.errors + b.errors
                }

        ( Invalid, _ ) ->
            Invalid

        ( _, Invalid ) ->
            Invalid

        _ ->
            Sizing


getNameLowlevel : Benchmark.Benchmark -> String
getNameLowlevel lowlevel =
    case Reporting.fromBenchmark lowlevel of
        Reporting.Single name _ ->
            name

        _ ->
            Debug.todo "should not happen?"


getProgressLowlevel : Benchmark.Benchmark -> Status.Status
getProgressLowlevel lowlevel =
    case Reporting.fromBenchmark lowlevel of
        Reporting.Single _ status ->
            status

        Reporting.Series _ namedStatuses ->
            List.head namedStatuses
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Status.Cold

        Reporting.Group _ _ ->
            Debug.todo "should not happen?"


getProgress : Benchmark -> Progress
getProgress structure =
    case structure of
        Wrapped _ bench ->
            getProgressLowlevel bench
                |> statusToStats

        Compare baseLine cases ->
            List.foldr combine
                (getProgressLowlevel baseLine |> statusToStats)
                (List.map (getProgressLowlevel >> statusToStats) cases)

        _ ->
            Debug.todo "not implemented"



{-
   Compare baseLine cases ->
       List.foldr combine
           (LowLevel.status baseLine |> statusToStats)
           (List.map (LowLevel.status >> statusToStats) cases)

   Group _ reports ->
       List.foldr
           (\report acc_ ->
               case acc_ of
                   Nothing ->
                       Just <| getProgress report

                   Just acc ->
                       Just <| combine acc (getProgress report)
           )
           Nothing
           reports
           |> Maybe.withDefault Invalid

   Series name entries ->
       Dict.foldl
           (\_ report acc_ ->
               case acc_ of
                   Nothing ->
                       Just <| getProgress report

                   Just acc ->
                       Just <| combine acc (getProgress report)
           )
           Nothing
           entries
           |> Maybe.withDefault Invalid
-}


statusToStats : Status.Status -> Progress
statusToStats status =
    case status of
        Status.Cold ->
            Sizing

        Status.Unsized ->
            Sizing

        Status.Pending total samples ->
            InProgress { progress = Status.progress status, errors = 0 }

        Status.Failure _ ->
            InProgress { progress = Status.progress status, errors = 1 }

        Status.Success _ _ ->
            InProgress { progress = Status.progress status, errors = 0 }


running : Benchmark -> Value
running report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "running" )
        , ( "data"
          , Json.Encode.string <| "\u{000D}" ++ progressBar 72 (getProgress report)
          )
        ]


progressBar : Int -> Progress -> String
progressBar width progress =
    case progress of
        Sizing ->
            "Sizing..."

        Invalid ->
            "Invalid benchmark structure."

        InProgress inProgress ->
            let
                {-
                   doneWhen : Int
                   doneWhen =
                       (current * 8 * toFloat width)
                           / total
                           |> floor

                   toGo : Int
                   toGo =
                       width - ceiling (toFloat doneWhen / 8)

                   percentDone : Int
                   percentDone =
                       (current * toFloat 100)
                           / total
                           |> floor
                -}
                doneWhen =
                    inProgress.progress
                        * 8
                        * toFloat width
                        |> floor

                toGo =
                    width - ceiling (toFloat doneWhen / 8)

                percentDone : Int
                percentDone =
                    (inProgress.progress * toFloat 100)
                        |> floor

                errors =
                    inProgress.errors

                error : String
                error =
                    if errors > 0 then
                        " " ++ Debug.toString errors ++ " errors"

                    else
                        ""
            in
            [ "▕"
            , String.repeat (doneWhen // 8) "█"
            , block (doneWhen |> modBy 8)
            , String.repeat toGo "·"
            , "▏"
            , String.padLeft 4 ' ' (Debug.toString percentDone ++ "%")
            , error
            ]
                |> String.concat


block : Int -> String
block i =
    case i of
        1 ->
            "▏"

        2 ->
            "▎"

        3 ->
            "▍"

        4 ->
            "▌"

        5 ->
            "▋"

        6 ->
            "▊"

        7 ->
            "▉"

        8 ->
            "█"

        _ ->
            ""


done : Benchmark -> Value
done report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "done" )
        , ( "msg", Json.Encode.string <| "\u{000D}" ++ progressBar 72 (getProgress report) )
        , ( "data", encode report |> Maybe.withDefault (Json.Encode.object []) )
        ]


start : Benchmark -> Value
start report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "start" )
        , ( "data"
          , Json.Encode.string <|
                Console.bold "\n⏱  Running benchmarks...\n\n"
                    ++ makePrettyIntro report
                    ++ "\n"
          )
        ]


indent : Int -> String -> String
indent level =
    (++) (String.repeat level "    ")


makePrettyIntro : Benchmark -> String
makePrettyIntro =
    makePrettyIntroLines >> String.join "\n"


makePrettyIntroLines : Benchmark -> List String
makePrettyIntroLines structure =
    case structure of
        Wrapped name benchmark ->
            [ name ]

        Compare baseline cases ->
            "Compare:"
                :: (("→ " ++ getNameLowlevel baseline) |> indent 1)
                :: List.map (getNameLowlevel >> (++) "↝ " >> indent 1) cases

        Group thisGroup reports ->
            ("↳ " ++ thisGroup)
                :: List.concatMap
                    (makePrettyIntroLines >> List.map (indent 1))
                    reports

        Series name variations ->
            ("Series - " ++ name)
                :: (Dict.toList variations
                        |> List.concatMap
                            (\( subname, variation ) ->
                                ("Variation: " ++ subname)
                                    :: (variation |> makePrettyIntroLines |> List.map (indent 1))
                            )
                        |> List.map (indent 1)
                   )


encode : Benchmark -> Maybe Value
encode benchmark =
    case benchmark of
        Wrapped _ bench ->
            encodeBench bench

        Compare baseline cases ->
            Maybe.map
                (\baseObject ->
                    [ ( "baseline", baseObject )
                    , ( "cases", Json.Encode.list identity (List.filterMap encodeBench cases) )
                    ]
                        |> Json.Encode.object
                )
                (encodeBench baseline)

        Group name members ->
            case List.filterMap encode members of
                [] ->
                    Nothing

                encodedMembers ->
                    [ ( "name", Json.Encode.string name )
                    , ( "entries", Json.Encode.list identity encodedMembers )
                    ]
                        |> Json.Encode.object
                        |> Just

        Series name variations ->
            case List.filterMap (mapTupleToMaybe encode) (Dict.toList variations) of
                [] ->
                    Nothing

                encodedEntries ->
                    [ ( "name", Json.Encode.string name )
                    , ( "variations", Json.Encode.object encodedEntries )
                    ]
                        |> Json.Encode.object
                        |> Just


mapTupleToMaybe : (b -> Maybe c) -> ( a, b ) -> Maybe ( a, c )
mapTupleToMaybe toMaybe ( a, b ) =
    toMaybe b |> Maybe.map (Tuple.pair a)


encodeBench : Benchmark.Benchmark -> Maybe Value
encodeBench bench =
    Reporting.fromBenchmark bench
        |> encodeReport
        |> Just


encodeReport : Reporting.Report -> Value
encodeReport report =
    case report of
        Reporting.Single name status ->
            encodeNameStatus name status

        Reporting.Series name items ->
            Json.Encode.object
                [ ( "name", Json.Encode.string name )
                , ( "series", Json.Encode.list (\( subname, status ) -> encodeNameStatus subname status) items )
                ]

        Reporting.Group name reports ->
            Json.Encode.object
                [ ( "name", Json.Encode.string name )
                , ( "group", Json.Encode.list encodeReport reports )
                ]


encodeNameStatus : String -> Status.Status -> Value
encodeNameStatus name status =
    Json.Encode.object (( "name", Json.Encode.string name ) :: encodeStatus status)


encodeStatus : Status.Status -> List ( String, Value )
encodeStatus status =
    case status of
        Status.Cold ->
            []

        Status.Unsized ->
            []

        Status.Pending _ _ ->
            []

        Status.Failure e ->
            [ ( "error", encodeError e ) ]

        Status.Success _ trend ->
            [ ( "runsPerSecond", Json.Encode.int (runsPerSecond trend) )
            , ( "goodnessOfFit", Json.Encode.float (Trend.goodnessOfFit trend) )
            ]


encodeError : Status.Error -> Value
encodeError e =
    Json.Encode.string <|
        case e of
            Status.MeasurementError Benchmark.LowLevel.StackOverflow ->
                "The benchmark caused a stack overflow"

            Status.MeasurementError (Benchmark.LowLevel.UnknownError msg) ->
                "Ran into an unknown error: " ++ msg

            Status.AnalysisError (Trend.Math.NeedMoreValues n) ->
                "Not enough values, I need at least " ++ String.fromInt n

            Status.AnalysisError Trend.Math.AllZeros ->
                "The benchmark results are all zero! I can't make a trend out of those."


runsPerSecond : Trend Trend.Quick -> Int
runsPerSecond trend =
    -- runs per second (= 1000ms)
    trend
        |> Trend.line
        |> (\a -> Trend.predictX a 1000)
        |> floor


run : (Value -> Cmd Msg) -> Benchmark.Benchmark -> BenchmarkProgram
run emit benchmarks =
    worker
        { init = \() -> init emit (fromLowLevel benchmarks)
        , update = update
        , subscriptions = always Sub.none
        }
