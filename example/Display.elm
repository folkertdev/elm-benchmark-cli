port module Display exposing (main)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode


main : Program Decode.Value () msg
main =
    Platform.programWithFlags
        { init = init
        , update = \_ _ -> () ! []
        , subscriptions = always Sub.none
        }


init : Decode.Value -> ( (), Cmd msg )
init flags =
    case Decode.decodeValue decodeBenches flags of
        Err error ->
            error
                |> Json.Encode.string
                |> emit
                |> (,) ()

        Ok benches ->
            benches
                |> List.foldr
                    (\bench acc ->
                        Dict.update
                            bench.kind
                            (\existing ->
                                Maybe.map ((::) bench) existing
                                    |> Maybe.withDefault [ bench ]
                                    |> Just
                            )
                            acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map
                    (\( kind, benches ) ->
                        benches
                            |> List.sortBy .inputSize
                            |> List.map
                                (\{ samples, sampleSize } ->
                                    toFloat (List.length samples * sampleSize)
                                        / toFloat (List.sum samples)
                                        |> toString
                                )
                            |> String.join "\t"
                            |> (++) "\t"
                            |> (++) kind
                    )
                |> String.join "\n"
                |> Json.Encode.string
                |> emit
                |> (,) ()


port emit : Json.Encode.Value -> Cmd msg


decodeBenches : Decoder (List Bench)
decodeBenches =
    Decode.keyValuePairs Decode.value
        |> Decode.andThen
            (\kvs ->
                let
                    combined =
                        List.foldr
                            (\( k, v ) acc ->
                                Result.map2 (::)
                                    (String.toInt k
                                        |> Result.andThen
                                            (\key ->
                                                Decode.decodeValue (benchDecoder key) v
                                            )
                                    )
                                    acc
                            )
                            (Ok [])
                            kvs
                in
                case combined of
                    Err err ->
                        Decode.fail err

                    Ok v ->
                        Decode.succeed v
            )


type alias Bench =
    { kind : String
    , inputSize : Int
    , sampleSize : Int
    , samples : List Int
    }


benchDecoder : Int -> Decoder Bench
benchDecoder inputSize =
    Decode.map4 Bench
        (Decode.field "name" Decode.string)
        (Decode.succeed inputSize)
        (Decode.field "sampleSize" Decode.int)
        (Decode.field "samples" (Decode.list Decode.int))
